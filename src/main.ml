(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let name = Filename.basename Sys.argv.(0)
let project_url = "http://github.com/mirage/xen-disk"

open Lwt
open Blkback
open Gnt
open Xs_protocol
module Client = Xs_client.Client(Xs_transport_lwt_unix_client)
open Client
open Backend
open Storage

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
  }
  (** options common to all subcommands *)

  let make verbose debug = { verbose; debug }
end

let ( >>= ) = Blkproto.( >>= )

let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()


let get_my_domid client =
  with_xs client (fun xs ->
    try_lwt
      lwt domid = read xs "domid" in
      return (int_of_string domid)
    with Xs_protocol.Enoent _ -> return 0)

let mk_backend_path client (domid,devid) =
  lwt self = get_my_domid client in
  return (Printf.sprintf "/local/domain/%d/backend/%s/%d/%d" self name domid devid)

let mk_frontend_path client (domid,devid) =
  return (Printf.sprintf "/local/domain/%d/device/vbd/%d" domid devid)

let writev client pairs =
  with_xst client (fun xs ->
    Lwt_list.iter_s (fun (k, v) -> write xs k v) pairs
  )

let readv client path keys =
  lwt options = with_xs client (fun xs ->
    Lwt_list.map_s (fun k ->
      try_lwt
        lwt v = read xs (path ^ "/" ^ k) in
        return (Some (k, v))
      with _ -> return None) keys
  ) in
  return (List.fold_left (fun acc x -> match x with None -> acc | Some y -> y :: acc) [] options)

let read_one client k = with_xs client (fun xs ->
  try_lwt
    lwt v = read xs k in
    return (`OK v)
  with _ -> return (`Error ("failed to read: " ^ k)))

let write_one client k v = with_xs client (fun xs -> write xs k v)

let exists client k = match_lwt read_one client k with `Error _ -> return false | _ -> return true

let event_channel_interface =
  let e = ref None in
  fun () -> match !e with
    | Some e -> e
    | None ->
      let e' = Eventchn.init () in
      e := Some e';
      e'

(* Request a hot-unplug *)
let request_close client (domid, devid) =
  lwt backend_path = mk_backend_path client (domid,devid) in
  writev client (List.map (fun (k, v) -> backend_path ^ "/" ^ k, v) (Blkproto.State.to_assoc_list Blkproto.State.Closing))

module Server(S: Storage.S) = struct

let handle_backend t client (domid,devid) =
  let xg = Gnttab.interface_open () in
  let xe = event_channel_interface () in

  lwt backend_path = mk_backend_path client (domid,devid) in

  (* Tell xapi we've noticed the backend *)
  lwt () = write_one client
    (backend_path ^ "/" ^ Blkproto.Hotplug._hotplug_status)
    Blkproto.Hotplug._online in

  try_lwt 

    let size = S.size t in
   
    (* Write the disk information for the frontend *)
    let di = Blkproto.({ DiskInfo.sector_size = sector_size;
                         sectors = Int64.div size (Int64.of_int sector_size);
                         media = Media.Disk;
                         mode = Mode.ReadWrite }) in
    lwt () = writev client (List.map (fun (k, v) -> backend_path ^ "/" ^ k, v) (Blkproto.DiskInfo.to_assoc_list di)) in
    lwt frontend_path = match_lwt read_one client (backend_path ^ "/frontend") with
      | `Error x -> failwith x
      | `OK x -> return x in
   
    (* wait for the frontend to enter state Initialised *)
    lwt () = wait client (fun xs ->
      try_lwt
        lwt state = read xs (frontend_path ^ "/" ^ Blkproto.State._state) in
        if Blkproto.State.of_string state = Some Blkproto.State.Initialised
        || Blkproto.State.of_string state = Some Blkproto.State.Connected
        then return ()
        else raise Eagain
      with Xs_protocol.Enoent _ -> raise Eagain
    ) in

    lwt frontend = readv client frontend_path Blkproto.RingInfo.keys in
    lwt () = Lwt_log.error_f ~logger "3 (frontend state=3)\n" in
    let ring_info = match Blkproto.RingInfo.of_assoc_list frontend with
      | `OK x -> x
      | `Error x -> failwith x in
     
    lwt () = Lwt_log.error_f ~logger "%s" (Blkproto.RingInfo.to_string ring_info) in
    let device_read page ofs sector_start sector_end =
      try_lwt
        let buf = Cstruct.of_bigarray page in
        let len_sectors = sector_end - sector_start + 1 in
        let len_bytes = len_sectors * sector_size in
        let buf = Cstruct.sub buf (sector_start * sector_size) len_bytes in

        S.read t buf ofs len_sectors
      with e ->
        lwt () = Lwt_log.error_f ~logger "read exception: %s, offset=%Ld sector_start=%d sector_end=%d" (Printexc.to_string e) ofs sector_start sector_end in
        Lwt.fail e in
    let device_write page ofs sector_start sector_end =
      try_lwt
        let buf = Cstruct.of_bigarray page in
        let len_sectors = sector_end - sector_start + 1 in
        let len_bytes = len_sectors * sector_size in
        let buf = Cstruct.sub buf (sector_start * sector_size) len_bytes in

        S.write t buf ofs len_sectors
      with e ->
        lwt () = Lwt_log.error_f ~logger "write exception: %s, offset=%Ld sector_start=%d sector_end=%d" (Printexc.to_string e) ofs sector_start sector_end in
        Lwt.fail e in
    let be_thread = Blkback.init xg xe domid ring_info Activations.wait {
      Blkback.read = device_read;
      Blkback.write = device_write;
    } in
    lwt () = writev client (List.map (fun (k, v) -> backend_path ^ "/" ^ k, v) (Blkproto.State.to_assoc_list Blkproto.State.Connected)) in

    (* wait for the frontend to disappear or enter a Closed state *)
    lwt () = wait client (fun xs -> 
      try_lwt
        lwt state = read xs (frontend_path ^ "/state") in
        if Blkproto.State.of_string state <> (Some Blkproto.State.Closed)
        then raise Eagain
        else return ()
      with Xs_protocol.Enoent _ ->
        return ()
    ) in
    Lwt.cancel be_thread;
    Lwt.return ()
  with e ->
    lwt () = Lwt_log.error_f ~logger "exn: %s" (Printexc.to_string e) in
    return ()
end

let find_vm client vm =
  (* First interpret as a domain ID, then UUID, then name *)
  let domainpath x = "/local/domain/" ^ x in
  lwt e = exists client (domainpath vm) in
  if e
  then return (Some vm)
  else begin
    lwt valid_domids = with_xs client (fun xs -> directory xs "/local/domain") in
    lwt valid_uuids = Lwt_list.map_s (fun d ->
      match_lwt read_one client ("/local/domain/" ^ d ^ "/vm") with
      | `OK path -> return (Some (Filename.basename path))
      | `Error _ -> return None
    ) valid_domids in
    lwt valid_names = Lwt_list.map_s (fun d ->
      match_lwt read_one client ("/local/domain/" ^ d ^ "/name") with
      | `OK path -> return (Some path)
      | `Error _ -> return None
    ) valid_domids in
    let uuids_to_domids = List.combine valid_uuids valid_domids in
    let names_to_domids = List.combine valid_names valid_domids in
    if List.mem_assoc (Some vm) uuids_to_domids
    then return (Some (List.assoc (Some vm) uuids_to_domids))
    else if List.mem_assoc (Some vm) names_to_domids
    then return (Some (List.assoc (Some vm) names_to_domids))
    else return None
  end

let (|>) a b = b a
let find_free_vbd client vm =
  lwt used = with_xs client (fun xs -> try_lwt directory xs (Printf.sprintf "/local/domain/%s/device/vbd" vm) with Xs_protocol.Enoent _ -> return []) in
  let free =
    used
    |> List.map int_of_string
    |> List.map Device_number.of_xenstore_key
    |> List.map Device_number.to_disk_number
    |> List.fold_left max (-1)
    |> (fun x -> x + 1) in
  return (Device_number.(to_xenstore_key (of_disk_number false free)))

let backend_of_path path format =
  let configuration = {
    filename = (match path with None -> "" | Some x -> x);
    format;
  } in
  let backend = Backend.choose_backend configuration in
  let module S = (val backend : S) in
  match_lwt S.open_disk configuration with
  | None ->
    failwith "Failed to open_disk"
  | Some t ->
    let module S = Server(S) in
    return (S.handle_backend t)

let main (vm: string) path format =
  lwt client = make () in
  (* Figure out where the device is going to go: *)
  lwt vm = match_lwt find_vm client vm with
    | Some vm -> return vm
    | None -> fail (Failure (Printf.sprintf "Failed to find VM %s" vm)) in
  Printf.fprintf stderr "Operating on VM domain id: %s\n%!" vm;
  lwt device = find_free_vbd client vm in
  Printf.fprintf stderr "Creating device %d (linux device /dev/%s)\n%!"
    device (Device_number.(to_linux_device (of_xenstore_key device)));
  let domid = int_of_string vm in

  (* If we're asked to shutdown cleanly, initiate a hot-unplug *)
  let shutdown_signal _ =
    Printf.fprintf stderr "Received signal, requesting hot-unplug.\n%!";
    let (_: unit Lwt.t) = request_close client (domid, device) in
    () in
  List.iter
    (fun signal ->
      let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal signal shutdown_signal in
      ()
    ) [ Sys.sigint; Sys.sigterm ];

  (* Construct the device: *)
  let (_: unit Lwt.t) = Activations.run (event_channel_interface ()) in
  lwt backend_path = mk_backend_path client (domid, device) in
  lwt frontend_path = mk_frontend_path client (domid, device) in
  lwt backend_domid = get_my_domid client in
  let c = Blkproto.Connection.({
    virtual_device = string_of_int device;
    backend_path;
    backend_domid;
    frontend_path;
    frontend_domid = domid;
    mode = Blkproto.Mode.ReadWrite;
    media = Blkproto.Media.Disk;
    removable = false;
  }) in
  lwt () = with_xst client (fun xs ->
    Lwt_list.iter_s (fun (owner_domid, (k, v)) ->
      lwt () = write xs k v in
      let acl =
        let open Xs_protocol.ACL in
        { owner = owner_domid; other = READ; acl = [ ] } in
      lwt () = setperms xs k acl in
      return ()
    ) (Blkproto.Connection.to_assoc_list c)
  ) in

  lwt t = backend_of_path path format in
  (* Serve requests until the frontend closes: *)
  lwt () = t client (int_of_string vm, device) in
  (* Clean up the backend: *)
  with_xs client (fun xs ->
    lwt () = rm xs backend_path in
    Printf.fprintf stderr "Cleaning up backend path %s\n%!" backend_path;
    lwt () = rm xs frontend_path in
    Printf.fprintf stderr "Cleaning up frontend path %s\n%!" frontend_path;
    return ()
  )

let connect (common: Common.t) (vm: string option) (path: string option) (format: string option) =
  let vm = match vm with
    | None -> failwith "Please name a VM to attach the disk to"
    | Some x -> x in
  let () = Lwt_main.run (main vm path format) in
  `Ok ()

open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in 
  Term.(pure Common.make $ debug $ verb)

let connect_command =
  let doc = "Connect a disk to a specific VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Connect a disk to a specific VM.";
    `P "If no path is provided then all read and write requests will succeed but no data will be modified. This is useful for testing only.";
    `P "If a path is provided then it will be used as the backing file for the VM's disk. If no explicit format is specified then we assume RAW.";
  ] in
  let vm =
    let doc = "The domain, UUID or name of the VM to connect disk to." in
    Arg.(value & pos 0 (some string) None & info [ ] ~docv:"VM" ~doc) in
  let path =
    let doc = "The path to the backing file containing disk data." in
    Arg.(value & opt (some file) None & info [ "path" ] ~docv:"PATH" ~doc) in
  let format =
    let doc = "The format of the backing file." in
    Arg.(value & opt (some string) (Some "raw") & info [ "format" ] ~docv:"FORMAT" ~doc) in 
  Term.(ret (pure connect $ common_options_t $ vm $ path $ format)),
  Term.info "connect" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "manipulate virtual block devices on Xen virtual machines" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "xen-disk" ~version:"1.0.1" ~sdocs:_common_options ~doc ~man

let cmds = [ connect_command ]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
