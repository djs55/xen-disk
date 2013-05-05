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
let project_url = "http://github.com/djs55/vhddisk"

open Lwt
open Blkback
open Xs_protocol
module Client = Xs_client.Client(Xs_transport_lwt_unix_client)
open Client

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
  }
  (** options common to all subcommands *)

  let make verbose debug = { verbose; debug }
end

let ( >>= ) = Blkproto.( >>= )

module BackendSet = Set.Make(struct type t = int * int let compare = compare end)

let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

let backends = ref BackendSet.empty

let sector_size = 512
let empty_sector = String.make sector_size '\000'

module type STORAGE = sig
  type t

  val size: t -> int64
  val read: t -> OS.Io_page.t -> int64 -> int -> int -> unit Lwt.t
  val write: t -> OS.Io_page.t -> int64 -> int -> int -> unit Lwt.t
end

module VHD = struct
  type t = Vhd.vhd

  let size t = t.Vhd.footer.Vhd.f_current_size

  let read vhd buf offset sector_start sector_end =
    try_lwt
      lwt () = for_lwt i=sector_start to sector_end do
      let offset = Int64.sub offset (Int64.of_int sector_start) in
      let sectornum = Int64.add offset (Int64.of_int i) in
      lwt res = Vhd.get_sector_pos vhd sectornum in
      match res with 
      | Some (mmap, mmappos) -> 
        let mmappos = Int64.to_int mmappos in
        (* let madvpos = (mmappos / 4096) * 4096 in
           Lwt_bytes.madvise mmap madvpos 512 Lwt_bytes.MADV_WILLNEED;
           lwt () = Lwt_bytes.wait_mincore mmap madvpos in *)
        Lwt_bytes.unsafe_blit mmap mmappos buf (i*512) 512;
        Lwt.return ()
      | None -> 
        Lwt_bytes.blit_string_bytes empty_sector 0 buf (i*512) 512;
        Lwt.return ()
      done in
      Lwt.return ()
    with e ->
      lwt () = Lwt_log.error_f ~logger "Caught exception: %s, offset=%Ld sector_start=%d sector_end=%d" (Printexc.to_string e) offset sector_start sector_end in
      Lwt.fail e

  let write vhd buf offset sector_start sector_end =
    let sec = String.create 512 in
    let offset = Int64.sub offset (Int64.of_int sector_start) in
    try_lwt
      lwt () = for_lwt i=sector_start to sector_end do
        Lwt_bytes.blit_bytes_string buf (i*512) sec 0 512;
        Vhd.write_sector vhd (Int64.add offset (Int64.of_int i)) sec
      done in
      Lwt.return ()
    with e ->
      lwt () = Lwt_log.error_f ~logger "Caught exception: %s, offset=%Ld sector_start=%d sector_end=%d" (Printexc.to_string e) offset sector_start sector_end in
      Lwt.fail e
end

module MMAP = struct
  type t = Lwt_bytes.t

  let read mmap buf offset sector_start sector_end =
    let offset = Int64.to_int offset in
    try_lwt
      let len = (sector_end - sector_start + 1) * 512 in
      let pos = (offset / 8) * 4096 in
      let pos2 = offset * 512 in
      Lwt_bytes.madvise mmap pos (len + pos2 - pos) Lwt_bytes.MADV_WILLNEED;
      lwt () = Lwt_bytes.wait_mincore mmap pos2 in
      Lwt_bytes.unsafe_blit mmap pos2 buf (sector_start*512) len;
      Lwt.return ()
    with e ->
      lwt () = Lwt_log.error_f ~logger "Caught exception: %s, offset=%d sector_start=%d sector_end=%d" (Printexc.to_string e) offset sector_start sector_end in
      Lwt.fail e

  let write mmap buf offset sector_start sector_end =
    let offset = Int64.to_int offset in
    let len = (sector_end - sector_start + 1) * 512 in
    Lwt_bytes.unsafe_blit buf (sector_start * 512) mmap (offset * 512) len;
    Lwt.return () 
end

let mk_backend_path client (domid,devid) =
  lwt self = with_xs client (fun xs -> read xs "domid") in
  return (Printf.sprintf "/local/domain/%s/backend/%s/%d/%d" self name domid devid)

let writev client pairs =
  with_xs client (fun xs ->
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

module Server(S: STORAGE) = struct

let handle_backend t client (domid,devid) =
  let xg = Gnttab.interface_open () in
  let xe = Eventchn.init () in

  lwt backend_path = mk_backend_path client (domid,devid) in

  (* Tell xapi we've noticed the backend *)
  lwt () = write_one client
    (backend_path ^ Blkproto.Hotplug._hotplug_status)
    Blkproto.Hotplug._online in

  try_lwt 

    let size = S.size t in
   
    (* Write the disk information for the frontend *)
    let di = Blkproto.({ DiskInfo.sector_size = sector_size;
                         sectors = Int64.div size (Int64.of_int sector_size);
                         media = Media.Disk;
                         mode = Mode.ReadWrite }) in
    lwt () = writev client (List.map (fun (k, v) -> backend_path ^ k, v) (Blkproto.DiskInfo.to_assoc_list di)) in
    lwt frontend_path = match_lwt read_one client (backend_path ^ "frontend") with
      | `Error x -> failwith x
      | `OK x -> return x in
   
    (* wait for the frontend to enter state Initialised *)
    lwt () = wait client (fun xs ->
      try_lwt
        lwt state = read xs (frontend_path ^ Blkproto.State._state) in
        if Blkproto.State.of_string state = Some Blkproto.State.Initialised
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
    let be_thread = Blkback.init xg xe domid ring_info Activations.wait {
      Blkback.read = S.read t;
      Blkback.write = S.write t
    } in
    lwt () = writev client (List.map (fun (k, v) -> backend_path ^ k, v) (Blkproto.State.to_assoc_list Blkproto.State.Connected)) in

    (* wait for the frontend to disappear *)
    lwt () = wait client (fun xs -> 
      try_lwt
        lwt _ = read xs (frontend_path ^ "/state") in
        raise Eagain
      with Xs_protocol.Eagain ->
        return ()
    ) in
    Lwt.cancel be_thread;
    Lwt.return ()
  with e ->
    lwt () = Lwt_log.error_f ~logger "exn: %s" (Printexc.to_string e) in
    return ()
end

module S = Server(VHD)

(*
let rec new_backends_loop client =
  lwt () = with_xs client (fun xs -> write xs backend_path "foo") in
  wait client (fun xs ->
    lwt dir = directory xs backend_path in
    let dir = List.filter (fun x -> String.length x > 0) dir in
    lwt _ = Lwt_log.error ~logger
      ("Paths: [" ^ 
        (String.concat "," 
          (List.map (fun s -> Printf.sprintf "'%s'" s) dir)) ^ "]\n") in
    lwt dir = Lwt_list.fold_left_s (fun acc path1 -> 
    let new_path = (backend_path ^ "/" ^ path1) in
    lwt () = Lwt_log.error ~logger ("checking path: " ^ new_path ^ "\n") in
    try_lwt 
      lwt subdir = directory xs new_path in
      return (List.fold_left (fun acc path2 ->  
        try
          let domid = int_of_string path1 in
          let devid = int_of_string path2 in
          BackendSet.add (domid,devid) acc
        with _ ->
          acc
      ) acc subdir)
    with _ -> return acc) BackendSet.empty dir in
    let diff = BackendSet.diff dir !backends in
    if BackendSet.is_empty diff 
    then raise Eagain 
    else 
      begin 
        backends := dir;
        BackendSet.iter (fun x -> ignore(handle_backend client x)) diff;
        return ()
      end) >>= fun () -> new_backends_loop client

let main () =
  lwt () = Lwt_log.debug ~logger "main()" in
  let (_: unit Lwt.t) = Activations.run () in
  lwt client = make () in
  new_backends_loop client
*)

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

let main (vm: string) path =
  lwt client = make () in
  lwt vm = match_lwt find_vm client vm with
    | Some vm -> return vm
    | None -> fail (Failure (Printf.sprintf "Failed to find VM %s" vm)) in
  Printf.fprintf stderr "VM domain id: %s\n%!" vm;
  return ()

let connect (common: Common.t) (vm: string option) (path: string option) =
  let vm = match vm with
    | None -> failwith "Please name a VM to attach the disk to"
    | Some x -> x in
  let () = Lwt_main.run (main vm path) in
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
  ] in
  let vm =
    let doc = "The domain, UUID or name of the VM to connect disk to." in
    Arg.(value & pos 0 (some string) None & info [ ] ~docv:"VM" ~doc) in
  let path =
    let doc = "The path to the file containing the disk data." in
    Arg.(value & opt (some file) None & info [ "path" ] ~docv:"PATH" ~doc) in
  Term.(ret (pure connect $ common_options_t $ vm $ path)),
  Term.info "connect" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "manipulate virtual block devices on Xen virtual machines" in 
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "blkback" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man

let cmds = [ connect_command ]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
