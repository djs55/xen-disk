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

let project_url = "http://github.com/mirage/xen-disk"

let name =
  let sanitise x =
    let x' = String.length x in
    let y = String.create x' in
    for i = 0 to x' - 1 do
      y.[i] <- match x.[i] with
               | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> x.[i]
               | _ -> '_'
    done;
    y in
 sanitise (Filename.basename Sys.argv.(0))

open Lwt
open Blkback
open Xs_protocol
module Client = Xs_client_lwt.Client(Xs_transport_lwt_unix_client)
open Client
open Backend
open Storage
open Gnt

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

let read_one client k = immediate client (fun xs ->
  try_lwt
    lwt v = read xs k in
    return (`OK v)
  with _ -> return (`Error ("failed to read: " ^ k)))

let exists client k = match_lwt read_one client k with `Error _ -> return false | _ -> return true

let find_vm client vm =
  (* First interpret as a domain ID, then UUID, then name *)
  let domainpath x = "/local/domain/" ^ x in
  lwt e = exists client (domainpath vm) in
  if e
  then return (Some vm)
  else begin
    lwt valid_domids = immediate client (fun xs -> directory xs "/local/domain") in
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
  lwt used = immediate client (fun xs -> try_lwt directory xs (Printf.sprintf "/local/domain/%s/device/vbd" vm) with Xs_protocol.Enoent _ -> return []) in
  let free =
    used
    |> List.map int_of_string
    |> List.map Device_number.of_xenstore_key
    |> List.map Device_number.to_disk_number
    |> List.fold_left max (-1)
    |> (fun x -> x + 1) in
  return (Device_number.(to_xenstore_key (of_disk_number false free)))

let main (vm: string) path backend =
  lwt client = make () in
  (* Figure out where the device is going to go: *)
  lwt vm = match_lwt find_vm client vm with
    | Some vm -> return vm
    | None -> fail (Failure (Printf.sprintf "Failed to find VM %s" vm)) in
  Printf.fprintf stderr "Operating on VM domain id: %s\n%!" vm;
  lwt devid = find_free_vbd client vm in
  Printf.fprintf stderr "Creating device %d (linux device /dev/%s)\n%!"
    devid (Device_number.(to_linux_device (of_xenstore_key devid)));
  let device = int_of_string vm, devid in

  let configuration = {
    filename = (match path with None -> "" | Some x -> "buffered:" ^ x);
    backend;
  } in
  let backend = Backend.choose_backend configuration in
  (* Serve requests until the frontend closes: *)
  Printf.fprintf stderr "Press Control+C to disconnect device.\n%!";

  let module S = (val backend : BLOCK) in
  let module S' = Blkback.Make(Unix_activations)(Client)(S) in

  (* If we're asked to shutdown cleanly, first initiate a hot-unplug.
     If we're asked again, be more aggressive. *)
  let already_asked = ref false in
  let shutdown_signal _ =
    if not(!already_asked) then begin
      already_asked := true;
      Printf.fprintf stderr "Received signal, requesting hot-unplug.\n%!";
      let (_: unit Lwt.t) = S'.request_close name device in
      ()
    end else begin
      Printf.fprintf stderr "Received signal again, tearing down the backend.\n%!";
      let (_: unit Lwt.t) = S'.force_close device in
      ()
    end in
  List.iter
    (fun signal ->
      let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal signal shutdown_signal in
      ()
    ) [ Sys.sigint; Sys.sigterm ];

  lwt () = S'.create name device in
  lwt () = S'.run configuration.filename name device in 
  S'.destroy name device

let connect (common: Common.t) (vm: string) (path: string option) (backend: string option) =
  match vm with
    | "" ->
      `Error(true, "I don't know which VM to operate on. Please supply a VM name or uuid.")
    | vm ->
      let () = Lwt_main.run (main vm path backend) in
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
    `P "If a path is provided then it will be used as the backing file for the VM's disk. If no explicit backend is specified then we assume RAW.";
  ] in
  let vm =
    let doc = "The domain, UUID or name of the VM to connect disk to." in
    Arg.(required & pos 0 (some string) None & info [ ] ~docv:"VM-name-or-uuid" ~doc) in
  let path =
    let doc = "The path to the backing file containing disk data." in
    Arg.(value & opt (some file) None & info [ "path" ] ~docv:"PATH" ~doc) in
  let backend =
    let doc = "The type of backing store." in
    Arg.(value & opt (some string) (Some "raw") & info [ "backend" ] ~docv:"FORMAT" ~doc) in 
  Term.(ret (pure connect $ common_options_t $ vm $ path $ backend)),
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
