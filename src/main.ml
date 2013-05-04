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

open Lwt
open Blkback
open Xs_protocol
module Client = Xs_client.Client(Xs_transport_lwt_unix_client)
open Client

module BackendSet = Set.Make(struct type t = int * int let compare = compare end)

let backend_path="/local/domain/0/backend/ovbd"
let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

let backends = ref BackendSet.empty

let xg = Gnttab.interface_open ()
let xe = Eventchn.init ()

let sector_size = 512
let empty_sector = String.make sector_size '\000'

let do_read_vhd vhd buf offset sector_start sector_end =
  try_lwt
    lwt () = for_lwt i=sector_start to sector_end do
    let offset = Int64.sub offset (Int64.of_int sector_start) in
    let sectornum = Int64.add offset (Int64.of_int i) in
    lwt res = Vhd.get_sector_pos vhd sectornum in
    match res with 
    | Some (mmap, mmappos) -> 
      let mmappos = Int64.to_int mmappos in
      let madvpos = (mmappos / 4096) * 4096 in
      (* Lwt_bytes.madvise mmap madvpos 512 Lwt_bytes.MADV_WILLNEED;
         lwt () = Lwt_bytes.wait_mincore mmap madvpos in *)
      Lwt_bytes.unsafe_blit mmap mmappos buf (i*512) 512;
      Lwt.return ()
    | None -> 
      Lwt_bytes.blit_string_bytes empty_sector 0 buf (i*512) 512;
      Lwt.return ()
    done in
    Lwt.return ()
  with e ->
    Lwt_log.error_f ~logger "Caught exception: %s, offset=%Ld sector_start=%d sector_end=%d" (Printexc.to_string e) offset sector_start sector_end;
    Lwt.fail e

let do_write_vhd vhd buf offset sector_start sector_end =
  let sec = String.create 512 in
  let offset = Int64.sub offset (Int64.of_int sector_start) in
  try_lwt
    lwt () = for_lwt i=sector_start to sector_end do
      Lwt_bytes.blit_bytes_string buf (i*512) sec 0 512;
      Vhd.write_sector vhd (Int64.add offset (Int64.of_int i)) sec
    done in
    Lwt.return ()
  with e ->
    Lwt_log.error_f ~logger "Caught exception: %s, offset=%Ld sector_start=%d sector_end=%d" (Printexc.to_string e) offset sector_start sector_end;
    Lwt.fail e

let do_read mmap buf offset sector_start sector_end =
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
    Lwt_log.error_f ~logger "Caught exception: %s, offset=%d sector_start=%d sector_end=%d" (Printexc.to_string e) offset sector_start sector_end;
    Lwt.fail e

let do_write mmap buf offset sector_start sector_end =
  let offset = Int64.to_int offset in
  let len = (sector_end - sector_start + 1) * 512 in
  Lwt_bytes.unsafe_blit buf (sector_start * 512) mmap (offset * 512) len;
  Lwt.return ()
 
let mk_backend_path (domid,devid) = 
  Printf.sprintf "%s/%d/%d/" backend_path domid devid

let writev client pairs =
  with_xs client (fun xs ->
    Lwt_list.iter_s (fun (k, v) -> write xs k v) pairs
  )

let readv client path keys =
  with_xs client (fun xs ->
    Lwt_list.map_s (fun k -> lwt v = read xs (path ^ "/" ^ k) in return (k, v)) keys
  )

let read_one client k = with_xs client (fun xs -> read xs k)
let write_one client k v = with_xs client (fun xs -> write xs k v)

let handle_backend client (domid,devid) =
  let backend_path = mk_backend_path (domid,devid) in

  (* Tell xapi we've noticed the backend *)
  lwt () = write_one client
    (backend_path ^ Blkproto.Hotplug._hotplug_status)
    Blkproto.Hotplug._online in

  (* Read the params key *)
  lwt params = read_one client (backend_path ^ Blkproto.Hotplug._params) in

  lwt () = Lwt_log.error ~logger ("Params=" ^ params ^ "\n") in

  try_lwt 

    lwt vhd = Vhd.load_vhd Sys.argv.(1) in

    let size = vhd.Vhd.footer.Vhd.f_current_size in
   
    (* Write the disk information for the frontend *)
    let di = Blkproto.({ DiskInfo.sector_size = sector_size;
                         sectors = Int64.div size (Int64.of_int sector_size);
                         media = Media.Disk;
                         mode = Mode.ReadWrite }) in
    lwt () = writev client (List.map (fun (k, v) -> backend_path ^ k, v) (Blkproto.DiskInfo.to_assoc_list di)) in
    lwt frontend_path = read_one client (backend_path ^ "frontend") in
   
    let handled=ref false in

    (* wait for the frontend to enter state Initialised *)
    lwt () = wait client (fun xs ->
      lwt state = read xs (frontend_path ^ Blkproto.State._state) in
      if Blkproto.State.of_string state = Some Blkproto.State.Initialised
      then return ()
      else raise Eagain
    ) in

    lwt frontend = readv client frontend_path Blkproto.RingInfo.keys in
    lwt () = Lwt_log.error_f ~logger "3 (frontend state=3)\n" in
    let ring_info = match Blkproto.RingInfo.of_assoc_list frontend with
      | `OK x -> x
      | `Error x -> failwith x in
     
    lwt () = Lwt_log.error_f ~logger "%s" (Blkproto.RingInfo.to_string ring_info) in
    let be_thread = Blkback.init xg xe domid ring_info Activations.wait {
      Blkback.read = do_read_vhd vhd;
      Blkback.write = do_write_vhd vhd
    } in
    lwt () = writev client (List.map (fun (k, v) -> backend_path ^ k, v) (Blkproto.State.to_assoc_list Blkproto.State.Connected)) in

    (* wait for the frontend to disappear *)
    lwt () = wait client (fun xs -> 
      try_lwt
        lwt x = read xs (frontend_path ^ "/state") in
        lwt _ = Lwt_log.error_f ~logger "XXX state=%s" x in
        raise Eagain
      with Xs_protocol.Enoent _ -> 
        lwt _ = Lwt_log.error_f ~logger "XXX caught enoent while reading frontend state" in
        return ()) in
    Lwt.cancel be_thread;
    Lwt.return ()
  with e ->
    lwt () = Lwt_log.error_f ~logger "exn: %s" (Printexc.to_string e) in
    return ()

let rec new_backends_loop client =
  with_xs client (fun xs -> 
    write xs backend_path "foo");
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
  Activations.run ();
  lwt client = make () in
  new_backends_loop client

let _ =
  Lwt_main.run (main ())
