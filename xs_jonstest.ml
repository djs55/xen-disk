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
open Xs_packet
module Client = Xs_client.Client(Xs_transport_unix)
open Client

module BackendSet = Set.Make(struct type t = int * int let compare = compare end)

let backend_path="/local/domain/0/backend/ovbd"
let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

let backends = ref BackendSet.empty

let xg = Gnttab.interface_open ()

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
    let len = (sector_end - sector_start) * 512 in
	Lwt_bytes.unsafe_blit buf (sector_start * 512) mmap (offset * 512) len;
	Lwt.return ()
 
let mk_backend_path (domid,devid) subpath = 
	Printf.sprintf "%s/%d/%d/%s" backend_path domid devid subpath

let string_of_segs segs = 
	Printf.sprintf "[%s]" (
	String.concat "," (List.map (fun seg ->
		Printf.sprintf "{gref=%ld first=%d last=%d}" seg.Blkif.Req.gref seg.Blkif.Req.first_sector seg.Blkif.Req.last_sector) (Array.to_list segs)))

let string_of_req req =
	Printf.sprintf "op=%s\nhandle=%d\nid=%Ld\nsector=%Ld\nsegs=%s\n" (Blkif.Req.string_of_op req.Blkif.Req.op) req.Blkif.Req.handle
		req.Blkif.Req.id req.Blkif.Req.sector (string_of_segs req.Blkif.Req.segs)


let handle_backend client (domid,devid) =
	(* Tell xapi we've noticed the backend *)
	lwt () = with_xs client (fun xs -> write xs (mk_backend_path (domid,devid) "hotplug-status") "online") in

    (* Read the params key *)
    lwt params = with_xs client (fun xs -> read xs (mk_backend_path (domid,devid) "params")) in

    lwt () = Lwt_log.error ~logger ("Params=" ^ params ^ "\n") in

    lwt mmapfd = Lwt_unix.openfile Sys.argv.(1) [Unix.O_RDWR] 0o644 in
    let mmap = Lwt_bytes.map_file ~fd:(Lwt_unix.unix_file_descr mmapfd) ~shared:true () in

	lwt size = 
			lwt stats = Lwt_unix.LargeFile.fstat mmapfd in
            Lwt.return stats.Unix.LargeFile.st_size
    in

   
    (* Write some junk into the backend for the frontend to read *)

    lwt () = with_xs client (fun xs -> write xs (mk_backend_path (domid,devid) "sector-size") "512") in
    lwt () = with_xs client (fun xs -> write xs (mk_backend_path (domid,devid) "sectors") (Printf.sprintf "%Ld" (Int64.div size 512L))) in
    lwt frontend = with_xs client (fun xs -> read xs (mk_backend_path (domid,devid) "frontend")) in

    let handled=ref false in

    wait client (fun xs -> 
	   lwt state = read xs (frontend ^ "/state") in
       match state with
       | "1"
	   | "2"
	   | "3" ->
		   lwt () = Lwt_log.error_f ~logger "state=%s\n" state in
		   raise Eagain
	   | "4" ->
		   lwt () = Lwt_log.error_f ~logger "3 (frontend state=4)\n" in
		   lwt ring_ref = with_xs client (fun xs -> read xs (frontend ^ "/ring-ref")) in
           let ring_ref = Int32.of_string ring_ref in
	       lwt evtchn = with_xs client (fun xs -> read xs (frontend ^ "/event-channel")) in
           let evtchn = int_of_string evtchn in
           lwt () = Lwt_log.error_f ~logger "Got ring-ref=%ld evtchn=%d\n" ring_ref evtchn in
           lwt () = with_xs client (fun xs -> write xs (mk_backend_path (domid,devid) "state") "4") in
           begin if not !handled then 
			   let be_thread = Blkif.Backend.init xg domid ring_ref evtchn {
				   Blkif.Backend.read = do_read mmap;
				   Blkif.Backend.write = do_write mmap } in
			   let waiter = 
				   lwt () = wait client 
				       (fun xs -> 
					       try_lwt 
						       lwt x = read xs (frontend ^ "/state") in
			                   lwt _ = Lwt_log.error_f ~logger "XXX state=%s" x in
			                   raise Eagain 
	                       with Xs_packet.Enoent _ -> 
					           lwt _ = Lwt_log.error_f ~logger "XXX caught enoent while reading frontend state" in
					           return ()); 
                   in
                   Lwt.cancel be_thread;
	               return ()
               in
(*handle_ring mmap client (domid,devid) frontend ring_ref evtchn in*)
			   handled := true
		   else 
			   () 
		   end;
           raise Eagain
       | "5"
	   | _ ->
           return ())

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
