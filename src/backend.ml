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
open Storage

module UNIMPLEMENTED = struct
  type 'a io = 'a Lwt.t

  type id = string

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Is_read_only
    | `Disconnected
  ]

  type page_aligned_buffer = Cstruct.t

  type info = {
    read_write: bool;
    sector_size: int;
    size_sectors: int64;
  }

  let get_info t = return { read_write = false; sector_size = 512; size_sectors = 0L }
  let connect id = return (`Error `Unimplemented)
  let read t offset bufs = return (`Error `Unimplemented)
  let write t offset bufs = return (`Error `Unimplemented)
  let disconnect t = return ()
end


module DISCARD = struct
  include UNIMPLEMENTED

  type t = string
  let id x = x

  (** Used to test the raw ring performance *)

  let mib = Int64.mul 1024L 1024L
  let gib = Int64.mul mib 1024L
  let get_info t = return { read_write = false; sector_size = 512; size_sectors = Int64.div gib 512L}
  let connect id = return (`Ok id)
  let read t offset bufs = return (`Ok ())
  let write t offset bufs = return (`Ok ())
  let disconnect t = return ()
end

let buffered_prefix = "buffered:"
let startswith prefix x =
  let prefix' = String.length prefix in
  let x' = String.length x in
  prefix' <= x' && (String.sub x 0 prefix' = prefix)
let filename_of_id id =
  if startswith buffered_prefix id
  then String.sub id (String.length buffered_prefix) (String.length id - (String.length buffered_prefix))
  else id

module MMAP = struct
  include UNIMPLEMENTED

  type t = {
    id: string;
    size: int64;
    mmap: Cstruct.t;
  }

  let id t = t.id

  let connect id =
    let fd = Unix.openfile (filename_of_id id) [ Unix.O_RDWR ] 0o0 in
    let stats = Unix.LargeFile.fstat fd in
    let mmap = Cstruct.of_bigarray (Lwt_bytes.map_file ~fd ~shared:true ()) in
    Unix.close fd;
    let size = stats.Unix.LargeFile.st_size in
    return (`Ok { id; size; mmap })

  let disconnect t = return () (* mmap will be GCed *)

  let get_info t =
    return { read_write = true; sector_size = 512; size_sectors = Int64.div t.size 512L }

  let forall offset bufs f =
    let rec loop offset = function
    | [] -> ()
    | b :: bs ->
      f offset b;
      loop (offset + (Cstruct.len b)) bs in
    loop (Int64.to_int offset * 512) bufs;
    return (`Ok ())

  let read t offset bufs =
    forall offset bufs
      (fun offset buf ->
        Cstruct.blit t.mmap offset buf 0 (Cstruct.len buf)
      )

  let write t offset bufs =
    forall offset bufs
      (fun offset buf ->
        Cstruct.blit buf 0 t.mmap offset (Cstruct.len buf)
      )
end


(* Given a configuration, choose which backend to use *)
let choose_backend { filename = filename; format = format } = match filename, format with
  | "", _ ->
    (module DISCARD: BLOCK)
  | _, Some "vhd" ->
    (module Vhd_lwt.Block: BLOCK)
  | _, Some "mmap" ->
    (module MMAP: BLOCK)
  | _, Some "raw"
  | _, None ->
    (module Block: BLOCK)
  | _, Some format ->
    failwith (Printf.sprintf "Unknown format: %s" format)

