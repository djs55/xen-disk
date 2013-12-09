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

module DISCARD = struct
  (** Used to test the raw ring performance *)

  type t = unit

  let open_disk _ = return (Some ())
  let size () = Int64.(mul (mul 128L 1024L) 1024L)
  let read () _ _ _ = return ()
  let write () _ _ _ = return ()
end

module MMAP = struct
  (** Virtual disks backed by (possibly sparse) files accessed via mmap(2) *)
  type t = int64 * Cstruct.t

  let open_disk configuration =
    let fd = Unix.openfile configuration.filename [ Unix.O_RDWR ] 0o0 in
    let stats = Unix.LargeFile.fstat fd in
    let mmap = Lwt_bytes.map_file ~fd ~shared:true () in
    Unix.close fd;
    return (Some (stats.Unix.LargeFile.st_size, Cstruct.of_bigarray mmap))

  let size = fst

  let read (_, mmap) buf offset_sectors len_sectors =
    let offset_sectors = Int64.to_int offset_sectors in
    let len_bytes = len_sectors * sector_size in
    let offset_bytes = offset_sectors * sector_size in
    Cstruct.blit mmap offset_bytes buf 0 len_bytes;
    return ()

  let write (_, mmap) buf offset_sectors len_sectors =
    let offset_sectors = Int64.to_int offset_sectors in
    let offset_bytes = offset_sectors * sector_size in
    let len_bytes = len_sectors * sector_size in
    Cstruct.blit buf 0 mmap offset_bytes len_bytes;
    return () 
end

(* Given a configuration, choose which backend to use *)
let choose_backend { filename = filename; format = format } = match filename, format with
  | "", _ ->
    (module DISCARD: Storage.S)
  | _, Some "raw"
  | _, None ->
    (module MMAP: Storage.S)
  | _, Some format ->
    failwith (Printf.sprintf "Unknown format: %s" format)

