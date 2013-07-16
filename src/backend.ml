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

module VHD = struct
  (** Virtual disks backed by vhd-format files accessed via mmap(2) *)

  type t = Vhd.vhd

  let size t = t.Vhd.footer.Vhd.f_current_size

  let empty_sector = String.make sector_size '\000'

  let open_disk configuration =
    lwt vhd = Vhd.load_vhd configuration.filename in
    return (Some vhd)

  let read vhd buf offset_sectors len_sectors =
    for_lwt i = 0 to len_sectors - 1 do
      let this_sector = Int64.(add offset_sectors (of_int i)) in
      match_lwt Vhd.get_sector_pos vhd this_sector with
      | Some (mmap, ofs) ->
        let ofs = Int64.to_int ofs in
        let mmap' = Cstruct.of_bigarray mmap in
        Cstruct.blit mmap' ofs buf (i * sector_size) sector_size;
        return ()
      | None ->
        Cstruct.blit_from_string empty_sector 0 buf (i * sector_size) sector_size;
        return ()
    done

  let write vhd buf offset_sectors len_sectors =
    let sec = String.create sector_size in
    for_lwt i = 0 to len_sectors - 1 do
      Cstruct.blit_to_string buf (i * sector_size) sec 0 sector_size;
      Vhd.write_sector vhd Int64.(add offset_sectors (of_int i)) sec
    done
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
  | _, Some "vhd" ->
    (module VHD: Storage.S)
  | _, Some format ->
    failwith (Printf.sprintf "Unknown format: %s" format)

