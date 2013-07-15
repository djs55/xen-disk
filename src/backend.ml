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

module DISCARD = struct
  type t = unit

  let size () = Int64.(mul (mul 128L 1024L) 1024L)
  let read () _ _ _ _ = return ()
  let write () _ _ _ _ = return ()
end

module VHD = struct
  type t = Vhd.vhd

  let size t = t.Vhd.footer.Vhd.f_current_size

  let sector_size = 512
  let empty_sector = String.make sector_size '\000'

  let read vhd buf offset sector_start sector_end =
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
    return ()

  let write vhd buf offset sector_start sector_end =
    let sec = String.create 512 in
    let offset = Int64.sub offset (Int64.of_int sector_start) in
    lwt () = for_lwt i=sector_start to sector_end do
      Lwt_bytes.blit_bytes_string buf (i*512) sec 0 512;
      Vhd.write_sector vhd (Int64.add offset (Int64.of_int i)) sec
    done in
    return ()
end

module MMAP = struct
  type t = Lwt_bytes.t

  let read mmap buf offset sector_start sector_end =
    let offset = Int64.to_int offset in
    let len = (sector_end - sector_start + 1) * 512 in
    let pos = (offset / 8) * 4096 in
    let pos2 = offset * 512 in
    Lwt_bytes.madvise mmap pos (len + pos2 - pos) Lwt_bytes.MADV_WILLNEED;
    lwt () = Lwt_bytes.wait_mincore mmap pos2 in
    Lwt_bytes.unsafe_blit mmap pos2 buf (sector_start*512) len;
    return ()

  let write mmap buf offset sector_start sector_end =
    let offset = Int64.to_int offset in
    let len = (sector_end - sector_start + 1) * 512 in
    Lwt_bytes.unsafe_blit buf (sector_start * 512) mmap (offset * 512) len;
    return () 
end


