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

(* Given a configuration, choose which backend to use *)
let choose_backend { filename = filename; format = format } = match filename, format with
  | "", _ ->
    (module DISCARD: Storage.S)
  | _, Some "raw"
  | _, None ->
    (module Block: Storage.S)
  | _, Some format ->
    failwith (Printf.sprintf "Unknown format: %s" format)

