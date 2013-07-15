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

module type S = sig
  (** A concrete mechanism to access and update a virtual disk. *)

  type t
  (** An open virtual disk *)

  val size: t -> int64
  (** [size t] is the size of the virtual disk in bytes. The actual
      number of bytes stored on media may be different. *)

  val read: t -> OS.Io_page.t -> int64 -> int -> int -> unit Lwt.t
  (** [read t page offset sector_start sector_end] copies all sectors
      within the range [sector_start] to [sector_end] inclusive from
      [t] starting at sector [offset] into [page] starting at sector
      offset [sector_start].
      FIXME: switch to using Cstruct.t to capture the range
      FIXME: use a length rather than sector_start ... sector_end
    *)

  val write: t -> OS.Io_page.t -> int64 -> int -> int -> unit Lwt.t
  (** [write t page offset sector_start sector_end] copies all sectors
      within the range [sector_start] to [sector_end] inclusive,
      starting at sector [sector_start] in [page] to [t], starting at
      sector offset [offset].
      FIXME: switch to using a Cstruct.t to capture the range
      FIXME: use a length rather than sector_start ... sector_end
    *)
end
