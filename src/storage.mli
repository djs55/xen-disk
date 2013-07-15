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

val sector_size: int

module type S = sig
  (** A concrete mechanism to access and update a virtual disk. *)

  type t
  (** An open virtual disk *)

  val size: t -> int64
  (** [size t] is the size of the virtual disk in bytes. The actual
      number of bytes stored on media may be different. *)

  val read: t -> Cstruct.t -> int64 -> int -> unit Lwt.t
  (** [read t buf offset_sectors len_sectors] copies [len_sectors]
      sectors beginning at sector [offset_sectors] from [t] into [buf] *)

  val write: t -> Cstruct.t -> int64 -> int -> unit Lwt.t
  (** [write t buf offset_sectors len_sectors] copies [len_sectors]
      sectors from [buf] into [t] beginning at sector [offset_sectors]. *)
end
