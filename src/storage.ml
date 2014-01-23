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
(*
let sector_size = 512
*)
type configuration = {
  filename: string;
  format: string option;
}
module type S = V1_LWT.BLOCK
  with type id := string
(*
module type S = sig
  type t

  val open_disk: configuration -> t option Lwt.t
  val size: t -> int64
  val read: t -> Cstruct.t -> int64 -> int -> unit Lwt.t
  val write: t -> Cstruct.t -> int64 -> int  -> unit Lwt.t
end
*)
