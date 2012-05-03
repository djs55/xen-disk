type handle
external interface_open : unit -> handle = "stub_xc_gnttab_open"
external interface_close : handle -> unit = "stub_xc_gnttab_close"
type t = Lwt_bytes.t
external map_grant_ref : handle -> int32 -> int32 -> int -> t
  = "stub_xc_gnttab_map_grant_ref"
external unmap : handle -> t -> unit = "stub_xc_gnttab_unmap"
external strblit : string -> int -> t -> int -> int -> unit
  = "stub_xc_gnttab_string_blit"
external ringblit : t -> int -> string -> int -> int -> unit
  = "stub_xc_gnttab_ring_blit"

val with_ref :
  handle -> int32 -> int32 -> int -> (t -> 'a Lwt.t) -> 'a Lwt.t

val read : t -> int -> int -> string
val write : t -> int -> string -> unit
