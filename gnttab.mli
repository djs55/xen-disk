type handle
external interface_open : unit -> handle = "stub_xc_gnttab_open"
external interface_close : handle -> unit = "stub_xc_gnttab_close"
type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external map_grant_ref : handle -> int32 -> int32 -> int -> t
  = "stub_xc_gnttab_map_grant_ref"
external unmap : handle -> t -> unit = "stub_xc_gnttab_unmap"
