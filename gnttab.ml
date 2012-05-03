
open Bigarray

type handle
external interface_open: unit -> handle = "stub_xc_gnttab_open"
external interface_close: handle -> unit = "stub_xc_gnttab_close"
		
type t = Lwt_bytes.t
		
external map_grant_ref: handle -> int32 -> int32 -> int -> t = "stub_xc_gnttab_map_grant_ref"
external unmap: handle -> t -> unit = "stub_xc_gnttab_unmap"
		
external strblit: string -> int -> t -> int -> int -> unit = "stub_xc_gnttab_string_blit"
external ringblit: t -> int -> string -> int -> int -> unit = "stub_xc_gnttab_ring_blit"

let with_ref handle domid gref prot fn =
	let t = map_grant_ref handle domid gref prot in
	try_lwt
		lwt result = fn t in
        unmap handle t;
		Lwt.return result
	with e ->
		unmap handle t;
		raise e

let read t off len =
	let str = String.create len in
	ringblit t off str 0 len;
	str

let write t off str =
	strblit str 0 t off (String.length str)
