open Gnttab

type t = contents

external strblit: string -> int -> t -> int -> int -> unit = "stub_xc_gnttab_string_blit"
external ringblit: t -> int -> string -> int -> int -> unit = "stub_xc_gnttab_ring_blit"

let with_ref handle domid reference prot fn =
	match map handle { domid; reference } prot with
	| None -> Lwt.fail (Failure "map failed")
    | Some t ->
	try_lwt
		lwt result = fn (contents t) in
		unmap_exn handle t;
		Lwt.return result
	with e ->
		unmap_exn handle t;
		raise e

let read t off len =
	let str = String.create len in
	ringblit t off str 0 len;
	str

let write t off str =
	strblit str 0 t off (String.length str)

