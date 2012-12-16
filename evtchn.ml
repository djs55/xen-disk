type t

type domid = int

type port = int

external init: unit -> t = "stub_evtchn_init"

let pending _ = assert false

let unmask _ _ = assert false

let notify _ _ = assert false

let bind_interdomain _ _ _ = assert false
