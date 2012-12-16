type t

type domid = int

type port = int

val init: unit -> t

val pending: t -> port

val unmask: t -> port -> unit

val notify: t -> port -> unit

val bind_interdomain: t -> domid -> port -> int

val fd_of: t -> Unix.file_descr
