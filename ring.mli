(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Shared ring handling to communicate with other Xen domains *)

(** Abstract type for a shared ring *)
type sring

val init_back :
  xg:Gnttab.handle ->
  domid:int32 -> gref:int32 -> idx_size:int -> name:string -> sring
val destroy_back :
	xg:Gnttab.handle -> sring -> unit

external sring_rsp_prod : sring -> int = "caml_sring_rsp_prod"
external sring_req_prod : sring -> int = "caml_sring_req_prod"
external sring_req_event : sring -> int = "caml_sring_req_event"
external sring_rsp_event : sring -> int = "caml_sring_rsp_event"
external sring_push_requests : sring -> int -> unit
  = "caml_sring_push_requests"
external sring_push_responses : sring -> int -> unit
  = "caml_sring_push_responses"
external sring_set_rsp_event : sring -> int -> unit
  = "caml_sring_set_rsp_event"
external sring_set_req_event : sring -> int -> unit
  = "caml_sring_set_req_event"
val nr_ents : sring -> int
val slot : sring -> int -> Gnttab.t * int * int


(** The front-end of the shared ring, which issues requests and reads
    responses from the remote domain. 
  *)
module Front : sig

  (** 'a is the response type, and 'b is the request id type (e.g. int or int64) *)
  type ('a,'b) t

  (** Given a shared ring, initialise it for this front-end module
    * @param sring Shared ring to attach this front end to
    * @return front end ring value
    *)
  val init : sring:sring -> ('a,'b) t

  (** Retrieve the request/response slot at the specified index as
    * a Bitstring.
    * @param idx Index to retrieve, should be less than nr_ents
    *)
  val slot : ('a,'b) t -> int -> Gnttab.t * int * int

  (** Retrieve number of slots in the shared ring *)
  val nr_ents : ('a,'b) t -> int

  (** Retrieve the number of free request slots remaining *)
  val get_free_requests : ('a,'b) t -> int

  (** Advance the request producer and return the latest slot id *)
  val next_req_id: ('a,'b) t -> int

  (** Read all the outstanding responses from the remote domain,
    * calling {[fn]} on them, and updating the response
    * consumer pointer after each individual slot has been processed.
    *
    * This is the low-level function which is only used if some
    * sort of batching of requests is being performed, and normally
    * you should use the flow-controlled {[poll]} that will ack
    * the responses and wake up any sleeping threads that were
    * waiting for that particular response.
    *)
  val ack_responses : ('a,'b) t -> (Gnttab.t * int * int -> unit) -> unit

  (** Update the shared request producer *)
  val push_requests : ('a,'b) t -> unit

  (** Update the shared request producer, and also check to see
      if an event notification is required to wake up the remote
      domain.
      @return true if an event channel notification is required
    *)
  val push_requests_and_check_notify : ('a,'b) t -> bool

  (** Given a function {[fn]} which writes to a slot and returns
      the request id, this will wait for a free request slot,
      write the request, and return with the response when it
      is available.
      @param fn Function that writes to a request slot and returns the request id
      @return Thread which returns the response value to the input request
    *)
  val push_request_and_wait : ('a,'b) t -> (Gnttab.t * int * int -> 'b) -> 'a Lwt.t

  (** Poll the ring for responses, and wake up any threads that are
      sleeping (as a result of calling {[push_request_and_wait]}).
    *)
  val poll : ('a,'b) t -> (Gnttab.t * int * int -> ('b * 'a)) -> unit
end

module Back : sig
  (** 'a is the response type, and 'b is the request id type (e.g. int or int64) *)
  type t

  (** Given a shared ring, initialise it for this backend module
    * @param sring Shared ring to attach this backend to
    * @return backend ring value
    *)
  val init : sring:sring -> t

  (** Retrieve the request/response slot at the specified index as
    * a Bitstring.
    * @param idx Index to retrieve, should be less than nr_ents
    *)
  val slot :  t -> int -> Gnttab.t * int * int

  (** Retrieve number of slots in the shared ring *)
  val nr_ents :  t -> int

  (** Advance the response producer and return the latest slot id *)
  val next_res_idx:  t -> int

  val next_slot: t -> Gnttab.t * int * int
  (** Update the shared response producer *)
  val push_responses :  t -> unit

  (** Update the shared response producer, and also check to see
      if an event notification is required to wake up the remote
      domain.
      @return true if an event channel notification is required
    *)
  val push_responses_and_check_notify :  t -> bool

  val more_to_do : t -> bool

  val write_response : t -> string -> bool * bool

  val service_thread : t -> int -> (Gnttab.t * int * int -> unit) -> unit Lwt.t
end

