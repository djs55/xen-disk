
let xe = Xeneventchn.init ()
let nr_events = 1024
let event_cb = Array.init nr_events (fun _ -> Lwt_sequence.create ())

let wait port =
  let th, u = Lwt.task () in
  let node = Lwt_sequence.add_r u event_cb.(port) in
  Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
  th

let wake port =
	Lwt_sequence.iter_node_l (fun node ->
		let u = Lwt_sequence.get node in
		Lwt_sequence.remove node;
		Lwt.wakeup u ()
    ) event_cb.(port)

(* Go through the event mask and activate any events, potentially spawning
   new threads *)
let run () =
	let fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true (Xeneventchn.fd xe) in
	let rec inner () =
		lwt () = Lwt_unix.wait_read fd in
	    let port = Xeneventchn.pending xe in
		wake port;
		Xeneventchn.unmask xe port;
        inner ()
   in inner ()
