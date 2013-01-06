
let main () =
	Activations.run ();
	let domid = int_of_string Sys.argv.(1) in 
	let port = int_of_string Sys.argv.(2) in
	let localp = Xeneventchn.bind_interdomain Activations.xe domid port in
	lwt () = Activations.wait port in
    lwt _ = Lwt_io.write Lwt_io.stdout "Got it" in
    Lwt.return ()

let _ =
	Lwt_main.run (main ())
