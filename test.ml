

let map domid gntref =
	let xg = Gnttab.interface_open () in
	let b = Gnttab.map_grant_ref xg domid gntref 3 in
    Printf.printf "%d\n" b.{0};
	Gnttab.interface_close xg

		
let _ =
	let domid = Int32.of_string Sys.argv.(1) in
	let gntref = Int32.of_string Sys.argv.(2) in
	map domid gntref
