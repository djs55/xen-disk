

let map domid gntref =
	let xg = Gnttab.interface_open () in
	let b = Gnttab.map_grant_ref xg domid gntref 3 in
	for i=0 to 4095 do 
		Printf.printf "%c" (Char.chr b.{0});
	done;
	Gnttab.interface_close xg

		
let _ =
	let domid = Int32.of_string Sys.argv.(1) in
	let gntref = Int32.of_string Sys.argv.(2) in
	map domid gntref
