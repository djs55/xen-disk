
default : xs_jonstest

testevtchn_rx : activations.cmx testevtchn_rx.cmx 
	ocamlfind ocamlopt -o testevtchn_rx -package xeneventchn,lwt,lwt.unix -linkpkg $^

xs_jonstest : gnttab.cmxa vhd.cmx activations.cmx ring.cmx blkif.cmx xs_jonstest.cmx
	ocamlfind ocamlopt -o xs_jonstest -g -package xeneventchn,bigarray,lwt,lwt.unix,bitstring -linkpkg -I ../ocaml-xenstore ../ocaml-xenstore/xs.cmxa $^

gnttab.cmxa : gnttab.cmx ring_stubs.o gnttab_stubs.o gnttab.cmi
	ocamlmklib -g -o gnttab ring_stubs.o gnttab_stubs.o -L. gnttab.cmx 

%.o : %.c
	gcc -c -fPIC -g -o $@ $<

%.cmx : %.ml 
	ocamlfind ocamlopt -package xeneventchn,bitstring.syntax,lwt,lwt.syntax -I ../ocaml-xenstore -syntax camlp4o -g -annot -c $<

%.cmi : %.mli
	ocamlfind ocamlopt -package xeneventchn,bitstring.syntax,lwt,lwt.syntax -I ../ocaml-xenstore -syntax camlp4o -c $^

.PHONY : clean
clean :
	rm -f *.cmx *.cmo *.cmi *.o *.cmxa *.a *.so *.cma test

ring.cmx : ring.cmi
gnttab.cmx : gnttab.cmi
