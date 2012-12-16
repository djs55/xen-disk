
default : xs_jonstest

testevtchn_rx : evtchn.cmx activations.cmx testevtchn_rx.cmx 
	ocamlfind ocamlopt -o testevtchn_rx -package lwt,lwt.unix -linkpkg $^

xs_jonstest : gnttab.cmxa vhd.cmx activations.cmx ring.cmx blkif.cmx xs_jonstest.cmx
	ocamlfind ocamlopt -o xs_jonstest -g -package bigarray,lwt,lwt.unix,bitstring -linkpkg -I ../ocaml-xenstore ../ocaml-xenstore/xs.cmxa $^

gnttab.cmxa : evtchn.cmx gnttab.cmx ring_stubs.o gnttab_stubs.o gnttab.cmi
	ocamlmklib -g -o gnttab ring_stubs.o gnttab_stubs.o -L. gnttab.cmx evtchn.cms

%.o : %.c
	gcc -c -fPIC -g -o $@ $<

%.cmx : %.ml 
	ocamlfind ocamlopt -package bitstring.syntax,lwt,lwt.syntax -I ../ocaml-xenstore -syntax camlp4o -g -annot -c $<

%.cmi : %.mli
	ocamlfind ocamlopt -package bitstring.syntax,lwt,lwt.syntax -I ../ocaml-xenstore -syntax camlp4o -c $^

.PHONY : clean
clean :
	rm -f *.cmx *.cmo *.cmi *.o *.cmxa *.a *.so *.cma test

ring.cmx : ring.cmi
gnttab.cmx : gnttab.cmi
