
default : test

test : gnttab.cmxa test.cmx
	ocamlfind ocamlopt -o test -package xenctrl,bigarray,lwt -linkpkg $^

gnttab.cmxa : gnttab.ml gnttab_stubs.o gnttab.cmi
	ocamlmklib -o gnttab gnttab_stubs.o -L. gnttab.ml gnttab.mli

%.o : %.c
	gcc -c -fPIC -o $@ $<

%.cmx : %.ml 
	ocamlfind ocamlopt -package bitstring.syntax,lwt -syntax camlp4o -c $<

%.cmi : %.mli
	ocamlfind ocamlopt -package bitstring.syntax,lwt -syntax camlp4o -c $^

.PHONY : clean
clean :
	rm -f *.cmx *.cmo *.cmi *.o *.cmxa *.a *.so *.cma test

ring.cmx : ring.cmi
gnttab.cmx : gnttab.cmi