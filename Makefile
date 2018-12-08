vxml: vxml.mli vxml.ml
	ocamlmktop -I `ocamlfind query xml-light` unix.cma xml-light.cma vxml.mli vxml.ml -o $@

test:
	./vxml

header_check:
	ocamlc -i -I `ocamlfind query xml-light` vxml.ml >junk.mli
	diff -w junk.mli vxml.mli

vxmlmain: vxml.mli vxml.ml main.ml
	ocamlc -I `ocamlfind query xml-light` unix.cma xml-light.cma vxml.mli vxml.ml main.ml -o $@

