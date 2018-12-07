vxml: vxml.mli vxml.ml
	ocamlmktop -I `ocamlfind query xml-light` xml-light.cma vxml.mli vxml.ml -o $@

test:
	./vxml

