all: vxml vxmlmain

vxml: vxml.mli vxml.ml
	ocamlmktop -g -I `ocamlfind query xml-light` unix.cma xml-light.cma vxml.mli vxml.ml -o $@

test:
	./vxml

header_check:
	ocamlc -i -I `ocamlfind query xml-light` vxml.ml >junk.mli
	diff -w junk.mli vxml.mli

vxmlmain: vxml.mli vxml.ml main.ml
	ocamlc -g -I `ocamlfind query xml-light` unix.cma xml-light.cma vxml.mli vxml.ml main.ml -o $@

testbench:
	verilator --cc --exe -Wno-width -Wno-multidriven -Wno-caseincomplete -trace picorv32_wrapper_opt_translate.v example/testbench.cc

testbench.vvp:
	iverilog -g2005-sv -o $@ picorv32_wrapper_opt_translate.v

combined.v:
	cat axi4_memory_opt_translate.v picorv32__pi2_opt_translate.v picorv32_axi__pi1_opt_translate.v  picorv32_axi_adapter_opt_translate.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v picorv32_wrapper_opt_translate.v > $@
