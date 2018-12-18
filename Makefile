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

testbench.vvp: example/testbench.v picorv32_wrapper_opt_translate.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA example/testbench.v ../picorv32/picorv32.v picorv32_wrapper_opt_translate.v

S = example/testbench.v example/axi4_memory.v example/picorv32_axi_adapter.v example/picorv32_axi.v example/picorv32_pcpi_div.v example/picorv32_pcpi_fast_mul.v example/picorv32_pcpi_mul.v example/picorv32_regs.v example/picorv32.v example/picorv32_wb.v example/picorv32_wrapper.v

split.vvp: $S
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA $S picorv32_wrapper_opt_translate_patched.v

patched.vvp: example/testbench.v picorv32_wrapper_opt_translate_patched.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA example/testbench.v ../picorv32/picorv32.v picorv32_wrapper_opt_translate_patched.v

reference.vvp: example/testbench.v picorv32_wrapper_opt_translate.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA ../picorv32/testbench.v ../picorv32/picorv32.v

combined.v:
	env VXML_SEPARATE=1 ./vxmlmain ../picorv32/xml_verilator_dir/Vpicorv32_wrapper.xml
	cat axi4_memory_opt_translate.v picorv32__pi2_opt_translate.v picorv32_axi__pi1_opt_translate.v  picorv32_axi_adapter_opt_translate.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v picorv32_wrapper_opt_translate.v > $@
