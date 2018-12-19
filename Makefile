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

R = ref_opt/axi4_memory.v ref_opt/picorv32_axi_adapter.v ref_opt/picorv32_axi.v ref_opt/picorv32_pcpi_div.v ref_opt/picorv32_pcpi_mul.v ref_opt/picorv32.v ref_opt/picorv32_wrapper.v
S = example/testbench.v example/axi4_memory.v example/picorv32_axi_adapter.v example/picorv32_axi.v example/picorv32_pcpi_div.v example/picorv32_pcpi_mul.v example/picorv32.v example/picorv32_wrapper.v
T = axi4_memory_opt_translate.v picorv32__pi2_opt_translate.v picorv32_axi__pi1_opt_translate.v  picorv32_axi_adapter_opt_translate.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v picorv32_wrapper_opt_translate.v
M = axi4_memory_opt_translate.v picorv32_axi_adapter_opt_translate.v picorv32_axi_opt_translate_edited.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v picorv32_opt_translate_edited.v ref_opt/picorv32_axi_mixed.v ref_opt/picorv32_mixed.v ref_opt/picorv32_wrapper_mixed.v # picorv32_wrapper_opt_translate_edited.v

mixed.vvp: $S $M
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA $S $M

mixed.vcs: $S $M
	vcs -full64 -sverilog -gui -debug_access+all -o $@ -DCOMPRESSED_ISA $S $M

split.vvp: $S $R
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA $S $R

patched.vvp: example/testbench.v picorv32_wrapper_opt_translate_patched.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA example/testbench.v ../picorv32/picorv32.v picorv32_wrapper_opt_translate_patched.v

reference.vvp: example/testbench.v picorv32_wrapper_opt_translate.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA ../picorv32/testbench.v ../picorv32/picorv32.v

combined.v:
	env VXML_SEPARATE=1 ./vxmlmain ../picorv32/xml_verilator_dir/Vpicorv32_wrapper.xml
	cat axi4_memory_opt_translate.v picorv32__pi2_opt_translate.v picorv32_axi__pi1_opt_translate.v picorv32_axi_adapter_opt_translate.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v picorv32_wrapper_opt_translate.v > $@

picorv32_axi_opt_translate_edited.v: picorv32_axi__pi1_opt_translate.v
	cp $< $@

picorv32_opt_translate_edited.v: picorv32__pi2_opt_translate.v
	cp $< $@

ref_opt/picorv32_axi_mixed.v: example/picorv32_axi.v
	sed -e 's=\(\ picorv32_axi\)\ =\1_mixed\ =' -e 's=\(picorv32\)\ =\1_mixed\ =' $< > $@

ref_opt/picorv32_axi.v: example/picorv32_axi.v
	sed -e 's=\(\ picorv32_axi\)\ =\1_opt\ =' $< > $@

ref_opt/picorv32_mixed.v: example/picorv32.v
	sed -e 's=\(\ picorv32\)\ =\1_mixed\ =' -e 's=\(picorv32_pcpi_mul\)\ =\1_opt\ =' $< > $@

ref_opt/picorv32_wrapper_mixed.v:  example/picorv32_wrapper.v
	sed -e 's=\(\ picorv32_wrapper\)\ =\1_mixed\ =' -e 's=\(picorv32_axi\)\ =\1_mixed\ =' $< > $@
