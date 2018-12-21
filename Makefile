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

B = example/testbench.v
V =  example/picorv32_wrapper.v example/axi4_memory.v example/picorv32_axi_adapter.v example/picorv32_axi.v example/picorv32_pcpi_div.v example/picorv32_pcpi_mul.v example/picorv32.v
S = $B $V
T = axi4_memory_opt_translate.v picorv32__pi2_opt_translate.v picorv32_axi__pi1_opt_translate.v  picorv32_axi_adapter_opt_translate.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v # picorv32_wrapper_opt_translate.v
P = picorv32_axi_opt_translate.v picorv32_wrapper_mixed.v picorv32_opt_translate.v 
M = axi4_memory_opt_translate.v picorv32_axi_adapter_opt_translate.v picorv32_axi_opt_translate.v picorv32_pcpi_div_opt_translate.v picorv32_pcpi_mul_opt_translate.v picorv32_opt_translate.v ref_opt/picorv32_axi_mixed.v ref_opt/picorv32_mixed.v ref_opt/picorv32_wrapper_mixed.v # picorv32_wrapper_opt_translate.v

mixed.vvp: $S $M
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA $S $M

trans.vvp: $S $T $P
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA $S $T $P

mixed.vcs: $S $M
	vcs -full64 -sverilog -debug_access+all +lint=TFIPC-L -o $@ -DCOMPRESSED_ISA $S $M

reference.vvp: example/testbench.v picorv32_wrapper_opt_translate.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA ../picorv32/testbench.v ../picorv32/picorv32.v

$T: obj_dir/Vpicorv32_wrapper.xml
	env VXML_SEPARATE=1 ./vxmlmain $<

obj_dir/Vpicorv32_wrapper.xml: $V
	verilator --xml-only -Wno-fatal -DCOMPRESSED_ISA $V

picorv32_axi_opt_translate.v: picorv32_axi__pi1_opt_translate.v
	sed -e 's=\(\ picorv32_axi\)__pi1\(_opt\)=\1\2\ =' -e 's=\(\picorv32\)__pi2\(_opt\)=\1\2=' $< > $@

picorv32_opt_translate.v: picorv32__pi2_opt_translate.v
	sed -e 's=\(\ picorv32\)__pi2\(_opt\)=\1\2\ =' $< > $@

ref_opt/picorv32_axi_mixed.v: example/picorv32_axi.v
	sed -e 's=\(\ picorv32_axi\)\ =\1_mixed\ =' -e 's=\(picorv32\)\ =\1_mixed\ =' $< > $@

ref_opt/picorv32_mixed.v: example/picorv32.v
	sed -e 's=\(\ picorv32\)\ =\1_mixed\ =' -e 's=\(picorv32_pcpi_mul\)\ =\1_opt\ =' $< > $@

ref_opt/picorv32_wrapper_mixed.v:  example/picorv32_wrapper.v
	sed -e 's=\(\ picorv32_wrapper\)\ =\1_mixed\ =' -e 's=\(picorv32_axi\)\ =\1_mixed\ =' $< > $@

picorv32_wrapper_mixed.v: picorv32_wrapper_opt_translate.v
	sed -e 's=\(\ picorv32_wrapper\)_opt=\1_mixed\ =' $< > $@
