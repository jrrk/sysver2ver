all: vxml vxmlmain

vxml: vxml.mli vxml.ml
	ocamlmktop -g -I `ocamlfind query xml-light` unix.cma nums.cma xml-light.cma vxml.mli vxml.ml -o $@

test:
	./vxml

header_check:
	ocamlc -i -I `ocamlfind query xml-light` vxml.ml >junk.mli
	diff -w junk.mli vxml.mli

vxmlmain: vxml.mli vxml.ml main.ml
	ocamlc -g -I `ocamlfind query xml-light` unix.cma nums.cma xml-light.cma vxml.mli vxml.ml main.ml -o $@

testbench:
	verilator --cc --exe -Wno-width -Wno-multidriven -Wno-caseincomplete -trace picorv32_wrapper_opt.v example/testbench.cc

testbench.vvp: example/testbench.v picorv32_wrapper_opt.v
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA example/testbench.v ../picorv32/picorv32.v picorv32_wrapper_opt.v

B = example/testbench_dual.v
G = example/testbench_golden.v

V =  example/picorv32_wrapper.v example/axi4_memory.v example/picorv32_axi_adapter.v example/picorv32_axi.v example/picorv32_pcpi_div.v example/picorv32_pcpi_mul.v example/picorv32.v
S = $B $V
T = axi4_memory_opt.v picorv32_opt.v picorv32_axi_opt.v picorv32_axi_adapter_opt.v picorv32_pcpi_div_opt.v picorv32_pcpi_mul_opt.v
N = axi4_memory.v picorv32.v picorv32_axi.v picorv32_axi_adapter.v picorv32_pcpi_div.v picorv32_pcpi_mul.v
P = picorv32_axi_opt.v picorv32_wrapper_mixed.v picorv32_opt.v 
M = axi4_memory_opt.v picorv32_axi_adapter_opt.v picorv32_axi_opt.v picorv32_pcpi_div_opt.v picorv32_pcpi_mul_opt.v picorv32_opt.v ref_opt/picorv32_axi_mixed.v ref_opt/picorv32_mixed.v ref_opt/picorv32_wrapper_mixed.v # picorv32_wrapper_opt.v
X = picorv32_wrapper_opt.v
Y = picorv32_wrapper.v

mixed.vvp: $S $M
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA -DDEBUGREGS $S $M

trans.vvp: $S $T $P
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA -DDEBUGREGS $S $T $P

mixed.vcs: $S $M
	vcs -full64 -sverilog -debug_access+all +lint=TFIPC-L -o $@ -DCOMPRESSED_ISA $S $M

golden.vvp: $G $V
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA $G $V

target.vvp: $G $T $X
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA -DWRAPPER=picorv32_wrapper_opt $G $T $X

nonopt.vvp: $G $N $Y
	iverilog -g2005-sv -o $@ -DCOMPRESSED_ISA -DWRAPPER=picorv32_wrapper $G $N $Y

$T: obj_dir/Vpicorv32_wrapper.xml
	env VXML_SEPARATE=1 ./vxmlmain $<

obj_dir/Vpicorv32_wrapper.xml: $V
	verilator --xml-only -Wno-fatal -DCOMPRESSED_ISA -DDEBUGREGS $V

picorv32_wrapper_mixed.v: picorv32_wrapper_opt.v
	sed -e 's=\(\ picorv32_wrapper\)_opt=\1_mixed\ =' $< > $@

clean:
	rm -rf old vout vopt voutopt edited debug debugopt elem *.v *.tcl *.elem *.names *.debug *.fss a.out traverse.cell \
	.restartSimSession.tcl.old .swave_codegen_notneeded csrc inter.vpd simv simv.daidir swave_compile_done ucli.key \
	FM_WORK FM_WORK1 formality1.lck  formality1.log  formality2.log  formality.lck   formality.log fm_shell_command.lck fm_shell_command1.lck
