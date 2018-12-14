rm -rf obj_dir
verilator \
--xml-only -Wno-fatal \
+define+FPGA_TARGET_XILINX \
../ariane-vcs-regression/ariane/src/debug/dm_pkg.sv \
../ariane-vcs-regression/ariane/include/riscv_pkg.sv \
../ariane-vcs-regression/ariane/include/ariane_pkg.sv \
../ariane-vcs-regression/ariane/src/axi/src/axi_pkg.sv \
../ariane-vcs-regression/ariane/include/axi_intf.sv \
../ariane-vcs-regression/ariane/include/std_cache_pkg.sv \
../ariane-vcs-regression/ariane/src/axi_mem_if/src/axi2mem.sv \
../ariane-vcs-regression/ariane/src/axi_adapter.sv \
example/master.sv --top-module master
make
./vxmlmain obj_dir/Vmaster.xml
verilator --xml-only -Wno-fatal master_opt_translate.v --top-module master_opt
./vxmlmain obj_dir/Vmaster_opt.xml
