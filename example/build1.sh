verilator \
--xml-only -Wno-fatal \
+define+FPGA_TARGET_XILINX \
../ariane-vcs-regression/ariane/src/debug/dm_pkg.sv \
../ariane-vcs-regression/ariane/include/riscv_pkg.sv \
../ariane-vcs-regression/ariane/include/ariane_pkg.sv \
../ariane-vcs-regression/ariane/src/axi/src/axi_pkg.sv \
../ariane-vcs-regression/ariane/include/axi_intf.sv \
../ariane-vcs-regression/ariane/include/std_cache_pkg.sv \
../ariane-vcs-regression/ariane/src/debug/dmi_cdc.sv \
../ariane-vcs-regression/ariane/src/debug/dmi_jtag.sv \
../ariane-vcs-regression/ariane/src/debug/dmi_jtag_tap.sv \
../ariane-vcs-regression/ariane/src/debug/jtag_xilinx_fixed.sv \
../ariane-vcs-regression/ariane/src/debug/dm_mem.sv \
../ariane-vcs-regression/ariane/src/debug/dm_sba.sv \
../ariane-vcs-regression/ariane/src/debug/dm_top.sv \
../ariane-vcs-regression/ariane/src/axi_adapter.sv \
../ariane-vcs-regression/ariane/src/debug/debug_rom/debug_rom.sv \
../ariane-vcs-regression/ariane/src/common_cells/src/cdc_2phase.sv \
../ariane-vcs-regression/ariane/src/debug/dm_csrs.sv \
../ariane-vcs-regression/ariane/src/common_cells/src/rstgen_bypass.sv \
../ariane-vcs-regression/ariane/src/axi_mem_if/src/axi2mem.sv \
../ariane-vcs-regression/ariane/src/common_cells/src/fifo_v2.sv \
/local/scratch/jrrk2/ariane-vcs-experiment2/tb/dmi_testharness.sv \
/local/scratch/jrrk2/ariane-vcs-experiment2/tb/dmi_testharness_0.sv \
/local/scratch/jrrk2/ariane-vcs-experiment2/fpga/slave_adapter.sv \
--top-module dmi_testharness_0
./vxmlmain obj_dir/Vdmi_testharness_0.xml