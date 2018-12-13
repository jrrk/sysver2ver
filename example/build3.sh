export V=/local/scratch/jrrk2/verilator/test_regress/t/t_sv_cpu_code
verilator --xml-only -I$V $V/chip.sv $V/timescale.sv $V/genbus_if.sv $V/ac_ana.sv $V/ac_dig.sv $V/ac.sv $V/adrdec.sv $V/cpu.sv $V/pad_gnd.sv $V/pad_gpio.sv $V/pads_h.sv $V/pads_if.sv $V/pads.sv $V/pad_vdd.sv $V/pinout_h.sv $V/ports_h.sv $V/ports.sv $V/program_h.sv 
./vxmlmain obj_dir/Vchip.xml --top-level chip
emacs --batch chip_opt_translate.v -f verilog-batch-indent
