eval `opam config env`
eval `opam config env`
make vxmlmain
verilator --xml-only ../ivtest/blif/blif01a.v 
./vxmlmain obj_dir/Vblif01a.xml 
./addN_fm.tcl
verilator --xml-only ../ivtest/blif/blif01b.v 
./vxmlmain obj_dir/Vblif01b.xml 
./test_logic_fm.tcl
verilator --xml-only ../ivtest/blif/blif01c.v 
./vxmlmain obj_dir/Vblif01c.xml 
./addN_fm.tcl
verilator --xml-only ../ivtest/blif/blif01d.v 
./vxmlmain obj_dir/Vblif01d.xml 
./subN_fm.tcl
verilator --xml-only ../ivtest/blif/blif01e.v 
./vxmlmain obj_dir/Vblif01e.xml 
./cmpN_fm.tcl
verilator --xml-only ../ivtest/blif/blif01f.v 
./vxmlmain obj_dir/Vblif01f.xml 
./muxN_fm.tcl
verilator --xml-only ../ivtest/blif/blif01g.v 
./vxmlmain obj_dir/Vblif01g.xml 
./test_logic_fm.tcl
verilator --xml-only ../ivtest/blif/blif01h.v 
./vxmlmain obj_dir/Vblif01h.xml 
./test_mux_fm.tcl
verilator --xml-only ../ivtest/blif/blif01i.v 
./vxmlmain obj_dir/Vblif01i.xml 
./ivtest_fm.tcl
verilator --xml-only ../ivtest/fpga_tests/ge8.v 
./vxmlmain obj_dir/Vge8.xml 
./ge8_fm.tcl
verilator --xml-only ../ivtest/fpga_tests/onehot16.v 
./vxmlmain obj_dir/Vonehot16.xml 
./onehot16_fm.tcl
