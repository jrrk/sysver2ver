make ./vxmlmain
rm -rf vout voutopt vout2 voutopt2 *.v *.tcl *.debug
mkdir vout voutopt vout2 voutopt2
./vxmlmain ../ariane/obj_dir/Variane_testharness.xml
mv *.debug debug
mv *opt.v voutopt
mv *.v vout
mv vout/axi_node_intf_wrap__pi4.v edited/axi_node_intf_wrap__pi4_edited.v
patch edited/axi_node_intf_wrap__pi4_edited.v < example/patch1
verilator --Mdir vout2 --xml-only vout/*.v edited/axi_node_intf_wrap__pi4_edited.v --top-module ariane_testharness
./vxmlmain vout2/Variane_testharness.xml
mv *opt.v voutopt2
mv *.v vout2
meld vout vout2
