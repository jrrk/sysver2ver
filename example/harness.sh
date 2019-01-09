make ./vxmlmain
rm -rf vout voutopt vout2 voutopt2 *.v *.tcl *.debug
mkdir vout voutopt vout2 voutopt2
./vxmlmain ../ariane/obj_dir/Variane_testharness.xml
mv *.debug debug
mv *opt.v voutopt
mv *.v vout
verilator --Mdir voutopt2 --xml-only voutopt/*opt.v --top-module ariane_testharness_opt
verilator --Mdir vout2 --xml-only vout/*.v --top-module ariane_testharness
./vxmlmain vout2/Variane_testharness.xml
mv *opt.v voutopt2
mv *.v vout2
meld vout vout2
