make ./vxmlmain
rm -rf debug vout voutopt vout2 voutopt2 *.v *.tcl *.debug
mkdir debug vout voutopt vout2 voutopt2
./vxmlmain ../ariane/obj_dir/Variane_testharness.xml
mv *.debug debug
mv *opt.v voutopt
mv *.v vout
verilator --Mdir vout2 --xml-only -Wno-width -Wno-fatal vout/*.v --top-module ariane_testharness
./vxmlmain vout2/Variane_testharness.xml
mv *opt.v voutopt2
mv *.v vout2
verilator --Mdir voutopt2 --xml-only -Wno-width -Wno-fatal voutopt/*opt.v --top-module ariane_testharness_opt
./vxmlmain vout2/Variane_testharness.xml
meld vout vout2
