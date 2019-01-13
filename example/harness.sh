make ./vxmlmain
rm -rf names elem debug vout voutopt vout2 voutopt2 vout3 voutopt3 *.v *.tcl *.debug
mkdir names elem debug vout voutopt vout2 voutopt2 vout3 voutopt3
./vxmlmain ../ariane/obj_dir/Variane_testharness.xml
mv *.elem elem
mv *.names names
mv *.debug debug
mv *opt.v voutopt
mv *.v vout
verilator --Mdir vout2 --xml-only -Wno-width -Wno-fatal vout/*.v --top-module ariane_testharness
./vxmlmain vout2/Variane_testharness.xml
mv *.elem elem
mv *.names names
mv *.debug debug
mv *opt.v voutopt2
mv *.v vout2
verilator --Mdir voutopt2 --xml-only -Wno-width -Wno-fatal voutopt/*opt.v --top-module ariane_testharness_opt
./vxmlmain vout2/Variane_testharness.xml
mv *.elem elem
mv *.names names
mv *.debug debug
mv *opt.v voutopt3
mv *.v vout3
meld vout vout3
