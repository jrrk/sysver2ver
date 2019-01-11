make ./vxmlmain
rm -rf elem debug vout voutopt vout2 voutopt2 vout3 voutopt3 *.v *.tcl *.debug
mkdir elem debug vout voutopt vout2 voutopt2 vout3 voutopt3
env VXML_OPTTREE=0 ./vxmlmain ../ariane/obj_dir/Variane_testharness.xml
mv *.elem elem
mv *.debug debug
mv *.v vout
verilator --Mdir vout2 --xml-only -Wno-width -Wno-fatal vout/*.v --top-module ariane_testharness
