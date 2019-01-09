make ./vxmlmain
rm -rf vout voutopt vout2 voutopt2 *.v *.tcl *.debug
mkdir vout voutopt vout2 voutopt2
verilator --Mdir vout --xml-only example/interface1.v
./vxmlmain vout/Vinterface1.xml
mv *opt.v voutopt
mv *.v vout
verilator --Mdir vout2 --xml-only vout/*.v
./vxmlmain vout2/Vinterface1.xml
mv *opt.v voutopt2
mv *.v vout2
meld vout vout2
