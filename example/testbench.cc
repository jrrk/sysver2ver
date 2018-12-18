#include "Vpicorv32_wrapper_opt_translate.h"
#include "verilated_vcd_c.h"

int main(int argc, char **argv, char **env)
{
	Verilated::commandArgs(argc, argv);
	Verilated::traceEverOn(true);
	Vpicorv32_wrapper_opt_translate* top = new Vpicorv32_wrapper_opt_translate;

	VerilatedVcdC* tfp = new VerilatedVcdC;
	top->trace (tfp, 99);
        tfp->open ("testbench.vcd");
	top->clk = 0;
	int t = 0;
	while (!Verilated::gotFinish()) {
		if (t > 200)
			top->resetn = 1;
		top->clk = !top->clk;
		top->eval();
		tfp->dump (t);
		t += 5;
	}
	tfp->close();
	delete top;
	exit(0);
}

