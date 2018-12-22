// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

`timescale 1 ns / 1 ps

`ifndef VERILATOR
module testbench #(
	parameter AXI_TEST = 0,
	parameter VERBOSE = 0
);
	reg clk = 1;
	reg resetn = 0;
	wire trap_opt;

	always #5 clk = ~clk;

	initial begin
		repeat (100) @(posedge clk);
		resetn <= 1;
	end

	initial begin
		if ($test$plusargs("vcd")) begin
			$dumpfile("testbench.vcd");
			$dumpvars(0, testbench);
		end
		repeat (1000000) @(posedge clk);
		$display("TIMEOUT");
		$finish;
	end

	wire trace_valid_opt;
	wire [35:0] trace_data_opt;
	integer trace_file;

	initial begin
		if ($test$plusargs("trace")) begin
			trace_file = $fopen("testbench.trace", "w");
			repeat (10) @(posedge clk);
			while (!trap_opt) begin
				@(posedge clk);
				if (trace_valid_opt)
					$fwrite(trace_file, "%x\n", trace_data_opt);
			end
			$fclose(trace_file);
			$display("Finished writing testbench.trace.");
		end
	end

`ifdef DEBUGREGS   
	wire [31:0] 	  dbg_reg_x0_opt;
	wire [31:0] 	  dbg_reg_x1_opt;
	wire [31:0] 	  dbg_reg_x2_opt;
	wire [31:0] 	  dbg_reg_x3_opt;
	wire [31:0] 	  dbg_reg_x4_opt;
	wire [31:0] 	  dbg_reg_x5_opt;
	wire [31:0] 	  dbg_reg_x6_opt;
	wire [31:0] 	  dbg_reg_x7_opt;
	wire [31:0] 	  dbg_reg_x8_opt;
	wire [31:0] 	  dbg_reg_x9_opt;
	wire [31:0] 	  dbg_reg_x10_opt;
	wire [31:0] 	  dbg_reg_x11_opt;
	wire [31:0] 	  dbg_reg_x12_opt;
	wire [31:0] 	  dbg_reg_x13_opt;
	wire [31:0] 	  dbg_reg_x14_opt;
	wire [31:0] 	  dbg_reg_x15_opt;
	wire [31:0] 	  dbg_reg_x16_opt;
	wire [31:0] 	  dbg_reg_x17_opt;
	wire [31:0] 	  dbg_reg_x18_opt;
	wire [31:0] 	  dbg_reg_x19_opt;
	wire [31:0] 	  dbg_reg_x20_opt;
	wire [31:0] 	  dbg_reg_x21_opt;
	wire [31:0] 	  dbg_reg_x22_opt;
	wire [31:0] 	  dbg_reg_x23_opt;
	wire [31:0] 	  dbg_reg_x24_opt;
	wire [31:0] 	  dbg_reg_x25_opt;
	wire [31:0] 	  dbg_reg_x26_opt;
	wire [31:0] 	  dbg_reg_x27_opt;
	wire [31:0] 	  dbg_reg_x28_opt;
	wire [31:0] 	  dbg_reg_x29_opt;
	wire [31:0] 	  dbg_reg_x30_opt;
	wire [31:0] 	  dbg_reg_x31_opt;
`endif

	picorv32_wrapper_opt top_opt (
		.clk(clk),
		.resetn(resetn),
		.trap(trap_opt),
`ifdef DEBUGREGS   
	.dbg_reg_x0(dbg_reg_x0_opt),
	.dbg_reg_x1(dbg_reg_x1_opt),
	.dbg_reg_x2(dbg_reg_x2_opt),
	.dbg_reg_x3(dbg_reg_x3_opt),
	.dbg_reg_x4(dbg_reg_x4_opt),
	.dbg_reg_x5(dbg_reg_x5_opt),
	.dbg_reg_x6(dbg_reg_x6_opt),
	.dbg_reg_x7(dbg_reg_x7_opt),
	.dbg_reg_x8(dbg_reg_x8_opt),
	.dbg_reg_x9(dbg_reg_x9_opt),
	.dbg_reg_x10(dbg_reg_x10_opt),
	.dbg_reg_x11(dbg_reg_x11_opt),
	.dbg_reg_x12(dbg_reg_x12_opt),
	.dbg_reg_x13(dbg_reg_x13_opt),
	.dbg_reg_x14(dbg_reg_x14_opt),
	.dbg_reg_x15(dbg_reg_x15_opt),
	.dbg_reg_x16(dbg_reg_x16_opt),
	.dbg_reg_x17(dbg_reg_x17_opt),
	.dbg_reg_x18(dbg_reg_x18_opt),
	.dbg_reg_x19(dbg_reg_x19_opt),
	.dbg_reg_x20(dbg_reg_x20_opt),
	.dbg_reg_x21(dbg_reg_x21_opt),
	.dbg_reg_x22(dbg_reg_x22_opt),
	.dbg_reg_x23(dbg_reg_x23_opt),
	.dbg_reg_x24(dbg_reg_x24_opt),
	.dbg_reg_x25(dbg_reg_x25_opt),
	.dbg_reg_x26(dbg_reg_x26_opt),
	.dbg_reg_x27(dbg_reg_x27_opt),
	.dbg_reg_x28(dbg_reg_x28_opt),
	.dbg_reg_x29(dbg_reg_x29_opt),
	.dbg_reg_x30(dbg_reg_x30_opt),
	.dbg_reg_x31(dbg_reg_x31_opt),
`endif
		.trace_valid(trace_valid_opt),
		.trace_data(trace_data_opt)
	);

endmodule
`endif
