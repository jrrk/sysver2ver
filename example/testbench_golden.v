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
	wire trap;

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

	wire trace_valid;
	wire [35:0] trace_data;
	integer trace_file;

	initial begin
		if ($test$plusargs("trace")) begin
			trace_file = $fopen("testbench.trace", "w");
			repeat (10) @(posedge clk);
			while (!trap) begin
				@(posedge clk);
				if (trace_valid)
					$fwrite(trace_file, "%x\n", trace_data);
			end
			$fclose(trace_file);
			$display("Finished writing testbench.trace.");
		end
	end

`ifdef DEBUGREGS   
	wire [31:0] 	  dbg_reg_x0;
	wire [31:0] 	  dbg_reg_x1;
	wire [31:0] 	  dbg_reg_x2;
	wire [31:0] 	  dbg_reg_x3;
	wire [31:0] 	  dbg_reg_x4;
	wire [31:0] 	  dbg_reg_x5;
	wire [31:0] 	  dbg_reg_x6;
	wire [31:0] 	  dbg_reg_x7;
	wire [31:0] 	  dbg_reg_x8;
	wire [31:0] 	  dbg_reg_x9;
	wire [31:0] 	  dbg_reg_x10;
	wire [31:0] 	  dbg_reg_x11;
	wire [31:0] 	  dbg_reg_x12;
	wire [31:0] 	  dbg_reg_x13;
	wire [31:0] 	  dbg_reg_x14;
	wire [31:0] 	  dbg_reg_x15;
	wire [31:0] 	  dbg_reg_x16;
	wire [31:0] 	  dbg_reg_x17;
	wire [31:0] 	  dbg_reg_x18;
	wire [31:0] 	  dbg_reg_x19;
	wire [31:0] 	  dbg_reg_x20;
	wire [31:0] 	  dbg_reg_x21;
	wire [31:0] 	  dbg_reg_x22;
	wire [31:0] 	  dbg_reg_x23;
	wire [31:0] 	  dbg_reg_x24;
	wire [31:0] 	  dbg_reg_x25;
	wire [31:0] 	  dbg_reg_x26;
	wire [31:0] 	  dbg_reg_x27;
	wire [31:0] 	  dbg_reg_x28;
	wire [31:0] 	  dbg_reg_x29;
	wire [31:0] 	  dbg_reg_x30;
	wire [31:0] 	  dbg_reg_x31;
`endif

	picorv32_wrapper #(
		.AXI_TEST (AXI_TEST),
		.VERBOSE  (VERBOSE)
	) top (
		.clk(clk),
		.resetn(resetn),
		.trap(trap),
`ifdef DEBUGREGS   
	.dbg_reg_x0,
	.dbg_reg_x1,
	.dbg_reg_x2,
	.dbg_reg_x3,
	.dbg_reg_x4,
	.dbg_reg_x5,
	.dbg_reg_x6,
	.dbg_reg_x7,
	.dbg_reg_x8,
	.dbg_reg_x9,
	.dbg_reg_x10,
	.dbg_reg_x11,
	.dbg_reg_x12,
	.dbg_reg_x13,
	.dbg_reg_x14,
	.dbg_reg_x15,
	.dbg_reg_x16,
	.dbg_reg_x17,
	.dbg_reg_x18,
	.dbg_reg_x19,
	.dbg_reg_x20,
	.dbg_reg_x21,
	.dbg_reg_x22,
	.dbg_reg_x23,
	.dbg_reg_x24,
	.dbg_reg_x25,
	.dbg_reg_x26,
	.dbg_reg_x27,
	.dbg_reg_x28,
	.dbg_reg_x29,
	.dbg_reg_x30,
	.dbg_reg_x31,
`endif
		.trace_valid(trace_valid),
		.trace_data(trace_data)
	);

endmodule
`endif
