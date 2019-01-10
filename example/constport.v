
/***************************************************************
 * constport.v based on picorv32
 ***************************************************************/

module constport (
	input 		  clk, resetn,
	output [31:0] 	  dbg_reg_x0
);
	assign dbg_reg_x0  = 0;

endmodule
