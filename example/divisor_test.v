
/***************************************************************
 * picorv32_pcpi_div
 ***************************************************************/

module divisor_test (
	input             clk, resetn,

	input [31:0]      pcpi_rs2,
        input             divs,
	output reg [62:0] divisor
);
	always @(posedge clk) begin
	   divisor <= (divs && pcpi_rs2[31] ? -pcpi_rs2 : pcpi_rs2) << 31;
	end
   
endmodule
