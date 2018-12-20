
/***************************************************************
 * accumulator from picorv32_pcpi_mul
 ***************************************************************/

module accum (
	input clk, resetn,
	input [63:0] rs1, rs2, rd, rdx,
	output reg [63:0] next_rs1, next_rs2, this_rs2,
	output reg [63:0] next_rd, next_rdx, next_rdt);
   
        localparam CARRY_CHAIN = 4;
	integer i, j;

	// carry save accumulator
	always @* begin
		next_rd = rd;
		next_rdx = rdx;
		next_rs1 = rs1;
		next_rs2 = rs2;

		this_rs2 = next_rs1[0] ? next_rs2 : 0;
		next_rdt = 0;
		for (j = 0; j < 64; j = j + CARRY_CHAIN)
			{next_rdt[j+CARRY_CHAIN-1], next_rd[j +: CARRY_CHAIN]} =
					next_rd[j +: CARRY_CHAIN] + next_rdx[j +: CARRY_CHAIN] + this_rs2[j +: CARRY_CHAIN];
		next_rdx = next_rdt << 1;
		next_rs1 = next_rs1 >> 1;
		next_rs2 = next_rs2 << 1;
	end

endmodule
