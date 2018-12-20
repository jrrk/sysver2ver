module decoder (
	input clk, resetn,
	input [31:0] mem_rdata_latched,
	output reg [31:0] decoded_imm_uj
);

always@(posedge clk) 
        begin
	   { decoded_imm_uj[31:20], decoded_imm_uj[10:1], decoded_imm_uj[11], decoded_imm_uj[19:12], decoded_imm_uj[0] } <= $signed({mem_rdata_latched[31:12], 1'b0});
           end

endmodule
