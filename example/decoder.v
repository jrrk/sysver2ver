module decoder (
	input clk, resetn,
	input [31:0] mem_rdata_latched,
	output reg [31:0] decoded_imm_uj,
        output logic [31:0] mem_rdata_q);

always@(posedge clk) 
  begin
     mem_rdata_q[14:12] <= 3'h0;
	   { decoded_imm_uj[31:20], decoded_imm_uj[10:1], decoded_imm_uj[11], decoded_imm_uj[19:12], decoded_imm_uj[0] } <= $signed({mem_rdata_latched[31:12], 1'b0});
           { decoded_imm_uj[31:11], decoded_imm_uj[4], decoded_imm_uj[9:8], decoded_imm_uj[10], decoded_imm_uj[6],
             decoded_imm_uj[7], decoded_imm_uj[3:1], decoded_imm_uj[5], decoded_imm_uj[0] } <= $signed({mem_rdata_latched[12:2], 1'b0});
           end

endmodule
