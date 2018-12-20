`default_nettype none

module reset_transform #(
        parameter STEPS_AT_ONCE = 1,
        parameter CARRY_CHAIN = 4
) (
    input logic clk,
    input logic resetn,
    input logic pcpi_valid,
    input logic[31:0] pcpi_insn,
    input logic[31:0] pcpi_rs1,
    input logic[31:0] pcpi_rs2,
    input logic [63:0] next_rs1,
    input logic [63:0] next_rs2,
    input logic [63:0] next_rd,
    input logic [63:0] next_rdx,
    input logic instr_any_mulh,
    input logic instr_rs1_signed,
    input logic instr_rs2_signed,
    input logic mul_start,
    output logic [63:0] rs1,
    output logic [63:0] rs2,
    output logic [63:0] rd,
    output logic [63:0] rdx,
    output logic [6:0] mul_counter,
    output logic mul_waiting,
    output logic mul_finish);
    logic [31:0] i;
    logic [31:0] j;
    logic __Vconcswap1;
    logic [3:0] __Vconcswap2;

        always @(posedge clk) begin
                mul_finish <= 0;
                if (!resetn) begin
                        mul_waiting <= 1;
                end else
                if (mul_waiting) begin
                        if (instr_rs1_signed)
                                rs1 <= $signed(pcpi_rs1);
                        else
                                rs1 <= $unsigned(pcpi_rs1);

                        if (instr_rs2_signed)
                                rs2 <= $signed(pcpi_rs2);
                        else
                                rs2 <= $unsigned(pcpi_rs2);

                        rd <= 0;
                        rdx <= 0;
                        mul_counter <= (instr_any_mulh ? 63 - STEPS_AT_ONCE : 31 - STEPS_AT_ONCE);
                        mul_waiting <= !mul_start;
                end else begin
                        rd <= next_rd;
                        rdx <= next_rdx;
                        rs1 <= next_rs1;
                        rs2 <= next_rs2;

                        mul_counter <= mul_counter - STEPS_AT_ONCE;
                        if (mul_counter[6]) begin
                                mul_finish <= 1;
                                mul_waiting <= 1;
                        end
                end
        end        

    endmodule
