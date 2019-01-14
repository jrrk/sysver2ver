// Copyright 2018 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// nested loops example, based on:

// Author:         Antonio Pullini - pullinia@iis.ee.ethz.ch
//
// Additional contributions by:
//                 Sven Stucki - svstucki@student.ethz.ch
//                 Markus Wegmann - markus.wegmann@technokrat.ch
//
// Design Name:    RISC-V register file
// Project Name:   zero-riscy
// Language:       SystemVerilog
//
// Description:    Register file with 31 or 15x 32 bit wide registers.
//                 Register 0 is fixed to 0. This register file is based on
//                 latches and is thus smaller than the flip-flop based RF.
//

module nested #(
  parameter int unsigned DATA_WIDTH     = 32,
  parameter int unsigned NR_READ_PORTS  = 2,
  parameter int unsigned NR_WRITE_PORTS = 2,
  parameter bit          ZERO_REG_ZERO  = 0
)(
  // clock and reset
  input  logic                                      clk_i,
  input  logic                                      rst_ni,
  // disable clock gates for testing
  input  logic                                      test_en_i,
  // read port
  input  logic [NR_READ_PORTS-1:0][4:0]             raddr_i,
  output logic [NR_READ_PORTS-1:0][DATA_WIDTH-1:0]  rdata_o,
  // write port
  input  logic [NR_WRITE_PORTS-1:0][4:0]            waddr_i,
  input  logic [NR_WRITE_PORTS-1:0][DATA_WIDTH-1:0] wdata_i,
  input  logic [NR_WRITE_PORTS-1:0]                 we_i
);

    localparam ADDR_WIDTH = 5;
    localparam NUM_WORDS  = 2**ADDR_WIDTH;

    logic [NR_WRITE_PORTS-1:0][NUM_WORDS-1:1]  waddr_onehot;

    // WRITE : Write Address Decoder (WAD), combinatorial process
    always_comb begin : decode_write_addess
        for (int unsigned i = 0; i < NR_WRITE_PORTS; i++) begin
            for (int unsigned j = 1; j < NUM_WORDS; j++) begin
                if (we_i[i] && (waddr_i[i] == j[4:0]))
                    waddr_onehot[i][j] = 1'b1;
                else
                    waddr_onehot[i][j] = 1'b0;
            end
        end
    end

endmodule
