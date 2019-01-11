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
// unpacked array test (based on plic_find_max by the authors below)
//-------------------------------------------------------------------------------
//-- Author     : Gian Marti      <gimarti.student.ethz.ch>
//-- Author     : Thomas Kramer   <tkramer.student.ethz.ch>
//-- Author     : Thomas E. Benz  <tbenz.student.ethz.ch>
//-- Company    : Integrated Systems Laboratory, ETH Zurich
//-- Created    : 2018-03-31
//-- Last update: 2018-03-31
//-- Platform   : ModelSim (simulation), Synopsys (synthesis)
//-- Standard   : SystemVerilog IEEE 1800-2012

module unpacked #(
    parameter int NUM_OPERANDS      = 3,
    parameter int ID_BITWIDTH       = 3,
    parameter int PRIORITY_BITWIDTH = 4
)(
    input logic [PRIORITY_BITWIDTH-1:0]  priorities_i [NUM_OPERANDS],
    input logic [ID_BITWIDTH-1:0 ]       identifiers_i [NUM_OPERANDS],
    output logic [PRIORITY_BITWIDTH-1:0] largest_priority_o,
    output logic [ID_BITWIDTH-1:0 ]      identifier_of_largest_o,
    output logic [31:0]                  num_operands_out,
    output logic [31:0]                  id_bitwidth_out,
    output logic [31:0]                  priority_bitwidth_out,
    output logic [31:0]                  max_stage_out,
    output logic [31:0]                  num_operands_aligned_out,
    input logic [4:0]                    idx1,
    input logic [3:0]                    idx2,
    input logic [3:0]                    idx3,
    input logic [1:0]                    idx4,
    input logic [2:0]                    idx5,
    input logic [4:0]                    idx6,
    output logic [10:0]                  rslt1,
    output logic [7:0]                   rslt2,
    output logic [31:0]                  rslt3,
    output logic [0:0]                   rslt4,
    output logic [0:0]                   rslt5,
    input logic                          wr, ck,
    input logic [31:0]                   data
);

   logic [10:0]         unpacked1 [20];
   logic [5:0] [7:0]    packed1;
   logic [31:0]         unpacked2 [0:15];
   logic [31:0]         unpacked2d [0:3][0:7];

   wire [4:0]           addr1 = idx1 < 20 ? idx1 : '0;
   
   assign rslt1 = unpacked1[addr1];
   assign rslt2 = packed1[idx5];
   assign rslt3 = unpacked2[idx2];
   assign rslt4 = unpacked2[idx3][num_operands_out];
   assign rslt5 = unpacked2d[idx4][idx5][idx6];

    always@(posedge ck)
        begin
        if (wr) 
            begin
            unpacked1[addr1] <= data[10:0];
            packed1[idx5] <= data[7:0];
            unpacked2[idx3] <= data;
            unpacked2d[idx4][idx5] <= data;
            end
        end

    localparam int max_stage            = ($clog2(NUM_OPERANDS)-1);
    localparam int num_operands_aligned = 2**(max_stage+1);

    logic [PRIORITY_BITWIDTH-1:0] priority_stages   [max_stage + 2][num_operands_aligned];
    logic [ID_BITWIDTH-1:0      ] identifier_stages [max_stage + 2][num_operands_aligned];

    assign num_operands_out = NUM_OPERANDS;
    assign id_bitwidth_out = ID_BITWIDTH;
    assign priority_bitwidth_out = PRIORITY_BITWIDTH;
    assign max_stage_out = max_stage;
    assign num_operands_aligned_out = num_operands_aligned;
   
    always_comb begin : proc_zero_padding
        for (integer operand = 0; operand < num_operands_aligned; operand++) begin
            if(operand < NUM_OPERANDS) begin
                priority_stages  [0][operand] = priorities_i  [operand];
                identifier_stages[0][operand] = identifiers_i [operand];
            end else begin
                priority_stages  [0][operand] = '0;
                identifier_stages[0][operand] = '0;
            end
        end
    end

    for (genvar comparator_stage = max_stage; comparator_stage >= 0 ; comparator_stage--) begin
        for (genvar stage_index  = 0; stage_index < 2**comparator_stage; stage_index++)   begin
            plic_comparator #(
                .ID_BITWIDTH        (ID_BITWIDTH       ),
                .PRIORITY_BITWIDTH  (PRIORITY_BITWIDTH )
            ) comp_instance(
                .left_priority_i        ( priority_stages  [max_stage - comparator_stage][2*stage_index]     ),
                .right_priority_i       ( priority_stages  [max_stage - comparator_stage][2*stage_index + 1] ),
                .left_identifier_i      ( identifier_stages[max_stage - comparator_stage][2*stage_index]     ),
                .right_identifier_i     ( identifier_stages[max_stage - comparator_stage][2*stage_index + 1] ),
                .larger_priority_o      ( priority_stages  [max_stage - (comparator_stage-1)][stage_index]   ),
                .identifier_of_larger_o ( identifier_stages[max_stage - (comparator_stage-1)][stage_index]   )
            );
        end
    end

    assign largest_priority_o      = priority_stages  [max_stage+1][0];
    assign identifier_of_largest_o = identifier_stages[max_stage+1][0];
endmodule
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
//-------------------------------------------------------------------------------
//-- Title      : Comperator
//-- File       : plic_comperator.sv
//-- Author     : Gian Marti      <gimarti.student.ethz.ch>
//-- Author     : Thomas Kramer   <tkramer.student.ethz.ch>
//-- Author     : Thomas E. Benz  <tbenz.student.ethz.ch>
//-- Company    : Integrated Systems Laboratory, ETH Zurich
//-- Created    : 2018-03-31
//-- Last update: 2018-03-31
//-- Platform   : ModelSim (simulation), Synopsys (synthesis)
//-- Standard   : SystemVerilog IEEE 1800-2012
//-------------------------------------------------------------------------------
//-- Description: Comparator
//-------------------------------------------------------------------------------
//-- Revisions  :
//-- Date        Version  Author  Description
//-- 2018-03-31  2.0      tbenz   Created header
//-------------------------------------------------------------------------------

// find larger operand (value and identifier)
// chooses the left operand on equality
module plic_comparator #(
    parameter int ID_BITWIDTH       = -1,
    parameter int PRIORITY_BITWIDTH = -1
)(
    input  logic [PRIORITY_BITWIDTH-1:0]  left_priority_i,
    input  logic [PRIORITY_BITWIDTH-1:0]  right_priority_i,
    input  logic [ID_BITWIDTH-1:0      ]  left_identifier_i,
    input  logic [ID_BITWIDTH-1:0      ]  right_identifier_i,
    output logic [PRIORITY_BITWIDTH-1:0]  larger_priority_o,
    output logic[ ID_BITWIDTH-1:0      ]  identifier_of_larger_o
);

    always_comb begin : proc_compare
        if (left_priority_i >= right_priority_i) begin
            larger_priority_o      = left_priority_i;
            identifier_of_larger_o = left_identifier_i;
        end else begin
            larger_priority_o      = right_priority_i;
            identifier_of_larger_o = right_identifier_i;
        end
    end

endmodule

`ifndef VERILATOR

module tb;

   parameter int NUM_OPERANDS      = 3;
   parameter int ID_BITWIDTH       = 3;   
   parameter int PRIORITY_BITWIDTH = 4;

    logic [PRIORITY_BITWIDTH-1:0] priorities_i [NUM_OPERANDS];
    logic [ID_BITWIDTH-1:0 ]      identifiers_i [NUM_OPERANDS];
    logic [PRIORITY_BITWIDTH-1:0] largest_priority_o;
    logic [ID_BITWIDTH-1:0 ]      identifier_of_largest_o;
    logic [31:0]                  num_operands_out;
    logic [31:0]                  id_bitwidth_out;
    logic [31:0]                  priority_bitwidth_out;
    logic [31:0]                  max_stage_out;
    logic [31:0]                  num_operands_aligned_out;

    unpacked dut(.*);
   
    initial
      begin
         priorities_i = '{default:0};
         identifiers_i = '{default:0};
         #1000
           $finish;
      end
   
endmodule // tb
`endif //  `ifndef VERILATOR
