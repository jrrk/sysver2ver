/* Copyright 2018 ETH Zurich and University of Bologna.
 * Copyright and related rights are licensed under the Solderpad Hardware
 * License, Version 0.51 (the “License”); you may not use this file except in
 * compliance with the License.  You may obtain a copy of the License at
 * http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
 * or agreed to in writing, software, hardware and materials distributed under
 * this License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * File:  caseinside.v by Jonathan Kimmitt based on dm_csrs.sv by Florian Zaruba <zarubaf@iis.ee.ethz.ch>
 *
 */

module caseinside (
    input  logic                              clk_i,              // Clock
    input  logic                              rst_ni,             // Asynchronous reset active low
    input  logic [6:0]                        addr,
    output logic [31:0]                       rslt
   );
   
   typedef enum logic [7:0] {
        Data0        = 8'h04,
        Data1        = 8'h05,
        Data2        = 8'h06,
        Data3        = 8'h07,
        Data4        = 8'h08,
        Data5        = 8'h09,
        Data6        = 8'h0A,
        Data7        = 8'h0B,
        Data8        = 8'h0C,
        Data9        = 8'h0D,
        Data10       = 8'h0E,
        Data11       = 8'h0F,
        DMControl    = 8'h10,
        DMStatus     = 8'h11,
        Hartinfo     = 8'h12,
        HaltSum1     = 8'h13,
        HAWindowSel  = 8'h14,
        HAWindow     = 8'h15,
        AbstractCS   = 8'h16,
        Command      = 8'h17} dm_csr_t;

    localparam logic [3:0] DataCount = 4'h2;
    localparam dm_csr_t DataEnd = dm_csr_t'((Data0 + {4'b0, DataCount}));

    function automatic logic [31:0] nop ();
        return 32'h00000013;
    endfunction
   
    logic [6:0] 		   addr2;

    always_comb begin : csr_read_write
            unique case ({1'b0, addr}) inside
                [(Data0):DataEnd]: begin
                  rslt = {25'b0,~addr};
                   $display("case [(Data0):DataEnd]");
                end
                Command:
                  rslt = nop();
                default:
                  addr2 = addr;
            endcase
        end

endmodule
