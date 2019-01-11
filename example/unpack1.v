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

module unpack1(
    input logic [4:0]   idx1,
    input logic         wr, ck,
    input logic [10:0]  data,
    output logic [10:0] rslt1
);

   logic [10:0]         unpacked1 [19:0];
   wire [4:0]           addr = idx1 < 20 ? idx1 : '0;
   
   assign rslt1 = unpacked1 [ addr ];

   always @(posedge ck)
     if (wr) unpacked1[addr] <= data;
   
endmodule
