// Copyright 2018 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

// Description: Xilinx FPGA top-level
// Author: Jonathan Kimmitt (based on fpga/src/ariane_xilinx.sv by Florian Zaruba <zarubaf@iis.ee.ethz.ch>)

/* compiled with:
verilator --xml-only -Wno-width -Wno-unpacked /local/scratch/jrrk2/ariane/include/riscv_pkg.sv /local/scratch/jrrk2/ariane/src/debug/dm_pkg.sv /local/scratch/jrrk2/ariane/include/ariane_pkg.sv /local/scratch/jrrk2/ariane/include/std_cache_pkg.sv /local/scratch/jrrk2/ariane/include/serpent_cache_pkg.sv /local/scratch/jrrk2/ariane/src/axi/src/axi_pkg.sv /local/scratch/jrrk2/ariane/src/register_interface/src/reg_intf.sv /local/scratch/jrrk2/ariane/include/axi_intf.sv /local/scratch/jrrk2/ariane/include/ariane_axi_pkg.sv example/axi_slave_connect_example.v ../ariane/src/util/axi_slave_connect.sv ../ariane/src/util/axi_master_connect.sv --top-module axi_slave_connect_example
*/
  
module axi_slave_connect_example (
    output ariane_axi::req_t    dm_axi_s_req,
    input  ariane_axi::resp_t   dm_axi_s_resp,		      
    input  ariane_axi::req_t    dm_axi_m_req,
    output ariane_axi::resp_t   dm_axi_m_resp
);
localparam AxiAddrWidth = 64;
localparam AxiDataWidth = 64;
localparam AxiIdWidthMaster = 4;
localparam AxiIdWidthSlaves = 4;
localparam AxiUserWidth = 1;

AXI_BUS #(
    .AXI_ADDR_WIDTH ( AxiAddrWidth     ),
    .AXI_DATA_WIDTH ( AxiDataWidth     ),
    .AXI_ID_WIDTH   ( AxiIdWidthMaster ),
    .AXI_USER_WIDTH ( AxiUserWidth     )
) slave();

AXI_BUS #(
    .AXI_ADDR_WIDTH ( AxiAddrWidth     ),
    .AXI_DATA_WIDTH ( AxiDataWidth     ),
    .AXI_ID_WIDTH   ( AxiIdWidthSlaves ),
    .AXI_USER_WIDTH ( AxiUserWidth     )
) master();

axi_master_connect i_axi_master_dm (.axi_req_i(dm_axi_m_req), .axi_resp_o(dm_axi_m_resp), .master(slave));
axi_slave_connect  i_axi_slave_dm  (.axi_req_o(dm_axi_s_req), .axi_resp_i(dm_axi_s_resp), .slave(master));

endmodule
