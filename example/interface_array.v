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
// interface_array example based on 
// ariane/tb/ariane_testharness.sv Author: Florian Zaruba, ETH Zurich

/*
example compilation:

verilator --xml-only -Wno-width -Wno-unpacked \
 /local/scratch/jrrk2/ariane/include/riscv_pkg.sv \
 /local/scratch/jrrk2/ariane/src/debug/dm_pkg.sv \
 /local/scratch/jrrk2/ariane/include/ariane_pkg.sv \
 /local/scratch/jrrk2/ariane/include/std_cache_pkg.sv \
 /local/scratch/jrrk2/ariane/include/serpent_cache_pkg.sv \
 /local/scratch/jrrk2/ariane/src/axi/src/axi_pkg.sv \
 /local/scratch/jrrk2/ariane/src/register_interface/src/reg_intf.sv \
 /local/scratch/jrrk2/ariane/include/axi_intf.sv \
 /local/scratch/jrrk2/ariane/include/ariane_axi_pkg.sv \
 ../ariane/tb/ariane_soc_pkg.sv example/interface_array.v \
 ../ariane/src/util/axi_slave_connect.sv \
 ../ariane/src/util/axi_master_connect.sv \
 ../ariane/src/axi_node/src/*.sv \
 ../ariane/src/common_cells/src/fifo_v*.sv --top-module interface_array

*/

module interface_array #(
    parameter int unsigned AXI_ID_WIDTH      = 4,
    parameter int unsigned AXI_USER_WIDTH    = 1,
    parameter int unsigned AXI_ADDRESS_WIDTH = 64,
    parameter int unsigned AXI_DATA_WIDTH    = 64
) (
    input  logic                           clk_i,
    input  logic                           rst_n,
    output logic [31:0]                    status_o
);

    wire test_en = 1'b0;

    localparam NB_SLAVE = 4;
    localparam NB_MASTER = 4;

    localparam AXI_ID_WIDTH_SLAVES = AXI_ID_WIDTH + $clog2(NB_SLAVE);

    AXI_BUS #(
        .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH ),
        .AXI_DATA_WIDTH ( AXI_DATA_WIDTH    ),
        .AXI_ID_WIDTH   ( AXI_ID_WIDTH      ),
        .AXI_USER_WIDTH ( AXI_USER_WIDTH    )
    ) slave[NB_SLAVE-1:0]();

    AXI_BUS #(
        .AXI_ADDR_WIDTH ( AXI_ADDRESS_WIDTH   ),
        .AXI_DATA_WIDTH ( AXI_DATA_WIDTH      ),
        .AXI_ID_WIDTH   ( AXI_ID_WIDTH_SLAVES ),
        .AXI_USER_WIDTH ( AXI_USER_WIDTH      )
    ) master[NB_MASTER-1:0]();

    assign status_o = master[0].aw_addr ^ slave[1].aw_addr;
   
    // ---------------
    // AXI Xbar
    // ---------------
    axi_node_intf_wrap #(
        .NB_SLAVE           ( NB_SLAVE                   ),
        .NB_MASTER          ( NB_MASTER                  ),
        .AXI_ADDR_WIDTH     ( AXI_ADDRESS_WIDTH          ),
        .AXI_DATA_WIDTH     ( AXI_DATA_WIDTH             ),
        .AXI_USER_WIDTH     ( AXI_USER_WIDTH             ),
        .AXI_ID_WIDTH       ( AXI_ID_WIDTH               )
        // .MASTER_SLICE_DEPTH ( 0                          ),
        // .SLAVE_SLICE_DEPTH  ( 0                          )
    ) i_axi_xbar (
        .clk          ( clk_i      ),
        .rst_n        ( rst_n      ),
        .test_en_i    ( test_en    ),
        .slave        ( slave      ),
        .master       ( master     ),
        .start_addr_i ({
            ariane_soc::DebugBase,
            ariane_soc::ROMBase,
            ariane_soc::CLINTBase,
            ariane_soc::PLICBase,
            ariane_soc::UARTBase,
            ariane_soc::SPIBase,
            ariane_soc::EthernetBase,
            ariane_soc::GPIOBase,
            ariane_soc::DRAMBase
        }),
        .end_addr_i   ({
            ariane_soc::DebugBase    + ariane_soc::DebugLength - 1,
            ariane_soc::ROMBase      + ariane_soc::ROMLength - 1,
            ariane_soc::CLINTBase    + ariane_soc::CLINTLength - 1,
            ariane_soc::PLICBase     + ariane_soc::PLICLength - 1,
            ariane_soc::UARTBase     + ariane_soc::UARTLength - 1,
            ariane_soc::SPIBase      + ariane_soc::SPILength - 1,
            ariane_soc::EthernetBase + ariane_soc::EthernetLength -1,
            ariane_soc::GPIOBase     + ariane_soc::GPIOLength - 1,
            ariane_soc::DRAMBase     + ariane_soc::DRAMLength - 1
        })
    );

endmodule
