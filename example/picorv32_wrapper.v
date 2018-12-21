// `define DEBUGREGS   

module picorv32_wrapper #(
	parameter AXI_TEST = 0,
	parameter VERBOSE = 0
) (
	input clk,
	input resetn,
	output trap,
`ifdef DEBUGREGS   
	output [31:0] 	  dbg_reg_x0,
	output [31:0] 	  dbg_reg_x1,
	output [31:0] 	  dbg_reg_x2,
	output [31:0] 	  dbg_reg_x3,
	output [31:0] 	  dbg_reg_x4,
	output [31:0] 	  dbg_reg_x5,
	output [31:0] 	  dbg_reg_x6,
	output [31:0] 	  dbg_reg_x7,
	output [31:0] 	  dbg_reg_x8,
	output [31:0] 	  dbg_reg_x9,
	output [31:0] 	  dbg_reg_x10,
	output [31:0] 	  dbg_reg_x11,
	output [31:0] 	  dbg_reg_x12,
	output [31:0] 	  dbg_reg_x13,
	output [31:0] 	  dbg_reg_x14,
	output [31:0] 	  dbg_reg_x15,
	output [31:0] 	  dbg_reg_x16,
	output [31:0] 	  dbg_reg_x17,
	output [31:0] 	  dbg_reg_x18,
	output [31:0] 	  dbg_reg_x19,
	output [31:0] 	  dbg_reg_x20,
	output [31:0] 	  dbg_reg_x21,
	output [31:0] 	  dbg_reg_x22,
	output [31:0] 	  dbg_reg_x23,
	output [31:0] 	  dbg_reg_x24,
	output [31:0] 	  dbg_reg_x25,
	output [31:0] 	  dbg_reg_x26,
	output [31:0] 	  dbg_reg_x27,
	output [31:0] 	  dbg_reg_x28,
	output [31:0] 	  dbg_reg_x29,
	output [31:0] 	  dbg_reg_x30,
	output [31:0] 	  dbg_reg_x31,
`endif
	output trace_valid,
	output [35:0] trace_data
);
	wire tests_passed;
	reg [31:0] irq;

	always @* begin
		irq = 0;
		irq[4] = &uut.picorv32_core.count_cycle[12:0];
		irq[5] = &uut.picorv32_core.count_cycle[15:0];
	end

	wire        mem_axi_awvalid;
	wire        mem_axi_awready;
	wire [31:0] mem_axi_awaddr;
	wire [ 2:0] mem_axi_awprot;

	wire        mem_axi_wvalid;
	wire        mem_axi_wready;
	wire [31:0] mem_axi_wdata;
	wire [ 3:0] mem_axi_wstrb;

	wire        mem_axi_bvalid;
	wire        mem_axi_bready;

	wire        mem_axi_arvalid;
	wire        mem_axi_arready;
	wire [31:0] mem_axi_araddr;
	wire [ 2:0] mem_axi_arprot;

	wire        mem_axi_rvalid;
	wire        mem_axi_rready;
	wire [31:0] mem_axi_rdata;

	axi4_memory #(
		.AXI_TEST (AXI_TEST),
		.VERBOSE  (VERBOSE)
	) mem (
		.clk             (clk             ),
		.mem_axi_awvalid (mem_axi_awvalid ),
		.mem_axi_awready (mem_axi_awready ),
		.mem_axi_awaddr  (mem_axi_awaddr  ),
		.mem_axi_awprot  (mem_axi_awprot  ),

		.mem_axi_wvalid  (mem_axi_wvalid  ),
		.mem_axi_wready  (mem_axi_wready  ),
		.mem_axi_wdata   (mem_axi_wdata   ),
		.mem_axi_wstrb   (mem_axi_wstrb   ),

		.mem_axi_bvalid  (mem_axi_bvalid  ),
		.mem_axi_bready  (mem_axi_bready  ),

		.mem_axi_arvalid (mem_axi_arvalid ),
		.mem_axi_arready (mem_axi_arready ),
		.mem_axi_araddr  (mem_axi_araddr  ),
		.mem_axi_arprot  (mem_axi_arprot  ),

		.mem_axi_rvalid  (mem_axi_rvalid  ),
		.mem_axi_rready  (mem_axi_rready  ),
		.mem_axi_rdata   (mem_axi_rdata   ),

		.tests_passed    (tests_passed    )
	);

`ifdef RISCV_FORMAL
	wire        rvfi_valid;
	wire [63:0] rvfi_order;
	wire [31:0] rvfi_insn;
	wire        rvfi_trap;
	wire        rvfi_halt;
	wire        rvfi_intr;
	wire [4:0]  rvfi_rs1_addr;
	wire [4:0]  rvfi_rs2_addr;
	wire [31:0] rvfi_rs1_rdata;
	wire [31:0] rvfi_rs2_rdata;
	wire [4:0]  rvfi_rd_addr;
	wire [31:0] rvfi_rd_wdata;
	wire [31:0] rvfi_pc_rdata;
	wire [31:0] rvfi_pc_wdata;
	wire [31:0] rvfi_mem_addr;
	wire [3:0]  rvfi_mem_rmask;
	wire [3:0]  rvfi_mem_wmask;
	wire [31:0] rvfi_mem_rdata;
	wire [31:0] rvfi_mem_wdata;
`endif

	picorv32_axi #(
`ifndef SYNTH_TEST
`ifdef SP_TEST
		.ENABLE_REGS_DUALPORT(0),
`endif
`ifdef COMPRESSED_ISA
		.COMPRESSED_ISA(1),
`endif
		.ENABLE_MUL(1),
		.ENABLE_DIV(1),
		.ENABLE_IRQ(1),
		.ENABLE_TRACE(1)
`endif
	) uut (
		.clk            (clk            ),
		.resetn         (resetn         ),
		.trap           (trap           ),
		.mem_axi_awvalid(mem_axi_awvalid),
		.mem_axi_awready(mem_axi_awready),
		.mem_axi_awaddr (mem_axi_awaddr ),
		.mem_axi_awprot (mem_axi_awprot ),
		.mem_axi_wvalid (mem_axi_wvalid ),
		.mem_axi_wready (mem_axi_wready ),
		.mem_axi_wdata  (mem_axi_wdata  ),
		.mem_axi_wstrb  (mem_axi_wstrb  ),
		.mem_axi_bvalid (mem_axi_bvalid ),
		.mem_axi_bready (mem_axi_bready ),
		.mem_axi_arvalid(mem_axi_arvalid),
		.mem_axi_arready(mem_axi_arready),
		.mem_axi_araddr (mem_axi_araddr ),
		.mem_axi_arprot (mem_axi_arprot ),
		.mem_axi_rvalid (mem_axi_rvalid ),
		.mem_axi_rready (mem_axi_rready ),
		.mem_axi_rdata  (mem_axi_rdata  ),
		.irq            (irq            ),
`ifdef RISCV_FORMAL
		.rvfi_valid     (rvfi_valid     ),
		.rvfi_order     (rvfi_order     ),
		.rvfi_insn      (rvfi_insn      ),
		.rvfi_trap      (rvfi_trap      ),
		.rvfi_halt      (rvfi_halt      ),
		.rvfi_intr      (rvfi_intr      ),
		.rvfi_rs1_addr  (rvfi_rs1_addr  ),
		.rvfi_rs2_addr  (rvfi_rs2_addr  ),
		.rvfi_rs1_rdata (rvfi_rs1_rdata ),
		.rvfi_rs2_rdata (rvfi_rs2_rdata ),
		.rvfi_rd_addr   (rvfi_rd_addr   ),
		.rvfi_rd_wdata  (rvfi_rd_wdata  ),
		.rvfi_pc_rdata  (rvfi_pc_rdata  ),
		.rvfi_pc_wdata  (rvfi_pc_wdata  ),
		.rvfi_mem_addr  (rvfi_mem_addr  ),
		.rvfi_mem_rmask (rvfi_mem_rmask ),
		.rvfi_mem_wmask (rvfi_mem_wmask ),
		.rvfi_mem_rdata (rvfi_mem_rdata ),
		.rvfi_mem_wdata (rvfi_mem_wdata ),
`endif
`ifdef DEBUGREGS   
	.dbg_reg_x0,
	.dbg_reg_x1,
	.dbg_reg_x2,
	.dbg_reg_x3,
	.dbg_reg_x4,
	.dbg_reg_x5,
	.dbg_reg_x6,
	.dbg_reg_x7,
	.dbg_reg_x8,
	.dbg_reg_x9,
	.dbg_reg_x10,
	.dbg_reg_x11,
	.dbg_reg_x12,
	.dbg_reg_x13,
	.dbg_reg_x14,
	.dbg_reg_x15,
	.dbg_reg_x16,
	.dbg_reg_x17,
	.dbg_reg_x18,
	.dbg_reg_x19,
	.dbg_reg_x20,
	.dbg_reg_x21,
	.dbg_reg_x22,
	.dbg_reg_x23,
	.dbg_reg_x24,
	.dbg_reg_x25,
	.dbg_reg_x26,
	.dbg_reg_x27,
	.dbg_reg_x28,
	.dbg_reg_x29,
	.dbg_reg_x30,
	.dbg_reg_x31,
`endif
		.trace_valid    (trace_valid    ),
		.trace_data     (trace_data     )
	);

`ifdef RISCV_FORMAL
	picorv32_rvfimon rvfi_monitor (
		.clock          (clk           ),
		.reset          (!resetn       ),
		.rvfi_valid     (rvfi_valid    ),
		.rvfi_order     (rvfi_order    ),
		.rvfi_insn      (rvfi_insn     ),
		.rvfi_trap      (rvfi_trap     ),
		.rvfi_halt      (rvfi_halt     ),
		.rvfi_intr      (rvfi_intr     ),
		.rvfi_rs1_addr  (rvfi_rs1_addr ),
		.rvfi_rs2_addr  (rvfi_rs2_addr ),
		.rvfi_rs1_rdata (rvfi_rs1_rdata),
		.rvfi_rs2_rdata (rvfi_rs2_rdata),
		.rvfi_rd_addr   (rvfi_rd_addr  ),
		.rvfi_rd_wdata  (rvfi_rd_wdata ),
		.rvfi_pc_rdata  (rvfi_pc_rdata ),
		.rvfi_pc_wdata  (rvfi_pc_wdata ),
		.rvfi_mem_addr  (rvfi_mem_addr ),
		.rvfi_mem_rmask (rvfi_mem_rmask),
		.rvfi_mem_wmask (rvfi_mem_wmask),
		.rvfi_mem_rdata (rvfi_mem_rdata),
		.rvfi_mem_wdata (rvfi_mem_wdata)
	);
`endif

	reg [1023:0] firmware_file;
	initial begin
		if (!$value$plusargs("firmware=%s", firmware_file))
			firmware_file = "firmware/firmware.hex";
		$readmemh(firmware_file, mem.memory);
	end

	integer cycle_counter;
	always @(posedge clk) begin
		cycle_counter <= resetn ? cycle_counter + 1 : 0;
		if (resetn && trap) begin
`ifndef VERILATOR
			repeat (10) @(posedge clk);
`endif
			$display("TRAP after %1d clock cycles", cycle_counter);
			if (tests_passed) begin
				$display("ALL TESTS PASSED.");
				$finish;
			end else begin
				$display("ERROR!");
				if ($test$plusargs("noerror"))
					$finish;
				$stop;
			end
		end
	end
endmodule
