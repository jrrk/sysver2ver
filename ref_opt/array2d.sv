module array2d(
        input [31:0] latched_raddr,
        input [31:0] latched_waddr,
        input [31:0] latched_wdata,
        input [ 3:0] latched_wstrb,
	input clk
	       );
	       
        reg [31:0]   memory [0:64*1024/4-1] /* verilator public */;

   always @(posedge clk)
     begin
                if (latched_waddr < 64*1024) begin
                        if (latched_wstrb[0]) memory[latched_waddr >> 2][ 7: 0] <= latched_wdata[ 7: 0];
                        if (latched_wstrb[1]) memory[latched_waddr >> 2][15: 8] <= latched_wdata[15: 8];
                        if (latched_wstrb[2]) memory[latched_waddr >> 2][23:16] <= latched_wdata[23:16];
                        if (latched_wstrb[3]) memory[latched_waddr >> 2][31:24] <= latched_wdata[31:24];
                end	
     end

endmodule // array2d
