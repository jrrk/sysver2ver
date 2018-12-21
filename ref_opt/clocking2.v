module clocking_edge(input clk, output logic mem_axi_arready);
   
    task handle_axi_arvalid;
        mem_axi_arready <= 1'h1;
    endtask
   
      always@(negedge clk) 
        begin
           if (mem_axi_arready) handle_axi_arvalid;
	end
   
      always @(posedge clk)
	begin
	   mem_axi_arready <= 1'h0;
	end

endmodule // clocking_edge
