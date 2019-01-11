module forstmt(
               input        clk,
               output [7:0] cnt);

   always @(posedge clk)
     for (logic [7:0] i = 0; i < 10; i=i+1) cnt <= cnt + 1;

endmodule // forstmt
