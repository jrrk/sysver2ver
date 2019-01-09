module genvarexample #(NUM_SOURCES=3) (input clk, input rst, output [NUM_SOURCES-1:0] [31:0] counter, output [NUM_SOURCES-1:0] tst);

      for (genvar idx= 0; idx< NUM_SOURCES; idx++)
         myinstance inst (.clk, .rst, .counter(counter[idx]));

      for (genvar idx= 0; idx< NUM_SOURCES; idx++)
         test inst (.clk, .rst, .counter(counter[idx]), .tst(tst[idx]));

endmodule

module myinstance(input clk, input rst, output logic [31:0] counter);

always @(posedge clk or posedge rst)
  if (rst)
    counter <= '0;
  else
    counter <= counter + 1;

endmodule // myinstance

module test(input clk, input rst, input [31:0] counter, output tst);

   assign tst = ^counter;
   
endmodule // myinstance
