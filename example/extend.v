module extend(input [7:0] ch, output [31:0] i, output [31:0] j, output [63:0] k);

   assign i = {24'b0,ch};
   assign j = $signed(ch);
   assign k = $signed(j);
   
endmodule // extend
