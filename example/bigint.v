module bigint(input clk, output reg [255:0] cnt);

   always @(posedge clk)
     cnt = cnt + 150'hdeadbeefc001f00ddeadbeefc001f00ddeadbeefc001f00ddeadbeefc001f00d;

endmodule // bigint

