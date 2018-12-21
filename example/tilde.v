
module tilde
(
	input [31:0] 	  irq, mask,
	output reg [31:0] wrdata,
	output reg 	  zero,lognot
);

   assign wrdata = irq & ~mask;
   assign zero = !mask;
   assign lognot = !$value$plusargs("lognot",);

endmodule
