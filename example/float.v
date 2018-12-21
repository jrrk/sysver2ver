
module tilde
(
	input [31:0] 	  irq, mask,
	output reg [31:0] wrdata,
	output reg 	  zero,lognot
);

   real arg = 3.14159;
   
   assign wrdata = irq & ~mask;
   assign zero = !mask;
   assign lognot = ( ! $realtobits(arg));

endmodule
