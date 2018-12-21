
module tilde
(
	input [31:0] 	  irq, mask,
	output reg [31:0] wrdata,
	output reg 	  zero,lognot
);

    reg [1023:0]  arg;
   
   assign wrdata = irq & ~mask;
   assign zero = !mask;
   assign lognot = !$value$plusargs("lognot=%d", arg);

endmodule
