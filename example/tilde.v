
module tilde
(
	input [31:0]		       irq, mask,
	output reg [31:0]	       wrdata
);

   assign wrdata = irq & ~mask;

endmodule
