// Cut down system verilog test

module test (input clk, input rst, output [31:0] test1);

   // interface instance
   handshake inf1 (), inf2[1:0]();
   simple simon1[3:0]();

   // source instance
   source source0 (
      .inf1 (inf1),
      .inf2 (inf2),
      .clk  (clk),
      .rst  (rst)
   );

   // drain instance
   drain drain0 (
      .clk  (clk),
      .rst  (rst),
      .inf1 (inf1),
      .inf2 (inf2)
   );

   assign test1 = inf1.port1 ^ inf2[0].port1;
   
endmodule

interface handshake #(parameter int unsigned WIDTH = 32)();

   // modport signals
   logic [WIDTH-1:0] port1, port2;

   modport dir1 (
      output port1,
      input  port2
   );

   modport dir2 (
      input  port1,
      output port2
   );

endinterface

interface simple #(parameter int unsigned WIDTH = 32)();

   // modport signals
   logic [WIDTH-1:0] simple1, simple2;

   modport dir1 (
      output simple1,
      input  simple2
   );

   modport dir2 (
      input  simple1,
      output simple2
   );

endinterface

// source module
module source (
   input logic    clk,
   input logic    rst,
   handshake.dir1 inf1,
   handshake.dir2 inf2[1:0]
);

   assign inf1.port1 = inf2[0].port1;

   simple simon ();

   dummy dum0 (.dm(inf2[1]), .simon(simon));
   
endmodule


// drain module
module drain (
   input logic    clk,
   input logic    rst,
   handshake.dir2 inf1,
   handshake.dir1 inf2[1:0]
);

   assign inf2[0].port1 = inf1.port1;

endmodule

// dummy module
module dummy (
   handshake.dir1 dm,
   simple.dir1 simon
);

   assign dm.port1 = simon.simple1;

endmodule
