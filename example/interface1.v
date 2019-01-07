// Cut down system verilog test

module interface1 (input clk, input rst, output [31:0] test1);

   // interface instance
   simple simon0(), simon1[1:0]();

   submodule x1(.conn(simon0));
   
   assign test1 = simon1[0].simple2 ^ simon1[1].simple2;
   
endmodule

module submodule (simple.dir1 conn);

   assign conn.simple1 = 32'h55AA;
   
   subsubmodule x1(.conn(conn), .rslt());
      
endmodule // submodule

module subsubmodule (simple.dir1 conn, output [31:0] rslt);

   assign rslt = conn.simple2;
   
endmodule // submodule

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
