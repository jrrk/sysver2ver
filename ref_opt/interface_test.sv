// This tests SystemVerilog interfaces
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2012 by Iztok Jeras.

module test (input clk, input rst);

   // error counter
   bit err = 0;

   // counters
   int cnt;
   int cnt_src;
   int cnt_drn;
   int inf_cnt;
   
   // add all counters
   assign cnt = cnt_src + cnt_drn + inf_cnt;

   // interface instance
   handshake inf (
      .clk (clk),
      .rst (rst),
      .cnt (inf_cnt)
   );

   // source instance
   source #(
      .RW  (8),
      .RP  (8'b11100001)
   ) source (
      .clk  (clk),
      .rst  (rst),
      .inf  (inf),
      .cnt  (cnt_src)
   );

   // drain instance
   drain #(
      .RW  (8),
      .RP  (8'b11010100)
   ) drain (
      .clk  (clk),
      .rst  (rst),
      .inf  (inf),
      .cnt  (cnt_drn)
   );

endmodule


// interface definition
interface handshake #(
   parameter int unsigned WC = 32
)(
   input logic clk,
   input logic rst,
   output integer cnt  // counter
);

   // modport signals
   logic req;  // request
   logic grt;  // grant
   logic inc;  // increment

   // local signals

   // source
   modport src (
      output req,
      input  grt
   );

   // drain
   modport drn (
      input  req,
      output grt
   );

   // increment condition
   assign inc = req & grt;

   // local logic (counter)
   always @ (posedge clk, posedge rst)
   if (rst) cnt <= '0;
   else     cnt <= cnt + {31'b0,inc};

endinterface


// source module
module source #(
   // random generator parameters
   parameter int unsigned RW=1,   // LFSR width
   parameter bit [RW-1:0] RP='0,  // LFSR polinom
   parameter bit [RW-1:0] RR='1   // LFSR reset state
)(
   input logic    clk,
   input logic    rst,
   handshake.src  inf,
   output integer cnt
);

   // LFSR
   logic [RW-1:0] rnd;

   // LFSR in Galois form
   always @ (posedge clk, posedge rst)
   if (rst) rnd <= RR;
   else     rnd <= {rnd[0], rnd[RW-1:1]} ^ ({RW{rnd[0]}} & RP);

   // counter
   always @ (posedge clk, posedge rst)
   if (rst) cnt <= 32'd0;
   else     cnt <= cnt + {31'b0, inf.req & inf.grt};

   // request signal
   assign inf.req = rnd[0];

endmodule


// drain module
module drain #(
   // random generator parameters
   parameter int unsigned RW=1,   // LFSR width
   parameter bit [RW-1:0] RP='0,  // LFSR polinom
   parameter bit [RW-1:0] RR='1   // LFSR reset state
)(
   input logic    clk,
   input logic    rst,
   handshake.drn  inf,
   output integer cnt
);

   // LFSR
   logic [RW-1:0] rnd;

   // LFSR in Galois form
   always @ (posedge clk, posedge rst)
   if (rst) rnd <= RR;
   else     rnd <= {rnd[0], rnd[RW-1:1]} ^ ({RW{rnd[0]}} & RP);

   // counter
   always @ (posedge clk, posedge rst)
   if (rst) cnt <= 32'd0;
   else     cnt <= cnt + {31'b0, inf.req & inf.grt};

   // grant signal
   assign inf.grt = rnd[0];

endmodule
