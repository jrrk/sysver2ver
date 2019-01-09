module sys;

   int f;

   initial begin
      f = $fopen("trace_hart_00.dasm", "w");
   end

endmodule // sys
