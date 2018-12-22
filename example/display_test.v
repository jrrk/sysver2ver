module display_test(input [7:0] ch);

   initial
     $write("%c", ch);

endmodule // display_test
