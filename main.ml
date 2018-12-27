open Vxml;;

let errlst = ref [] in
Printexc.record_backtrace true;
let (line,range,rwxml,xml,mods) = Vxml.translate (ref []) Sys.argv.(1) in
List.iter (fun l -> Printf.printf "%d:" (List.length l)) [
!exprothlst;
!stmtothlst;
!portothlst;
!iothlst;
!csothlst;
!bgnothlst;
!itmothlst;
!catothlst;
!cellothlst;
List.flatten (!posneglst);
];
Printf.printf "%d\n" (List.length !errlst);
if (line > 0) then Printf.printf "%s:line %d:char %d-%d\n" Sys.argv.(1) line (fst range) (snd range);
()

