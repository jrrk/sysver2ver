open Vxml;;

let errlst = ref [] in
let (line,range,rwxml,xml) = Vxml.translate (ref []) Sys.argv.(1) in
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
List.flatten (List.map (fun (_,_,_,rw) -> rw) !typothlst);
!memothlst
];
Printf.printf "%d\n" (List.length !errlst);
if (line > 0) then Printf.printf "%s:line %d:char %d-%d\n" Sys.argv.(1) line (fst range) (snd range);
()

