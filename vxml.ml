(*
BSD 2-Clause License

Copyright (c) 2018, jrrk
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Printf

type unaryop =
| Unknown
| Unot
| Unegate
| Uextend

type cmpop =
| Cunknown
| Ceq
| Cneq
| Cgt
| Cgts
| Cgte
| Ceqwild
| Cneqwild
| Cltes
| Clte
| Clt
| Clts

type logop =
| Lunknown
| Land
| Lredand
| Lor
| Lredor
| Lxor
| Lredxor
| Lshiftl
| Lshiftr
| Lshiftrs

type arithop =
| Aunknown
| Aadd
| Asub
| Amul
| Amuls

type rw =
| XML of rw list
| DT of string * string * string * (string*string) list * rw list
| RDT of string * int * string * int * rw list
| EITM of string * string * string * int * rw list
| IO of string * int * string * string * rw list
| VAR of string * int * string
| IVAR of string * int * string * int
| CNST of string * rw list
| VRF of string * rw list
| TYP of string * rw list
| FNC of string * rw list
| INST of string * string * rw list
| SFMT of string * rw list
| SYS of string * rw list
| PORT of string * string * int * rw list
| CA of rw list
| UNRY of unaryop * rw list
| SEL of rw list
| ASEL of rw list
| SNITM of string * rw list
| ASGNDLY of rw list
| ASGN of rw list
| ARITH of arithop * rw list
| LOGIC of logop * rw list
| CMP of cmpop * rw list
| FRF of string * rw list
| XRF of string * rw list
| PKG of string * string * rw list
| CAT of rw list
| EXT of rw list
| CPS of rw list
| CND of rw list
| REPL of string * rw list
| MODUL of string * string * (string*string) list * rw list
| BGN of string * rw list
| RNG of rw list
| ALWYS of rw list
| SNTRE of rw list
| IF of rw list
| INIT of string * rw list
| IRNG of rw list
| IFC of string * rw list
| IRDT of string * rw list
| IMP of string * rw list
| IMRF of string * rw list
| JMPL of rw list
| JMPG of rw list
| CS of rw list
| CSITM of rw list
| WHL of rw list
| ARG of rw list
| DSPLY of rw list
| FILS of string * rw list
| FIL of string * string
| NL of rw list
| CELLS of rw list
| CELL of string * string * string * string * rw list
| POSNEG of string*string
| NEGNEG of string*string
| POSEDGE of string
| COMB
| INITIAL
| FINAL
| UNKNOWN

let constnet = function
| "1'h1" -> "supply1"
| "1'h0" -> "supply0"
| oth -> "wire"

let unaryop = function
|"not" -> Unot
|"negate" -> Unegate
|"extend" -> Uextend
| _ -> Unknown

let cmpop = function
|"eq" -> Ceq
|"neq" -> Cneq
|"gt" -> Cgt
|"gts" -> Cgts
|"gte" -> Cgte
|"eqwild" -> Ceqwild
|"neqwild" -> Cneqwild
|"ltes" -> Cltes
|"lte" -> Clte
|"lt" -> Clt
|"lts" -> Clts
| _ -> Cunknown

let logop = function
|"and" -> Land
|"redand" -> Lredand
|"or" -> Lor
|"redor" -> Lredor
|"xor" -> Lxor
|"redxor" -> Lredxor
|"shiftl" -> Lshiftl
|"shiftr" -> Lshiftr
|"shiftrs" -> Lshiftrs
| _ -> Lunknown

let arithop = function
|"add" -> Aadd
|"sub" -> Asub
|"mul" -> Amul
|"muls" -> Amuls
| _ -> Aunknown

let rec rw' errlst = function
| Xml.Element ("verilator_xml", [], xlst) -> XML (List.map (rw' errlst) xlst)
| Xml.Element ("files"|"module_files" as fils, [], xlst) -> FILS (fils, List.map (rw' errlst) xlst)
| Xml.Element ("file", [("id", encoding); ("filename", nam); ("language", "1800-2017")], []) -> FIL (encoding, nam)
| Xml.Element ("netlist", [], xlst) -> NL (List.map (rw' errlst) xlst)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
               IO (nam, int_of_string tid, dir, typ, List.map (rw' errlst) xlst)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               VAR (nam, int_of_string tid, typ)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
               [Xml.Element ("const", [("fl", _); ("name", lev); ("dtype_id", cid)], [])]) ->
                             IVAR (nam, int_of_string tid, constnet lev, int_of_string cid)
| Xml.Element ("const", [("fl", _); ("name", value); ("dtype_id", tid)], xlst) -> CNST (value, List.map (rw' errlst) xlst)
| Xml.Element ("contassign", [("fl", _); ("dtype_id", tid)], xlst) -> CA (List.map (rw' errlst) xlst)
| Xml.Element ("not"|"negate"|"extend" as op, [("fl", _); ("dtype_id", tid)], xlst) -> UNRY (unaryop op, List.map (rw' errlst) xlst)
| Xml.Element ("varref", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> VRF (nam, List.map (rw' errlst) xlst)
| Xml.Element ("instance", [("fl", _); ("name", nam); ("defName", dnam); ("origName", nam')], xlst) ->
               INST (nam, dnam, List.map (rw' errlst) xlst)
| Xml.Element ("range", [("fl", _)], xlst) -> RNG (List.map (rw' errlst) xlst)
| Xml.Element ("port", [("fl", _); ("name", nam); ("direction", dir); ("portIndex", idx)], xlst) ->
               PORT (nam, dir, int_of_string idx, List.map (rw' errlst) xlst)
| Xml.Element ("port", [("fl", _); ("name", nam); ("portIndex", idx)], xlst) ->
               PORT (nam, "N/A", int_of_string idx, List.map (rw' errlst) xlst)
| Xml.Element ("sel", [("fl", _); ("dtype_id", tid)], xlst) -> SEL (List.map (rw' errlst) xlst)
| Xml.Element ("arraysel", [("fl", _); ("dtype_id", tid)], xlst) -> ASEL (List.map (rw' errlst) xlst)
| Xml.Element ("always", [("fl", _)], xlst) -> ALWYS (List.map (rw' errlst) xlst)
| Xml.Element ("sentree", [("fl", _)], xlst) -> SNTRE (List.map (rw' errlst) xlst)
| Xml.Element ("senitem", [("fl", _); ("edgeType", etyp)], xlst) -> SNITM (etyp, List.map (rw' errlst) xlst)
| Xml.Element ("begin", [("fl", _); ("name", namedblk)], xlst) -> BGN (namedblk, List.map (rw' errlst) xlst)
| Xml.Element ("begin", [("fl", _)], xlst) -> BGN ("", List.map (rw' errlst) xlst)
| Xml.Element ("assigndly", [("fl", _); ("dtype_id", tid)], xlst) -> ASGNDLY (List.map (rw' errlst) xlst)
| Xml.Element ("if", [("fl", _)], xlst) -> IF (List.map (rw' errlst) xlst)
| Xml.Element ("add"|"sub"|"mul"|"muls" as op, [("fl", _); ("dtype_id", tid)], xlst) -> ARITH (arithop op, List.map (rw' errlst) xlst)
| Xml.Element ("and"|"redand"|"or"|"redor"|"xor"|"redxor"|"shiftl"|"shiftr"|"shiftrs" as log,
               [("fl", _); ("dtype_id", tid)], xlst) -> LOGIC (logop log, List.map (rw' errlst) xlst)
| Xml.Element ("eq"|"neq"|"gt"|"gts"|"gte"|"eqwild"|"neqwild"|"ltes"|"lte"|"lt"|"lts" as cmp, [("fl", _); ("dtype_id", tid)], xlst) -> CMP (cmpop cmp, List.map (rw' errlst) xlst)
| Xml.Element ("initial"|"final" as action, [("fl", _)], xlst) -> INIT (action, List.map (rw' errlst) xlst)
| Xml.Element ("assign", [("fl", _); ("dtype_id", tid)], xlst) -> ASGN (List.map (rw' errlst) xlst)
| Xml.Element ("package", [("fl", orig); ("name", nam); ("origName", nam')], xlst) -> PKG (orig, nam, List.map (rw' errlst) xlst)
| Xml.Element ("typedef", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> TYP (nam, List.map (rw' errlst) xlst)
| Xml.Element ("func", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> FNC (nam, List.map (rw' errlst) xlst)
| Xml.Element ("jumplabel", [("fl", _)], xlst) -> JMPL (List.map (rw' errlst) xlst)
| Xml.Element ("jumpgo", [("fl", _)], xlst) -> JMPG (List.map (rw' errlst) xlst)
| Xml.Element ("concat", [("fl", _); ("dtype_id", tid)], xlst) -> CAT (List.map (rw' errlst) xlst)
| Xml.Element ("cvtpackstring", [("fl", _); ("dtype_id", tid)], xlst) -> CPS (List.map (rw' errlst) xlst)
| Xml.Element ("cond", [("fl", _); ("dtype_id", tid)], xlst) -> CND (List.map (rw' errlst) xlst)
| Xml.Element ("sformatf", [("fl", _); ("name", fmt); ("dtype_id", tid)], xlst) -> SFMT (fmt, List.map (rw' errlst) xlst)
| Xml.Element ("module", ("fl", origin) :: ("name", nam) :: attr, xlst) -> MODUL (origin, nam, attr, List.map (rw' errlst) xlst)
| Xml.Element ("case", [("fl", _)], xlst) -> CS (List.map (rw' errlst) xlst)
| Xml.Element ("caseitem", [("fl", _)], xlst) -> CSITM (List.map (rw' errlst) xlst)
| Xml.Element ("while", [("fl", _)], xlst) -> WHL (List.map (rw' errlst) xlst)
| Xml.Element ("insiderange", [("fl", _); ("dtype_id", tid)], xlst) -> IRNG (List.map (rw' errlst) xlst)
| Xml.Element ("funcref", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> FRF (nam, List.map (rw' errlst) xlst)
| Xml.Element ("varxref", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> XRF (nam, List.map (rw' errlst) xlst)
| Xml.Element ("arg", [("fl", _)], xlst) -> ARG (List.map (rw' errlst) xlst)
| Xml.Element ("replicate"|"initarray"|"streaml"|"extends"|"powsu" as op, [("fl", _); ("dtype_id", tid)], xlst) -> REPL (op, List.map (rw' errlst) xlst)
| Xml.Element ("iface", [("fl", _); ("name", bus); ("origName", bus')], xlst) -> IFC (bus, List.map (rw' errlst) xlst)
| Xml.Element ("ifacerefdtype", [("fl", _); ("id", num)], xlst) -> IRDT (num, List.map (rw' errlst) xlst)
| Xml.Element ("modport", [("fl", _); ("name", port)], xlst) -> IMP (port, List.map (rw' errlst) xlst)
| Xml.Element ("modportvarref", [("fl", _); ("name", member)], xlst) -> IMRF (member, List.map (rw' errlst) xlst)
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype" as dtyp, ("fl", _) :: ("id", num) :: ("name", nam) :: rnglst, xlst) ->
    DT (dtyp, num, nam, rnglst, List.map (rw' errlst) xlst)
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp, [("fl", _); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) -> RDT (dtyp, int_of_string num, nam, int_of_string subtype, List.map (rw' errlst) xlst)
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp, [("fl", _); ("id", num); ("sub_dtype_id", subtype)], xlst) -> RDT (dtyp, int_of_string num, "", int_of_string subtype, List.map (rw' errlst) xlst)
| Xml.Element ("enumitem" as dtyp, [("fl", _); ("name", nam); ("dtype_id", num)], xlst) -> EITM (dtyp, nam, "", int_of_string num, List.map (rw' errlst) xlst)
| Xml.Element ("cells", [], xlst) -> CELLS(List.map (rw' errlst) xlst)
| Xml.Element ("cell", [("fl", origin); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    CELL(origin, nam, subnam, hier, List.map (rw' errlst) xlst)
| Xml.Element ("display", [("fl", _)], xlst) -> DSPLY (List.map (rw' errlst) xlst)
| Xml.Element (("fopen"|"fclose"|"readmem"|"typetable" as sys), [("fl", _)], xlst) -> SYS (sys, List.map (rw' errlst) xlst)
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> errlst := err :: !errlst; failwith str

type itms = { 
  top: bool;
  io: (string*int*string*string*string list) list ref;
  v: (string*int*string*int) list ref;
  ca: (string*string) list ref;
  typ: string list ref;
  alwys: (rw*rw list) list ref;
  init: (rw*string list) list ref;
  bgn: (string*itms) list ref;
  func: (string*string list) list ref;
  gen: (string list) list ref;
  inst: (string*string*string list) list ref;
}

let modules = Hashtbl.create 255
let packages = Hashtbl.create 255
let files = Hashtbl.create 255
let hierarchy = Hashtbl.create 255
let typetable = Hashtbl.create 255
let empty_itms top = { top=top;
io=ref [];
v=ref [];
ca=ref [];
typ=ref [];
alwys=ref [];
init=ref [];
bgn=ref [];
func=ref [];
gen=ref [];
inst=ref [] }
let exprothlst = ref []
let stmtothlst = ref []
let portothlst = ref []
let iothlst = ref []
let csothlst = ref []
let bgnothlst = ref []
let itmothlst = ref []
let catothlst = ref []
let cellothlst = ref []
let posneglst = ref []
let typothlst = ref []
let memothlst = ref []

let unaryopv = function
| Unknown -> "???"
| Unot -> "!"
| Unegate -> "-"
| Uextend -> "extend"

let cmpopv = function
| Cunknown -> "???"
| Ceq -> "=="
| Cneq -> "!="
| Cgt -> ">"
| Cgts -> ">"
| Cgte -> ">="
| Ceqwild -> "=="
| Cneqwild -> "!="
| Cltes -> "<="
| Clte -> "<="
| Clt -> "<"
| Clts -> "<"

let logopv = function
| Lunknown -> "???"
| Land -> "&"
| Lredand -> "&&"
| Lor -> "|"
| Lredor -> "||"
| Lxor -> "^"
| Lredxor -> "^^"
| Lshiftl -> "<<"
| Lshiftr -> ">>"
| Lshiftrs -> ">>"

let arithopv = function
| Aadd -> "+"
| Asub -> "-"
| Amul -> "*"
| Amuls -> "*"
| Aunknown -> "???"

let cexp exp = try Scanf.sscanf exp "%d'h%x" (fun b n -> (b,n)) with err -> (-1,-1)

let rec expr = function
| VRF (id, []) -> id
| CNST (cexp, []) -> cexp
| UNRY (op, expr1 :: []) -> (unaryopv op^expr expr1)
| CMP (op, expr2 :: expr1 :: []) -> (expr expr1^cmpopv op^expr expr2)
| LOGIC (op, expr1 :: []) -> (logopv op^expr expr1)
| LOGIC (op, expr2 :: expr1 :: []) -> (expr expr1^logopv op^expr expr2)
| ARITH (op, expr2 :: expr1 :: []) -> (expr expr1^arithopv op^expr expr2)
| SEL (expr1 :: strt :: wid :: []) ->
    let (b,n) = cexp (expr wid) and (b',n') = cexp (expr strt) in
    let s = expr expr1^if n = 1 then "["^string_of_int n'^"]"
    else "["^string_of_int (n'+n-1)^":"^string_of_int n'^"]" in
    s
| ASEL (VRF (lval, []) :: expr1 :: []) -> lval^"["^expr expr1^"]"
| CND (expr1 :: lft :: rght :: []) -> expr expr1^" ? "^expr lft^" : "^expr rght
| CAT (expr1 :: expr2 :: []) -> "{"^expr expr1^","^expr expr2^"}"
| FRF (fref, arglst) -> fref^"("^String.concat ", " (List.map (function ARG (arg :: []) -> expr arg | _ -> "???") arglst)^")"
| REPL (kind, arglst) -> kind^"("^String.concat ", " (List.map expr arglst)^")"
| IRNG (expr2 :: expr1 :: []) -> "["^expr expr1^":"^expr expr2^"]"
| XRF (id, []) -> " /* "^id^" */ "
| oth -> exprothlst := oth :: !exprothlst; failwith "exprothlst"

let rec portconn = function
| VRF (id, []) -> "."^id
| PORT (id_o, dir, idx, []) -> "."^id_o^" /*"^dir^","^string_of_int idx^"*/"
| PORT (id_i, dir, idx, expr1 :: []) -> "."^id_i^"("^expr expr1^") /*"^dir^","^string_of_int idx^"*/"
| RNG [CNST (lft, []); CNST (rght, [])] -> "/* ["^lft^":"^rght^"] */"
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst, []) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

let rec stmt = function
| BGN(str1, rw_lst) -> " begin "^String.concat ";\n\t" (List.map stmt rw_lst)^"; end "
| IF(cnd :: then_stmt :: []) -> "if ("^expr cnd^") "^stmt then_stmt
| IF(cnd :: then_stmt :: else_stmt :: []) -> "if ("^expr cnd^") "^stmt then_stmt^" else "^stmt else_stmt
| ASGNDLY(src :: dst :: []) -> expr dst ^"<="^expr src
| ASGN (src :: dst :: []) -> expr dst ^"<="^expr src
| CS (sel :: lst) -> "case ("^expr sel^") "^String.concat "\n\t" (List.map (function
    | CSITM (cexp :: (BGN _ as st) :: []) -> expr cexp^": "^stmt st
    | CSITM (cexp :: st :: []) -> expr cexp^": "^stmt st^";"
    | CSITM (st :: []) -> "default: "^stmt st^";"
    | CSITM (cexplst) -> (match List.rev cexplst with (hd::tl) -> expr (List.hd tl)^": "^stmt hd^";" | [] -> "")
    | oth -> csothlst := oth :: !csothlst; failwith "csothlst") lst)^" endcase\n"
| CA(rght::lft::[]) -> "assign "^expr lft^" = "^expr rght
| VAR (id, _, kind) -> kind^" "^id
| WHL (cnd :: stmts) -> "while ("^expr cnd^") "^String.concat ";\n\t" (List.map stmt stmts)
| DSPLY (SFMT (fmt, arglst) :: []) -> "$display("^fmt^", "^String.concat ", " (List.map expr arglst)^")"
| DSPLY (SFMT (fmt, arglst) :: expr1 :: []) -> "$fdisplay("^expr expr1^", "^fmt^", "^String.concat ", " (List.map expr arglst)^")"
| SYS (fn, arglst) -> "$"^fn^"("^String.concat ", " (List.map expr arglst)^")"
| IO(str1, int1, str2, str3, []) -> str2^" "^str3^" "^str1
| JMPL _ -> "jmpl"
| CNST(cexpr, []) -> cexpr
| oth -> stmtothlst := oth :: !stmtothlst; failwith "stmtothlst"

let rec catitm itms = function
| IO(str1, int1, str2, str3, clst) -> itms.io := (str1, int1, str2, str3, List.map ioconn clst) :: !(itms.io)
| VAR(str1, int1, str2) -> itms.v := (str1, int1, str2, -1) :: !(itms.v)
| IVAR(str1, int1, str2, int2) -> itms.v := (str1, int1, str2, int2) :: !(itms.v)
| CA(rght::lft::[]) -> itms.ca := (expr lft, expr rght) :: !(itms.ca)
| TYP(str1, []) -> itms.typ := str1 :: !(itms.typ)
| INST(str1, str2, port_lst) -> itms.inst := (str1, str2, List.map portconn (List.rev port_lst)) :: !(itms.inst)
| ALWYS(SNTRE(SNITM ("POS", [VRF (ck, [])]) :: []) :: rw_lst) ->
    itms.alwys := (POSEDGE(ck), rw_lst) :: !(itms.alwys)
| ALWYS(SNTRE(SNITM (("POS"|"NEG") as edg, [VRF (ck, [])]) :: SNITM ("NEG", [VRF (rst, [])]) :: []) :: rw_lst) ->
    let rw_lst' = (function
       | BGN(lbl, (IF(VRF(rst',[]) :: thn :: els :: []) :: [])) :: [] ->
           BGN("", (IF(UNRY(Unot, VRF(rst',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | IF(VRF(rst',[]) :: thn :: els :: []) :: [] ->
           BGN("", (IF(UNRY(Unot, VRF(rst',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | oth -> posneglst := oth :: !posneglst; oth) rw_lst in
    itms.alwys := ((match edg with "POS" -> POSNEG(ck,rst) | "NEG" -> NEGNEG(ck,rst) | _ -> UNKNOWN), rw_lst') :: !(itms.alwys)
| ALWYS(rw_lst) -> itms.alwys := (COMB, rw_lst) :: !(itms.alwys)
| INIT ("initial", rw_lst) -> itms.init := (INITIAL, List.map stmt rw_lst) :: !(itms.init)
| INIT ("final", rw_lst) -> itms.init := (FINAL, List.map stmt rw_lst) :: !(itms.init)
| BGN(str1, rw_lst) ->
    let itms = empty_itms false in
    List.iter (catitm itms) rw_lst;
    itms.bgn := (str1, itms) :: !(itms.bgn)
| FNC(str1, rw_lst) -> itms.func := (str1, List.map stmt rw_lst) :: !(itms.func)
| IF(rw_lst) -> itms.gen := (List.map stmt rw_lst) :: !(itms.gen)
| oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst"

let find_source origin =
    let last = ref 0 in
    for i = String.length origin - 1 downto 0 do
        match origin.[i] with '0'..'9' -> last := i | _ -> ();
    done;
    if false then printf "%s: %d\n" origin !last;
    let k = String.sub origin 0 !last in
    let source = if Hashtbl.mem files k then Hashtbl.find files k else "origin_unknown" in
    let line = String.sub origin !last (String.length origin - !last) in
    (source, int_of_string line)

let rec cell_hier = function
| CELL (_, nam, subnam, hier, rw_lst) ->
   let hier_lst = List.flatten (List.map cell_hier rw_lst) in
   Hashtbl.replace hierarchy subnam hier_lst;
   subnam :: hier_lst
| oth -> cellothlst := oth :: !cellothlst; failwith "cellothlst"

let rec categorise itms = function
| XML(rw_lst) -> catlst itms rw_lst
| DT(dtyp, num, nam, attr_list, rw_list) ->
(*
    print_endline (dtyp^":"^num^":"^nam);
*)
    Hashtbl.add typetable (int_of_string num) (dtyp,nam,attr_list,rw_list)
| RDT(str1, str2, str3, str4, rw_lst) -> catlst itms rw_lst
| EITM(str1, str2, str3, str4, rw_lst) -> catlst itms rw_lst
| CNST(str1, rw_lst) -> catlst itms rw_lst
| VRF(str1, rw_lst) -> catlst itms rw_lst
| FNC(str1, rw_lst) -> catlst itms rw_lst
| SFMT(str1, rw_lst) -> catlst itms rw_lst
| SYS(str1, rw_lst) -> catlst itms rw_lst
| PORT(str1, str2, str3, rw_lst) -> catlst itms rw_lst
| UNRY(str1, rw_lst) -> catlst itms rw_lst
| SEL(rw_lst) -> catlst itms rw_lst
| ASEL(rw_lst) -> catlst itms rw_lst
| SNITM(str1, rw_lst) -> catlst itms rw_lst
| ASGN(rw_lst) -> catlst itms rw_lst
| ARITH(str1, rw_lst) -> catlst itms rw_lst
| LOGIC(str1, rw_lst) -> catlst itms rw_lst
| CMP(str1, rw_lst) -> catlst itms rw_lst
| FRF(str1, rw_lst) -> catlst itms rw_lst
| XRF(str1, rw_lst) -> catlst itms rw_lst
| CAT(rw_lst) -> catlst itms rw_lst
| EXT(rw_lst) -> catlst itms rw_lst
| CPS(rw_lst) -> catlst itms rw_lst
| CND(rw_lst) -> catlst itms rw_lst
| REPL(str1, rw_lst) -> catlst itms rw_lst
| MODUL(origin, str1, attr_list, rw_lst) ->
    let (source, line) = find_source origin in
    let top = match attr_list with ("topModule", "1") :: [] -> true | _ -> false in
    let itms = empty_itms top in
    List.iter (catitm itms) rw_lst;
    Hashtbl.add modules str1 (source, line, itms)
| PKG(origin, str1, rw_lst) ->
    let (source, line) = find_source origin in
    let itms = empty_itms false in
    List.iter (catitm itms) rw_lst;
    Hashtbl.add packages str1 (source, line, itms)
| RNG(rw_lst) -> catlst itms rw_lst
| SNTRE(rw_lst) -> catlst itms rw_lst
| IRNG(rw_lst) -> catlst itms rw_lst
| IFC(str1, rw_lst) -> catlst itms rw_lst
| IRDT(str1, rw_lst) -> catlst itms rw_lst
| IMP(str1, rw_lst) -> catlst itms rw_lst
| IMRF(str1, rw_lst) -> catlst itms rw_lst
| JMPL(rw_lst) -> catlst itms rw_lst
| JMPG(rw_lst) -> catlst itms rw_lst
| CS(rw_lst) -> catlst itms rw_lst
| CSITM(rw_lst) -> catlst itms rw_lst
| WHL(rw_lst) -> catlst itms rw_lst
| ARG(rw_lst) -> catlst itms rw_lst
| FILS(str1, rw_lst) -> catlst itms rw_lst
| FIL(enc, fil) -> Hashtbl.add files enc fil
| NL(rw_lst) -> catlst itms rw_lst
| CELLS(rw_lst) -> ignore (List.map cell_hier rw_lst)
| TYP(id, []) -> ()
| VAR(id, n, id_t) -> ()
| oth -> catothlst := oth :: !catothlst; failwith "catothlst"
and catlst itms lst = List.iter (categorise itms) lst

let fold1 fn = function
| [] -> 0 (* should never occur, just to keep type system happy *)
| (hd::tl) -> List.fold_left fn hd tl

let rec cntbasic = function
| ("structdtype",_,[],rw_lst) -> fold1 (+) (List.map cntmembers rw_lst)
| ("uniondtype",_,[],rw_lst) -> fold1 (max) (List.map cntmembers rw_lst)
| ("basicdtype", ("logic"|"integer"|"int"), [("left", hi); ("right", lo)], []) -> (int_of_string hi) - (int_of_string lo) + 1
| ("basicdtype", "logic", [], []) -> 1
| oth -> typothlst := oth :: !typothlst; failwith "typothlst"

and cntmembers = function
| RDT ("memberdtype", idx1, field1, idx2, []) -> findmembers idx2
| oth -> memothlst := oth :: !memothlst; failwith "cntmembers"

and findmembers idx = if Hashtbl.mem typetable idx then
    let wid' = cntbasic (Hashtbl.find typetable idx) in
    wid' else 1

let dump f (source, line, modul) =
  let srcpath = "/local/scratch/jrrk2/ariane-vcs-regression/ariane" in
  let outnam f = f^"_translate.v" in
  let outtcl = "./"^f^"_fm.tcl" in
  if true then print_endline ("f \""^f^"\";; /* "^outnam f^" versus "^source^":"^string_of_int line^" "^outtcl^" */");
  let fd = open_out outtcl in
  fprintf fd "#!/opt/synopsys/fm_vO-2018.06-SP3/bin/fm_shell -f\n";
  fprintf fd "read_sverilog -container r -libname WORK -12 { ";
  let plst = ref [] in Hashtbl.iter (fun _ (s,_,_) -> plst := s :: !plst) packages;
  let iflst = if Hashtbl.mem hierarchy f then Hashtbl.find hierarchy f else [] in
  let hlst = List.sort_uniq compare (source :: List.map (fun k -> let (s,l,_) = if Hashtbl.mem modules k then Hashtbl.find modules k else ("not_found", 0, empty_itms false) in s) iflst) in
  let slst = !plst @ hlst in
(*
 let cnt = ref 0 in List.iter (fun src -> incr cnt; print_endline (string_of_int !cnt^":"^src)) slst;
*)
  List.iter (fun src -> fprintf fd "%s/%s " srcpath src) slst;
  fprintf fd " } \n";
  fprintf fd "set_top r:/WORK/%s\n" f;
  fprintf fd "read_sverilog -container i -libname WORK -12 {";
  let hlst' = List.sort_uniq compare (f :: iflst) in
  List.iter (fun nam -> fprintf fd "%s " (outnam nam)) hlst';
  fprintf fd " } \n";
  fprintf fd "set_top i:/WORK/%s\n" f;
  fprintf fd "match\n";
  fprintf fd "verify\n";
  fprintf fd "quit\n";
  close_out fd;
  Unix.chmod outtcl 0o740;
  let fd = open_out (outnam f) in
  fprintf fd "module %s(" f;
  let delim = ref "" in List.iter (fun (io, idx, dir, kind, lst) -> let wid = findmembers idx in
                 if wid > 1 then
                   fprintf fd "%s\n\t%s\tlogic [%d:0]\t%s" !delim dir (wid-1) io
                 else
                 fprintf fd "%s\n\t%s\t%s\t%s" !delim dir kind io;
                 delim := ",";
                 ) (List.rev !(modul.io));
  fprintf fd "\n);\n\n";
  List.iter (fun (id, idx, kind, n) -> let wid = findmembers idx in
                 if wid > 1 then
                   fprintf fd "\tlogic [%d:0]\t%s;\n" (wid-1) id
                 else fprintf fd "\t%s\t%s;\n" kind id;
                 ) (List.rev !(modul.v));
  fprintf fd "\n";
  List.iter (fun (dst, src) ->
                 fprintf fd "\tassign %s = %s;\n" dst src;
                 ) (List.rev !(modul.ca));
  fprintf fd "\n";
  List.iter (function
             | (COMB, lst) ->
                 fprintf fd "\talways @* begin\n";
                 List.iter (fun itm -> fprintf fd "%s;\n" itm) (List.map stmt lst);
                 fprintf fd "\tend\n";
             | (POSNEG (ck, rst), lst) ->
                 fprintf fd "\talways @(posedge %s, negedge %s) begin\n" ck rst;
                 List.iter (fun itm -> fprintf fd "%s;\n" itm) (List.map stmt lst);
                 fprintf fd "\tend\n";
             | (NEGNEG (ck, rst), lst) ->
                 fprintf fd "\talways @(negedge %s, negedge %s) begin\n" ck rst;
                 List.iter (fun itm -> fprintf fd "%s;\n" itm) (List.map stmt lst);
                 fprintf fd "\tend\n";
             | (POSEDGE (ck), lst) ->
                 fprintf fd "\talways @(posedge %s) begin\n" ck;
                 List.iter (fun itm -> fprintf fd "%s;\n" itm) (List.map stmt lst);
                 fprintf fd "\tend\n";
             | (_, lst) -> fprintf fd "\t// not implemented\n";
                 ) (List.rev !(modul.alwys));
  fprintf fd "\n";
  fprintf fd "\n";
  List.iter (fun (inst, kind, lst) ->
                 fprintf fd "\t%s %s (" kind inst;
                 let delim = ref "" in List.iter (fun term -> fprintf fd "%s\n\t%s" !delim term; delim := ",") (List.rev lst);
                 fprintf fd "\n\t);\n";
                 ) (List.rev !(modul.inst));
  fprintf fd "\n";
  fprintf fd "endmodule\n";
  fprintf fd "\n";
  close_out fd

let translate errlst xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    let (line,range) = match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos) | None -> (0, (0,0)) in
    let rwxml = rw' errlst xml in
    categorise (empty_itms false) rwxml;
    print_endline "MODULES:";
    Hashtbl.iter dump modules;
    (line,range)
    

