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
| RDT of string * string * string * string * rw list
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
| PKG of string * rw list
| CAT of rw list
| EXT of rw list
| CPS of rw list
| CND of rw list
| REPL of string * rw list
| MODUL of string * (string*string) list * rw list
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
| FIL of string
| NL of rw list
| CELLS of rw list
| CELL of string * string * string *rw list
| POSNEG of string*string
| COMB
| INITIAL
| FINAL
| UNKNOWN

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
| Xml.Element ("file", [("id", _); ("filename", nam); ("language", "1800-2017")], []) -> FIL (nam)
| Xml.Element ("netlist", [], xlst) -> NL (List.map (rw' errlst) xlst)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
               IO (nam, int_of_string tid, dir, typ, List.map (rw' errlst) xlst)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               VAR (nam, int_of_string tid, typ)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
               [Xml.Element ("const", [("fl", _); ("name", lev); ("dtype_id", cid)], [])]) ->
                             IVAR (nam, int_of_string tid, lev, int_of_string cid)
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
| Xml.Element ("package", [("fl", _); ("name", nam); ("origName", nam')], xlst) -> PKG (nam, List.map (rw' errlst) xlst)
| Xml.Element ("typedef", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> TYP (nam, List.map (rw' errlst) xlst)
| Xml.Element ("func", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> FNC (nam, List.map (rw' errlst) xlst)
| Xml.Element ("jumplabel", [("fl", _)], xlst) -> JMPL (List.map (rw' errlst) xlst)
| Xml.Element ("jumpgo", [("fl", _)], xlst) -> JMPG (List.map (rw' errlst) xlst)
| Xml.Element ("concat", [("fl", _); ("dtype_id", tid)], xlst) -> CAT (List.map (rw' errlst) xlst)
| Xml.Element ("cvtpackstring", [("fl", _); ("dtype_id", tid)], xlst) -> CPS (List.map (rw' errlst) xlst)
| Xml.Element ("cond", [("fl", _); ("dtype_id", tid)], xlst) -> CND (List.map (rw' errlst) xlst)
| Xml.Element ("sformatf", [("fl", _); ("name", fmt); ("dtype_id", tid)], xlst) -> SFMT (fmt, List.map (rw' errlst) xlst)
| Xml.Element ("module", ("fl", _) :: ("name", nam) :: attr, xlst) -> MODUL (nam, attr, List.map (rw' errlst) xlst)
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
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype" as dtyp, ("fl", _) :: ("id", num) :: ("name", nam) :: rnglst, xlst) -> DT (dtyp, num, nam, rnglst, List.map (rw' errlst) xlst)
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp, [("fl", _); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) -> RDT (dtyp, num, nam, subtype, List.map (rw' errlst) xlst)
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp, [("fl", _); ("id", num); ("sub_dtype_id", subtype)], xlst) -> RDT (dtyp, num, "", subtype, List.map (rw' errlst) xlst)
| Xml.Element ("enumitem" as dtyp, [("fl", _); ("name", nam); ("dtype_id", num)], xlst) -> RDT (dtyp, nam, "", num, List.map (rw' errlst) xlst)
| Xml.Element ("cells", [], xlst) -> CELLS(List.map (rw' errlst) xlst)
| Xml.Element ("cell", [("fl", _); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    CELL(nam, subnam, hier, List.map (rw' errlst) xlst)
| Xml.Element ("display", [("fl", _)], xlst) -> DSPLY (List.map (rw' errlst) xlst)
| Xml.Element (("fopen"|"fclose"|"readmem"|"typetable" as sys), [("fl", _)], xlst) -> SYS (sys, List.map (rw' errlst) xlst)
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> errlst := err :: !errlst; failwith str

type itms = { 
  top: bool;
  io: (string*int*string*string*string list) list ref;
  v: (string*int*string*int) list ref;
  ca: (string*string) list ref;
  typ: string list ref;
  alwys: (rw*string list) list ref;
  init: (rw*string list) list ref;
  bgn: (string*itms) list ref;
  func: (string*string list) list ref;
  gen: (string list) list ref;
  inst: (string*string*string list) list ref;
}

let modules = Hashtbl.create 255
let packages = Hashtbl.create 255
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
| Ceqwild -> "==="
| Cneqwild -> "!=="
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

let cexp exp = Scanf.sscanf exp "%d'h%x" (fun b n -> (b,n))

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
| XRF (id, []) -> " (* "^id^" *) "
| oth -> exprothlst := oth :: !exprothlst; failwith "exprothlst"

let rec portconn = function
| VRF (id, []) -> "."^id
| PORT (id_o, dir, idx, []) -> "."^id_o^" (*"^dir^","^string_of_int idx^"*)"
| PORT (id_i, dir, idx, expr1 :: []) -> "."^id_i^"("^expr expr1^") (*"^dir^","^string_of_int idx^"*)"
| RNG [CNST (lft, []); CNST (rght, [])] -> "(* ["^lft^":"^rght^"] *)"
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst, []) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

let rec stmt = function
| BGN(str1, rw_lst) -> " begin "^String.concat ";\n\t" (List.map stmt rw_lst)^" end "
| IF(cnd :: rw_lst) -> "if ("^expr cnd^") "^String.concat ";\n\t" (List.map stmt rw_lst)
| ASGNDLY(src :: dst :: []) -> expr dst ^"<="^expr src
| ASGN (src :: dst :: []) -> expr dst ^"<="^expr src
| CS (sel :: lst) -> "case ("^expr sel^") "^String.concat ";\n\t" (List.map (function
    | CSITM (cexp :: st :: []) -> "case "^expr cexp^": "^stmt st
    | CSITM (st :: []) -> "default: "^stmt st
    | oth -> csothlst := oth :: !csothlst; failwith "csothlst") lst)
| CA(rght::lft::[]) -> "assign "^expr lft^" = "^expr rght
| VAR (id, _, kind) -> kind^" "^id
| WHL (cnd :: stmts) -> "while ("^expr cnd^") "^String.concat ";\n\t" (List.map stmt stmts)
| DSPLY (SFMT (fmt, arglst) :: []) -> "$display("^fmt^", "^String.concat ", " (List.map expr arglst)^")"
| DSPLY (SFMT (fmt, arglst) :: expr1 :: []) -> "$fdisplay("^expr expr1^", "^fmt^", "^String.concat ", " (List.map expr arglst)^")"

| SYS (fn, arglst) -> "$"^fn^"("^String.concat ", " (List.map expr arglst)^")"
| oth -> stmtothlst := oth :: !stmtothlst; failwith "stmtothlst"

let rec catitm itms = function
| IO(str1, int1, str2, str3, clst) -> itms.io := (str1, int1, str2, str3, List.map ioconn clst) :: !(itms.io)
| VAR(str1, int1, str2) -> itms.v := (str1, int1, str2, -1) :: !(itms.v)
| IVAR(str1, int1, str2, int2) -> itms.v := (str1, int1, str2, int2) :: !(itms.v)
| CA(rght::lft::[]) -> itms.ca := (expr lft, expr rght) :: !(itms.ca)
| TYP(str1, []) -> itms.typ := str1 :: !(itms.typ)
| INST(str1, str2, port_lst) -> itms.inst := (str1, str2, List.map portconn port_lst) :: !(itms.inst)
| ALWYS(SNTRE(SNITM ("POS", [VRF (ck, [])]) :: SNITM ("NEG", [VRF (rst, [])]) :: []) :: rw_lst) ->
     itms.alwys := (POSNEG(ck,rst), List.map stmt rw_lst) :: !(itms.alwys)
| ALWYS(rw_lst) -> itms.alwys := (COMB, List.map stmt rw_lst) :: !(itms.alwys)
| INIT ("initial", rw_lst) -> itms.init := (INITIAL, List.map stmt rw_lst) :: !(itms.init)
| INIT ("final", rw_lst) -> itms.init := (FINAL, List.map stmt rw_lst) :: !(itms.init)
| BGN(str1, rw_lst) ->
    let itms = empty_itms false in
    List.iter (catitm itms) rw_lst;
    itms.bgn := (str1, itms) :: !(itms.bgn)
| FNC(str1, rw_lst) -> itms.func := (str1, List.map stmt rw_lst) :: !(itms.func)
| IF(rw_lst) -> itms.gen := (List.map stmt rw_lst) :: !(itms.gen)
| oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst"

let rec categorise itms = function
| XML(rw_lst) -> catlst itms rw_lst
| DT(str1, str2, str3, attr_list, rw_lst) -> catlst itms rw_lst
| RDT(str1, str2, str3, str4, rw_lst) -> catlst itms rw_lst
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
| MODUL(str1, attr_list, rw_lst) ->
    let top = match attr_list with ("topModule", "1") :: [] -> true | _ -> false in
    let itms = empty_itms top in
    List.iter (catitm itms) rw_lst;
    Hashtbl.add modules str1 itms
| PKG(str1, rw_lst) ->
    let itms = empty_itms false in
    List.iter (catitm itms) rw_lst;
    Hashtbl.add packages str1 itms
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
| FIL(fil) -> print_endline fil
| NL(rw_lst) -> catlst itms rw_lst
| CELLS(rw_lst) -> catlst itms rw_lst
| CELL _ -> ()
| oth -> catothlst := oth :: !catothlst; failwith "catothlst"
and catlst itms lst = List.iter (categorise itms) lst

open Printf

let dump f modul =
  print_endline ("f \""^f^"\";;");
  let fd = open_out (f^"_trans.v") in
  fprintf fd "module %s(" f;
  let delim = ref "" in List.iter (fun (io, idx, dir, kind, lst) ->
                 fprintf fd "%s\n\t%s\t%s\t%s" !delim dir kind io;
                 delim := ",";
                 ) (List.rev !(modul.io));
  fprintf fd "\n);\n\n";
  List.iter (fun (id, idx, kind, n) ->
                 fprintf fd "\t%s\t%s;\n" kind id;
                 ) (List.rev !(modul.v));
  fprintf fd "\n";
  List.iter (fun (dst, src) ->
                 fprintf fd "\tassign %s = %s;\n" dst src;
                 ) (List.rev !(modul.ca));
  fprintf fd "\n";
  List.iter (function
             | (COMB, lst) ->
                 fprintf fd "\talways_comb begin\n";
                 List.iter (fun itm -> fprintf fd "%s\n" itm) (List.rev lst);
                 fprintf fd "\tend\n";
             | (POSNEG (ck, rst), lst) ->
                 fprintf fd "\talways_ff @(posedge %s, negedge %s) begin\n" ck rst;
                 List.iter (fun itm -> fprintf fd "%s\n" itm) (List.rev lst);
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

let translate xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    let (line,range) = match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos) | None -> (0, (0,0)) in
    let errlst = ref [] in
    let rwxml = rw' errlst xml in
    categorise (empty_itms false) rwxml;
    print_endline "MODULES:";
    Hashtbl.iter dump modules;
    (line,range)
    

