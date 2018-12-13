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
| Cgtes
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
| Lxnor
| Lredxor
| Lredxnor
| Lshiftl
| Lshiftr
| Lshiftrs

type arithop =
| Aunknown
| Aadd
| Asub
| Amul
| Amuls

type dirop = 
| Dunknown
| Dinput
| Doutput
| Dinout
| Dvif
| Dinam of string 
| Dport of (string * int * dirop * string * string list)

type rw =
| UNKNOWN
| XML of rw list
| DT of string * string * string * (string*string) list * rw list
| RDT of string * int * string * int * rw list
| EITM of string * string * string * int * rw list
| IO of string * int * dirop * string * rw list
| VAR of string * int * string
| IVAR of string * int * rw list * int
| CNST of string * int * rw list
| VRF of string * rw list
| TYP of string * rw list
| FNC of string * int * rw list
| TASK of string * string * rw list
| INST of string * (string * rw list)
| SFMT of string * rw list
| SYS of string * rw list
| PORT of string * dirop * int * rw list
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
| XRF of string * string * string * rw list
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
| IFC of string * string * rw list
| IMP of string * rw list
| IMRF of string * string * rw list
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
| POSPOS of string*string
| POSNEG of string*string
| NEGNEG of string*string
| POSEDGE of string
| COMB
| INITIAL
| FINAL
| MODPORTFTR of string

type typmap =
| TYPNONE
| SUBTYP of int
| TYPRNG of int*int
| TYPMEMBER of int*string*int
| TYPENUM of string * int * string

let constnet = function
| "1'h1" -> "supply1"
| "1'h0" -> "supply0"
| oth -> "wire"

let dirop = function
| "output" -> Doutput
| "input" -> Dinput
| "inout" -> Dinout
| "out" -> Doutput
| "in" -> Dinput
| oth -> failwith oth

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
|"gtes" -> Cgtes
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
|"xnor" -> Lxnor
|"redxor" -> Lredxor
|"redxnor" -> Lredxnor
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

let chkvif nam =
       let m = "__Viftop" in
       let lm = String.length m in
       let l = String.length nam in
       let vif = l > lm && String.sub nam (l-lm) lm = m in
       (vif, if vif then String.sub nam 0 (l-lm) else nam)

let rec rw' errlst = function
| Xml.Element ("verilator_xml", [], xlst) -> XML (List.map (rw' errlst) xlst)
| Xml.Element ("files"|"module_files" as fils, [], xlst) -> FILS (fils, List.map (rw' errlst) xlst)
| Xml.Element ("file", [("id", encoding); ("filename", nam); ("language", "1800-2017")], []) -> FIL (encoding, nam)
| Xml.Element ("netlist", [], xlst) -> NL (List.rev(List.map (rw' errlst) xlst))
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
               IO (nam, int_of_string tid, dirop dir, typ, List.map (rw' errlst) xlst)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", ("ifaceref" as typ)); ("origName", nam')], []) ->
	       let (vif, sub) = chkvif nam in
	       if vif then
	       VAR (sub, int_of_string tid, typ)
	       else
	       IO (nam, int_of_string tid, Dunknown, typ, [])
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               VAR (nam, int_of_string tid, typ)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
               [Xml.Element ("const", [("fl", _); ("name", _); ("dtype_id", cid)], []) as lev]) ->
                             IVAR (nam, int_of_string tid, [rw' errlst lev], int_of_string cid)
| Xml.Element ("var", [("fl", _); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
	       [Xml.Element ("initarray", [("fl", _); ("dtype_id", cid)], initlst)]) ->
                             IVAR (nam, int_of_string tid, List.map (rw' errlst) initlst, int_of_string cid)
| Xml.Element ("const", [("fl", _); ("name", value); ("dtype_id", tid)], xlst) -> CNST (value, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("contassign", [("fl", _); ("dtype_id", tid)], xlst) -> CA (List.map (rw' errlst) xlst)
| Xml.Element ("not"|"negate"|"extend" as op, [("fl", _); ("dtype_id", tid)], xlst) -> UNRY (unaryop op, List.map (rw' errlst) xlst)
| Xml.Element ("varref", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> VRF (snd (chkvif nam), List.map (rw' errlst) xlst)
| Xml.Element ("instance", [("fl", _); ("name", nam); ("defName", dnam); ("origName", nam')], xlst) ->
               INST (nam, (dnam, List.map (rw' errlst) xlst))
| Xml.Element ("range", [("fl", _)], xlst) -> RNG (List.map (rw' errlst) xlst)
| Xml.Element ("port", [("fl", _); ("name", nam); ("direction", dir); ("portIndex", idx)], xlst) ->
               PORT (nam, dirop dir, int_of_string idx, List.map (rw' errlst) xlst)
| Xml.Element ("port", [("fl", _); ("name", nam); ("portIndex", idx)], xlst) -> let (vif,sub) = chkvif nam in
               PORT (sub, Dvif, int_of_string idx, List.map (rw' errlst) xlst)
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
| Xml.Element ("and"|"redand"|"or"|"redor"|"xor"|"redxor"|"xnor"|"redxnor"|"shiftl"|"shiftr"|"shiftrs" as log,
               [("fl", _); ("dtype_id", tid)], xlst) -> LOGIC (logop log, List.map (rw' errlst) xlst)
| Xml.Element ("eq"|"neq"|"gt"|"gts"|"gte"|"gtes"|"eqwild"|"neqwild"|"ltes"|"lte"|"lt"|"lts" as cmp, [("fl", _); ("dtype_id", tid)], xlst) -> CMP (cmpop cmp, List.map (rw' errlst) xlst)
| Xml.Element ("initial"|"final" as action, [("fl", _)], xlst) -> INIT (action, List.map (rw' errlst) xlst)
| Xml.Element ("assign", [("fl", _); ("dtype_id", tid)], xlst) -> ASGN (List.map (rw' errlst) xlst)
| Xml.Element ("package", [("fl", orig); ("name", nam); ("origName", nam')], xlst) -> PKG (orig, nam, List.map (rw' errlst) xlst)
| Xml.Element ("typedef", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> TYP (nam, List.map (rw' errlst) xlst)
| Xml.Element ("func", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> FNC (nam, int_of_string tid, List.map (rw' errlst) xlst)
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
| Xml.Element ("varxref", [("fl", _); ("name", nam); ("dtype_id", tid); ("dotted", dotted)], xlst) ->
    XRF (nam, tid, dotted, List.map (rw' errlst) xlst)
| Xml.Element ("arg", [("fl", _)], xlst) -> ARG (List.map (rw' errlst) xlst)
| Xml.Element ("replicate"|"initarray"|"streaml"|"extends"|"powsu" as op, [("fl", _); ("dtype_id", tid)], xlst) -> REPL (op, List.map (rw' errlst) xlst)
| Xml.Element ("iface", [("fl", src); ("name", bus); ("origName", bus')], xlst) -> IFC (src, bus, List.map (rw' errlst) xlst)
| Xml.Element ("ifacerefdtype" as ifr, [("fl", _); ("id", num); ("modportname", nam)], xlst) -> DT (ifr, num, nam, [], List.map (rw' errlst) xlst)
| Xml.Element ("modport", [("fl", _); ("name", port)], xlst) -> IMP (port, List.map (rw' errlst) xlst)
| Xml.Element ("modportvarref", [("fl", _); ("name", member); ("direction", dir)], xlst) -> IMRF (member, dir, List.map (rw' errlst) xlst)
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype" as dtyp, ("fl", _) :: ("id", num) :: rnglst, xlst) -> (match rnglst with
    | ("name", nam) :: tl -> DT (dtyp, num, nam, tl, List.map (rw' errlst) xlst)
    | _ -> DT (dtyp, num, "", rnglst, List.map (rw' errlst) xlst))
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp, [("fl", _); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) -> RDT (dtyp, int_of_string num, nam, int_of_string subtype, List.map (rw' errlst) xlst)
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp, [("fl", _); ("id", num); ("sub_dtype_id", subtype)], xlst) -> RDT (dtyp, int_of_string num, "", int_of_string subtype, List.map (rw' errlst) xlst)
| Xml.Element ("enumitem" as dtyp, [("fl", _); ("name", nam); ("dtype_id", num)], xlst) -> EITM (dtyp, nam, "", int_of_string num, List.map (rw' errlst) xlst)
| Xml.Element ("cells", [], xlst) -> CELLS(List.map (rw' errlst) xlst)
| Xml.Element ("cell", [("fl", origin); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    CELL(origin, nam, subnam, hier, List.map (rw' errlst) xlst)
| Xml.Element ("display", [("fl", _)], xlst) -> DSPLY (List.map (rw' errlst) xlst)
| Xml.Element (("fopen"|"fclose"|"readmem"|"typetable" as sys), [("fl", _)], xlst) -> SYS (sys, List.map (rw' errlst) xlst)
| Xml.Element (("task"|"taskref") as tsk, [("fl", _); ("name", nam)], xlst) -> TASK(tsk, nam, List.map (rw' errlst) xlst)
| Xml.Element ("modportftaskref", [("fl", _); ("name", nam)], []) -> MODPORTFTR nam
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> errlst := err :: !errlst; failwith str

type itms = { 
  top: bool;
  io: (string*int*dirop*string*string list) list ref;
  v: (string*(int*string*int)) list ref;
  iv: (string*(int*rw list*int)) list ref;
  ir: (string*int) list ref;
  ca: (string*string) list ref;
  typ: string list ref;
  alwys: (rw*rw list) list ref;
  init: (rw*rw list) list ref;
  func: (string*int*rw list) list ref;
  task: (string*rw list) list ref;
  gen: (rw list) list ref;
  imp : (string*string) list list ref;
  inst: (string*(string*rw list)) list ref;
}

let modules = Hashtbl.create 255
let modules_opt = Hashtbl.create 255
let packages = Hashtbl.create 255
let interfaces = Hashtbl.create 255
let files = Hashtbl.create 255
let hierarchy = Hashtbl.create 255
let (typetable : (int, string*string*typmap*typmap list) Hashtbl.t) = Hashtbl.create 255
let top = ref []
let empty_itms top = { top=top;
io=ref [];
v=ref [];
iv=ref [];
ir=ref [];
ca=ref [];
typ=ref [];
alwys=ref [];
init=ref [];
func=ref [];
task=ref [];
gen=ref [];
imp=ref [];
inst=ref [] }
let copy_itms prev = { top=prev.top;
io=ref !(prev.io);
v=ref !(prev.v);
iv=ref !(prev.iv);
ir=ref !(prev.ir);
ca=ref !(prev.ca);
typ=ref !(prev.typ);
alwys=ref !(prev.alwys);
init=ref !(prev.init);
func=ref !(prev.func);
task=ref !(prev.task);
gen=ref !(prev.gen);
imp=ref !(prev.imp);
inst=ref !(prev.inst) }
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
let mapothlst = ref []
let subothlst = ref []

let unaryopv = function
| Unknown -> "???"
| Unot -> " ~ "
| Unegate -> " - "
| Uextend -> "???"

let cmpopv = function
| Cunknown -> "???"
| Ceq -> " == "
| Cneq -> " != "
| Cgt -> " > "
| Cgts -> " > "
| Cgte -> " >= "
| Cgtes -> " >= "
| Ceqwild -> " == "
| Cneqwild -> " != "
| Cltes -> " <= "
| Clte -> " <= "
| Clt -> " < "
| Clts -> " < "

let logopv = function
| Lunknown -> "???"
| Land -> " & "
| Lredand -> " & "
| Lor -> " | "
| Lredor -> " | "
| Lxor -> " ^ "
| Lxnor -> " ~^ "
| Lredxor -> " ^ "
| Lredxnor -> " ~^ "
| Lshiftl -> " << "
| Lshiftr -> " >> "
| Lshiftrs -> " >> "

let arithopv = function
| Aadd -> "+"
| Asub -> "-"
| Amul -> "*"
| Amuls -> "*"
| Aunknown -> "???"

let diropv = function
| Dinput -> "input"
| Doutput -> "output"
| Dinout -> "inout"
| Dvif -> "vif"
| Dinam str -> str
| Dport _ -> "ifport"
| Dunknown -> "inout"

let cexp exp = try Scanf.sscanf exp "%d'h%x" (fun b n -> (b,n)) with err ->
    try Scanf.sscanf exp "%d'sh%x" (fun b n -> (b,n)) with err -> (-1,-1)

let rec expr = function
| VRF (id, []) -> id
| CNST (cexp, tid, []) -> cexp
| UNRY (Uextend, expr1 :: []) -> "{1'b0,"^expr expr1^"}"
| UNRY (op, expr1 :: []) -> "("^unaryopv op^expr expr1^")"
| CMP (op, expr1 :: expr2 :: []) -> "("^expr expr1^cmpopv op^expr expr2^")"
| LOGIC (op, expr1 :: []) -> "("^logopv op^expr expr1^")"
| LOGIC (op, expr1 :: expr2 :: []) -> "("^expr expr1^logopv op^expr expr2^")"
| ARITH (op, expr1 :: expr2 :: []) -> "("^expr expr1^arithopv op^expr expr2^")"
| SEL ((VRF _ as expr1) :: (CNST _ as lo) :: (CNST _ as wid) :: []) ->
    let (szlo,lo') = cexp (expr lo) and (szw,wid') = cexp (expr wid) in
    expr expr1^if wid' = 1 then "["^string_of_int lo'^"]"
    else "["^string_of_int (lo'+wid'-1)^":"^string_of_int lo'^"]"
| SEL ((VRF _ as expr1) :: expr2 :: (CNST _ as wid) :: []) ->
    let (szw,wid') = cexp (expr wid) in
    expr expr1^if wid' = 1 then "["^expr expr2^"]"
    else "["^expr expr2^"+:"^string_of_int wid'^"]"
| SEL (expr1 :: CNST ("32'h0", _, []) :: (CNST _ as wid) :: []) -> expr expr1
| ASEL (VRF (lval, []) :: expr1 :: []) -> lval^"["^expr expr1^"]"
| CND (expr1 :: lft :: rght :: []) -> expr expr1^" ? "^expr lft^" : "^expr rght
| CAT (expr1 :: expr2 :: []) -> "{"^expr expr1^","^expr expr2^"}"
| FRF (fref, arglst) -> fref^"("^String.concat ", " (List.map (function ARG (arg :: []) -> expr arg | _ -> "???") arglst)^")"
| REPL (kind, arglst) -> kind^"("^String.concat ", " (List.map expr arglst)^")"
| IRNG (expr2 :: expr1 :: []) -> "["^expr expr1^":"^expr expr2^"]"
| XRF (id, tid, dotted, []) -> dotted^"_"^id
| oth -> exprothlst := oth :: !exprothlst; "NotImplemented" (* failwith "exprothlst" *)

let rec portconn = function
| VRF (id, []) -> "."^id^" /* */"
| PORT (id_o, dir, idx, []) -> "."^id_o^"() /* "^diropv dir^","^string_of_int idx^"*/"
| PORT (id_i, dir, idx, expr1 :: []) -> "."^id_i^"("^expr expr1^") /* "^diropv dir^","^string_of_int idx^"*/"
| RNG [CNST (lft, _, []); CNST (rght, _, [])] -> "/* ["^lft^":"^rght^"] */"
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst, _, []) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

type token =
| SP
| SEMI
| COLON
| COMMA
| AT
| STAR
| NL
| EXPR of string
| BEGIN
| END
| DEFAULT
| LPAREN
| RPAREN
| LBRACK
| RBRACK
| IFF
| ELSE
| ASSIGN
| ASSIGNMENT
| ASSIGNDLY
| CASE
| ENDCASE
| WHILE
| ALWAYS
| POSEDGE
| NEGEDGE
| RETURN

let stmtdly = function
| false -> ASSIGNMENT
| true -> ASSIGNDLY

let outlst = ref []
let tokenout fd = function
| SP -> fprintf fd " "
| SEMI -> fprintf fd ";"
| COLON -> fprintf fd ":"
| COMMA -> fprintf fd ","
| LPAREN -> fprintf fd "("
| RPAREN -> fprintf fd ")"
| LBRACK -> fprintf fd "["
| RBRACK -> fprintf fd "]"
| AT -> fprintf fd "@"
| STAR -> fprintf fd "*"
| NL -> fprintf fd "\n\t"
| DEFAULT -> fprintf fd "default: "
| EXPR str -> fprintf fd "%s " str
| BEGIN -> fprintf fd "begin "
| END -> fprintf fd "end "
| IFF -> fprintf fd "if "
| ELSE -> fprintf fd "else "
| ASSIGN -> fprintf fd "assign "
| ASSIGNMENT  -> fprintf fd " = "
| ASSIGNDLY  -> fprintf fd " <= "
| CASE -> fprintf fd "case "
| ENDCASE -> fprintf fd "endcase "
| WHILE -> fprintf fd "while "
| ALWAYS -> fprintf fd "always "
| POSEDGE -> fprintf fd "posedge "
| NEGEDGE -> fprintf fd "negedge "
| RETURN -> fprintf fd "return "

let reviter lst =
    let delim = ref COLON in
    List.rev (List.flatten (List.map (fun itm -> let lst' = !delim :: EXPR (expr itm) :: [] in delim := COMMA; lst') lst))

let rec iter2 dly tok lst =
    let delim = ref [tok] in
    List.flatten (List.map (fun itm -> let lst' = !delim @ cstmt dly itm in delim := [SEMI;NL]; lst') lst)

and csitm dly = function
    | CSITM (cexp :: (BGN _ as st) :: []) -> EXPR (expr cexp) :: COLON :: cstmt dly st @ [NL]
    | CSITM (cexp :: st :: []) -> EXPR (expr cexp) :: COLON :: cstmt dly st @ [SEMI;NL]
    | CSITM (st :: []) -> DEFAULT :: cstmt dly st @ [SEMI]
    | CSITM (cexplst) -> (match List.rev cexplst with
			| ((BGN _ as hd)::tl) -> reviter tl @ cstmt dly hd @ [NL]
			| (hd::tl) -> reviter tl @ cstmt dly hd @ [SEMI; NL]
                        | [] -> [])
    | oth -> csothlst := oth :: !csothlst; failwith "csothlst"
    
and cstmt dly = function
| JMPG [] -> []
| BGN(str1, rw_lst) -> iter2 dly BEGIN rw_lst @ [SEMI;NL;END;NL]
| IF(cnd :: then_stmt :: []) -> IFF :: LPAREN :: EXPR (expr cnd) :: RPAREN :: cstmt dly then_stmt
| IF(cnd :: (BGN _ as then_stmt) :: else_stmt :: []) ->
    IFF :: LPAREN :: EXPR (expr cnd) :: RPAREN :: cstmt dly then_stmt @ [NL;ELSE;NL] @ cstmt dly else_stmt
| IF(cnd :: then_stmt :: else_stmt :: []) ->
    IFF :: LPAREN :: EXPR (expr cnd) :: RPAREN :: cstmt dly then_stmt @ [SEMI;NL;ELSE;NL] @ cstmt dly else_stmt
| ASGNDLY(src :: dst :: []) -> EXPR ( expr dst) :: (stmtdly dly) :: EXPR (expr src) :: []
| ASGN (src :: dst :: []) -> EXPR ( expr dst) :: (stmtdly dly) :: EXPR (expr src) :: []
| CS (sel :: lst) -> CASE :: LPAREN :: EXPR (expr sel) :: RPAREN :: NL :: List.flatten (List.map (csitm dly) lst) @ [NL;ENDCASE;NL]
| CA(rght::lft::[]) -> ASSIGN :: EXPR (expr lft) :: stmtdly dly :: EXPR (expr rght) :: []
| VAR (id, _, kind) -> EXPR kind :: EXPR id :: []
| WHL (cnd :: stmts) -> WHILE :: LPAREN :: EXPR (expr cnd) :: iter2 dly RPAREN stmts
| DSPLY (SFMT (fmt, arglst) :: []) -> EXPR "$display" :: LPAREN :: EXPR fmt :: COMMA :: reviter arglst @ [LPAREN]
| DSPLY (SFMT (fmt, arglst) :: expr1 :: []) ->
    EXPR "$fdisplay" :: LPAREN :: EXPR (expr expr1) :: COMMA :: EXPR fmt :: COMMA :: reviter arglst @ [LPAREN]
| SYS (fn, arglst) -> EXPR ("$"^fn) :: LPAREN :: EXPR fn :: COMMA :: reviter arglst @ [LPAREN]
| CNST(cexpr, _, []) -> EXPR cexpr :: []
| TASK ("taskref", nam, arglst) -> EXPR nam :: LPAREN :: reviter arglst @ [LPAREN]
| JMPL(rw_lst) -> iter2 dly BEGIN rw_lst @ [NL;END;NL]
| oth -> stmtothlst := oth :: !stmtothlst; failwith "stmtothlst"

let flatten1 dly lst = 
BEGIN :: List.flatten (List.map (cstmt dly) lst) @ [END;NL;NL]

let rec catitm pth itms = function
| IO(str1, int1, Dunknown, "ifaceref", []) ->
    let (dtype, dir, _, _) = Hashtbl.find typetable int1 in
    itms.io := (str1, int1, Dinam dir, "logic", []) :: !(itms.io)
| IO(str1, int1, dir, str3, clst) -> itms.io := (str1, int1, dir, str3, List.map ioconn clst) :: !(itms.io)
| VAR(str1, int1, "ifaceref") -> itms.ir := (str1, int1) :: !(itms.ir)
| VAR(str1, int1, str2) -> itms.v := (str1, (int1, str2, -1)) :: !(itms.v)
| IVAR(str1, int1, rwlst, int2) -> itms.iv := (str1, (int1, rwlst, int2)) :: !(itms.iv)
| CA(rght::lft::[]) -> itms.ca := (expr lft, expr rght) :: !(itms.ca)
| TYP(str1, []) -> itms.typ := str1 :: !(itms.typ)
| INST(str1, (str2, port_lst)) -> let pth = if String.length pth > 0 then pth^"_"^str1 else str1 in
    itms.inst := (pth, (str2, List.rev port_lst)) :: !(itms.inst)
| ALWYS(SNTRE(SNITM ("POS", [VRF (ck, [])]) :: SNITM ("POS", [VRF (rst, [])]) :: []) :: rw_lst) ->
    itms.alwys := (POSPOS(ck,rst), rw_lst) :: !(itms.alwys)    
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
| INIT ("initial", rw_lst) -> itms.init := (INITIAL, rw_lst) :: !(itms.init)
| INIT ("final", rw_lst) -> itms.init := (FINAL, rw_lst) :: !(itms.init)
| BGN(str1, rw_lst) -> let pth' = String.map (function ('A'..'Z' | 'a'..'z' | '0'..'9') as ch -> ch | _ -> '_') str1 in
    List.iter (catitm (pth^"_"^pth') itms) rw_lst
| FNC(str1, idx1, rw_lst) -> itms.func := (str1, idx1, rw_lst) :: !(itms.func)
| IF(rw_lst) -> itms.gen := rw_lst :: !(itms.gen)
| IMP(str1, rw_lst) -> itms.imp := (List.map (function
    | IMRF(str1, str2, []) -> (str1,str2)
    | MODPORTFTR str1 -> (str1,str1)
    | oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst") rw_lst) :: !(itms.imp)
| TASK ("task", str1, rw_lst) -> itms.task := (str1, rw_lst) :: !(itms.task)
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
   (nam,subnam) :: hier_lst
| oth -> cellothlst := oth :: !cellothlst; failwith "cellothlst"

let rec typmap = function
| [] -> TYPNONE
| [("left", lft); ("right", rght)] -> TYPRNG(int_of_string lft, int_of_string rght)
| oth -> mapothlst := oth :: !mapothlst; failwith "mapothlst"

let rec subtypmap = function
| RNG [CNST (cexp1, _, []); CNST (cexp2, _, [])] -> let (b,n) = cexp cexp1 and (b',n') = cexp cexp2 in TYPRNG(n,n')
| EITM ("enumitem", itm, "", n, [CNST (cexp, _, [])]) -> TYPENUM(itm, n, cexp)
| RDT ("memberdtype", idx1, id, idx2, []) -> TYPMEMBER(idx1, id, idx2)
| oth -> subothlst := oth :: !subothlst; failwith "subothlst"

let rec categorise itms = function
| XML(rw_lst) -> catlst itms rw_lst
| DT(dtyp, num, nam, attr_list, rw_lst) ->
    Hashtbl.add typetable (int_of_string num) (dtyp,nam,typmap attr_list,List.map subtypmap rw_lst)
| RDT(dtyp, int1, nam, int2, rw_lst) ->
    Hashtbl.add typetable int1 (dtyp,nam,SUBTYP int2,List.map subtypmap rw_lst)
| EITM(str1, str2, str3, str4, rw_lst) -> catlst itms rw_lst
| CNST(str1, tid, rw_lst) -> catlst itms rw_lst
| VRF(str1, rw_lst) -> catlst itms rw_lst
| FNC(str1, idx1, rw_lst) -> catlst itms rw_lst
| TASK(str1, str2, rw_lst) -> catlst itms rw_lst
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
| XRF(str1, str2, str3, rw_lst) -> catlst itms rw_lst
| CAT(rw_lst) -> catlst itms rw_lst
| EXT(rw_lst) -> catlst itms rw_lst
| CPS(rw_lst) -> catlst itms rw_lst
| CND(rw_lst) -> catlst itms rw_lst
| REPL(str1, rw_lst) -> catlst itms rw_lst
| MODUL(origin, str1, attr_list, rw_lst) ->
    let (source, line) = find_source origin in
    let top = match attr_list with ("topModule", "1") :: [] -> true | _ -> false in
    let itms = empty_itms top in
    List.iter (catitm "" itms) rw_lst;
    Hashtbl.add modules str1 (source, line, itms)
| PKG(origin, str1, rw_lst) ->
    let (source, line) = find_source origin in
    let itms = empty_itms false in
    List.iter (catitm str1 itms) rw_lst;
    Hashtbl.add packages str1 (source, line, itms)
| IFC(origin, str1, rw_lst) ->
    let (source, line) = find_source origin in
    let itms = empty_itms false in
    List.iter (catitm str1 itms) rw_lst;
    Hashtbl.add interfaces str1 (source, line, itms, rw_lst)
| RNG(rw_lst) -> catlst itms rw_lst
| SNTRE(rw_lst) -> catlst itms rw_lst
| IRNG(rw_lst) -> catlst itms rw_lst
| JMPL(rw_lst) -> catlst itms rw_lst
| JMPG(rw_lst) -> catlst itms rw_lst
| CS(rw_lst) -> catlst itms rw_lst
| CSITM(rw_lst) -> catlst itms rw_lst
| WHL(rw_lst) -> catlst itms rw_lst
| ARG(rw_lst) -> catlst itms rw_lst
| FILS(str1, rw_lst) -> catlst itms rw_lst
| FIL(enc, fil) -> Hashtbl.add files enc fil
| NL(rw_lst) -> catlst itms rw_lst
| CELLS(rw_lst) -> top := List.flatten(List.map cell_hier rw_lst)
| TYP(id, []) -> ()
| VAR(id, n, id_t) -> ()
| oth -> catothlst := oth :: !catothlst; failwith "catothlst"
and catlst itms lst = List.iter (categorise itms) lst

let fold1 fn = function
| [] -> 0 (* should never occur, just to keep type system happy *)
| (hd::tl) -> List.fold_left fn hd tl

let rec cntbasic = function
| ("structdtype",_,typmap,rw_lst) -> fold1 (+) (List.map cntmembers rw_lst)
| ("uniondtype",_,typmap,rw_lst) -> fold1 (max) (List.map cntmembers rw_lst)
| ("basicdtype", ("logic"|"integer"|"int"|"bit"), TYPRNG(hi, lo), []) -> hi - lo + 1
| ("basicdtype", ("logic"|"bit"), TYPNONE, []) -> 1
| ("ifacerefdtype", _, TYPNONE, []) -> 0
| ("packarraydtype", "", SUBTYP subtyp, [TYPRNG(n,n')]) -> let subw = findmembers subtyp in subw * (n - n' + 1)
| ("unpackarraydtype", "", SUBTYP subtyp, [TYPRNG (n,n')]) -> let subw = findmembers subtyp in subw * (n - n' + 1)
| oth -> typothlst := oth :: !typothlst; failwith "typothlst"

and cntmembers = function
| TYPMEMBER (idx1, field1, idx2) -> findmembers idx2
| oth -> memothlst := oth :: !memothlst; failwith "memothlst"

and findmembers idx = if Hashtbl.mem typetable idx then
    let wid' = cntbasic (Hashtbl.find typetable idx) in
    wid' else 1

let rec fnstmt dly fd nam delim = function
| IO (io, idx, dir, kind', lst) ->
    let wid = findmembers idx and dir = diropv dir in
    let lst = EXPR !delim :: NL :: EXPR dir :: EXPR "logic" ::
    (if wid > 1 then LBRACK :: EXPR (string_of_int (wid-1)) :: COLON :: EXPR "0" :: RBRACK :: [] else []) @ [SP; EXPR io] in
    delim := ","; lst
| VAR (id, idx, kind') -> let wid = findmembers idx in
  if !delim <> ";\n\t" then delim := ");\n\t";
    let lst = EXPR !delim :: EXPR "logic" ::
    (if wid > 1 then LBRACK :: EXPR (string_of_int (wid-1)) :: COLON :: EXPR "0" :: RBRACK :: [] else []) @ [SP; EXPR id] in
    delim := ";\n\t"; lst
| ASGN (expr1 :: VRF (nam', []) :: []) when nam = nam' ->
    EXPR !delim :: RETURN :: EXPR (expr expr1) :: SEMI :: []
| JMPL(rw_lst) -> BEGIN :: List.flatten (List.map (fnstmt dly fd nam delim) rw_lst) @ [END]
| JMPG [] -> []
| itm ->
  let lst = EXPR !delim :: cstmt dly itm in
  delim := ";\n\t";
  lst

let outnam f = f^"_translate.v"
let outtcl f = "./"^f^"_fm.tcl"

let dumpform f source = 
    let fd = open_out (outtcl f) in
    let srcpath = try Sys.getenv "XMLSRCPATH" with err -> "." in
    fprintf fd "#!/opt/synopsys/fm_vO-2018.06-SP3/bin/fm_shell -f\n";
    fprintf fd "read_sverilog -container r -libname WORK -12 { \\\n";
    let plst = ref [] in Hashtbl.iter (fun _ (s,_,_) -> plst := s :: !plst) packages;
    let iflst = List.map snd (if Hashtbl.mem hierarchy f then Hashtbl.find hierarchy f else []) in
    let hlst = List.sort_uniq compare (source :: List.map (fun k -> let (s,l,_) = if Hashtbl.mem modules k then Hashtbl.find modules k else ("not_found", 0, empty_itms false) in s) iflst) in
    let slst = !plst @ hlst in
    List.iter (fun src -> if src.[0] == '/' then fprintf fd "%s \\\n" src else fprintf fd "%s/%s \\\n" srcpath src) slst;
    fprintf fd "}\n";
    fprintf fd "set_top r:/WORK/%s\n" f;
    fprintf fd "read_sverilog -container i -libname WORK -12 { \\\n";
    let hlst' = List.sort_uniq compare (f :: iflst) in
    List.iter (fun nam -> fprintf fd "%s \\\n" (outnam nam)) hlst';
    fprintf fd "}\n";
    fprintf fd "set_top i:/WORK/%s\n" f;
    fprintf fd "match\n";
    fprintf fd "verify\n";
    fprintf fd "quit\n";
    close_out fd;
    Unix.chmod (outtcl f) 0o740

let dump fd formality f (source, line, modul) =
  if true then print_endline ("f \""^f^"\";; /* "^outnam f^" versus "^source^":"^string_of_int line^" "^outtcl f ^" */");
  fprintf fd "module %s(" f;
  let delim = ref "" in List.iter (fun (io, idx, dir, kind', lst) -> let wid = findmembers idx and dir = diropv dir in
                 if wid > 1 then
                   fprintf fd "%s\n\t%s\tlogic [%d:0]\t%s" !delim dir (wid-1) io
                 else
                 fprintf fd "%s\n\t%s\tlogic\t%s" !delim dir io;
                 delim := ",";
                 ) (List.rev !(modul.io));
  fprintf fd "\n);\n\n";
  List.iter (fun (id, (idx, kind', n)) -> 
                 let wid = findmembers idx in
                 if wid > 1 then
                   fprintf fd "\tlogic [%d:0]\t%s;\n" (wid-1) id
                 else fprintf fd "\tlogic\t%s;\n" id;
                 ) (List.rev !(modul.v));
  fprintf fd "\n";
  List.iter (fun (nam, idx, lst) -> let wid = findmembers idx in
                 fprintf fd "function logic";
                 if wid > 1 then fprintf fd " [%d:0]" (wid-1);
                 fprintf fd " %s " nam;
		 let delim = ref "(" in List.iter (tokenout fd) (List.flatten (List.map (fnstmt false fd nam delim) (List.tl lst)));
		 fprintf fd "\nendfunction\n\n";
                 ) (List.rev !(modul.func));
  List.iter (fun (dst, src) ->
                 fprintf fd "\tassign %s = %s;\n" dst src;
                 ) (List.rev !(modul.ca));
  fprintf fd "\n";
  List.iter (function
    | (COMB, lst) ->
      List.iter (tokenout fd) (ALWAYS :: AT :: STAR :: flatten1 false lst);
    | (POSNEG (ck, rst), lst) ->
      List.iter (tokenout fd) (ALWAYS :: AT :: LPAREN :: POSEDGE :: EXPR ck :: COMMA :: NEGEDGE :: EXPR rst :: RPAREN :: flatten1 true lst);
    | (NEGNEG (ck, rst), lst) ->
      List.iter (tokenout fd) (ALWAYS :: AT :: LPAREN :: NEGEDGE :: EXPR ck :: COMMA :: NEGEDGE :: EXPR rst :: RPAREN :: flatten1 true lst);
    | (POSEDGE (ck), lst) ->
      List.iter (tokenout fd) (ALWAYS :: AT :: LPAREN :: POSEDGE :: EXPR ck :: RPAREN :: flatten1 true lst);
    | (_, lst) -> fprintf fd "\t// not implemented\n";
    ) (List.rev !(modul.alwys));
  fprintf fd "\n";
  fprintf fd "\n";
  List.iter (fun (inst, (kind, lst)) ->
                 fprintf fd "\t%s %s (" kind inst;
                 let delim = ref "" in List.iter (fun term -> fprintf fd "%s\n\t%s" !delim (portconn term); delim := ",") (List.rev lst);
                 fprintf fd "\n\t);\n\n";
                 ) (List.rev !(modul.inst));
  fprintf fd "\nendmodule\n\n"

let rec iterate f (source, line, modul) =
    let newitms = copy_itms modul in
    newitms.ir := [];
    newitms.inst := [];
    List.iter (fun (inst, (kind, iolst)) ->
        if Hashtbl.mem interfaces kind then
           begin
           let (src, lin, intf, _) = Hashtbl.find interfaces kind in
           List.iter (fun (nam, (idx, kind, n)) ->
                 let pth = inst^"_"^nam in
                 newitms.v := (pth, (idx, kind, n)) :: !(newitms.v);
                ) !(intf.v);
           end
        else if Hashtbl.mem modules kind then
           begin
           let (src, lin, itms) = Hashtbl.find modules kind in
           let newiolst = ref [] in
           let newinnerlst = ref [] in
	   let previolst = !(itms.io) in
           List.iter2 (fun ((_, ix, idir, typ, ilst) as inr) -> function
		       | VRF (id, []) ->
                           newiolst := PORT(id, idir, ix, [VRF(id, [])]) :: !newiolst;
                           newinnerlst := inr :: !newinnerlst;
		       | PORT (id_i, Dvif, idx, VRF (id, []) :: []) as pat ->
                           if List.mem_assoc id !(modul.inst) then
			       let (inam,_) = List.assoc id !(modul.inst) in
			       if Hashtbl.mem interfaces inam then (match idir with Dinam iport ->
				  begin
				  let (src, lin, intf, imp) = Hashtbl.find interfaces inam in
				  let imp' = List.filter (function IMP _ -> true | _ -> false) imp in 
                                  let imp'' = List.map (function IMP(str, lst) -> (str,lst) | _ -> ("",[])) imp' in
                                  if List.mem_assoc iport imp'' then
				  List.iter (function IMRF (nam, dir, []) ->
                                        (* print_endline (inam^":"^iport^":"^nam^":"^id_i); *)
                                        let (idx, kind, _) = List.assoc nam !(intf.v) in
					newiolst := PORT(id_i^"_"^nam, dirop dir, idx, [VRF(id^"_"^nam, [])]) :: !newiolst;
				        newinnerlst := (id_i^"_"^nam, idx, dirop dir, typ, ilst) :: !newinnerlst;
				       | _ -> ()) (List.assoc iport imp'')
				  end
                               | _ -> newiolst := pat :: !newiolst; newinnerlst := inr :: !newinnerlst)
			       else
			          begin
			          newiolst := pat :: !newiolst;
				  newinnerlst := inr :: !newinnerlst;
				  end
			    else
			       begin
			       newiolst := pat :: !newiolst;
			       newinnerlst := inr :: !newinnerlst;
			       end
		       | PORT _ as pat -> newiolst := pat :: !newiolst; newinnerlst := inr :: !newinnerlst
		       | RNG [CNST (lft, _, []); CNST (rght, _, [])] -> fprintf stdout "%s" ("/* ["^lft^":"^rght^"] */")
		       | oth -> portothlst := oth :: !portothlst; failwith "portothlst"
		       ) previolst iolst;
           let newinnerlst = List.rev !newinnerlst in
	   let kind_opt = kind^"_opt" in
           if not (Hashtbl.mem modules_opt kind_opt) then
               begin
               printf "%d:%d\n" (List.length newinnerlst) (List.length previolst);
               let newinneritms = copy_itms itms in
               newinneritms.io := newinnerlst;
               let newhash = (src, lin, newinneritms) in
	       iterate kind newhash
               end;
           newitms.inst := (inst, (kind_opt, !newiolst)) :: !(newitms.inst);
           end
        ) !(modul.inst);
    Hashtbl.replace modules_opt (f^"_opt") (source, line, newitms);
    print_endline (f^" done")

let translate errlst xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    let (line,range) = match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos) | None -> (0, (0,0)) in
    let rwxml = rw' errlst xml in
    categorise (empty_itms false) rwxml;
    let top = snd(List.hd !top) in
    print_endline ("toplevel is "^top);
    let (topsrc, topline, topmodul) as tophash = Hashtbl.find modules top in
    iterate top tophash;
    let top_opt = top^"_opt" in
    dumpform top_opt topsrc;
    let fd = open_out (outnam top_opt) in
    fprintf fd "`default_nettype none\n";
    Hashtbl.iter (dump fd true) modules_opt;
    close_out fd;
    (line,range,rwxml,xml)
    

