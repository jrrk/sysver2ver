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
| Ulognot
| Unegate
| Uextend
| Uextends

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

type typmap =
| TYPNONE
| SUBTYP of int
| TYPRNG of int*int
| TYPMEMBER of int*string*int
| TYPENUM of string * int * (int*int)

type typetable_t = string*string*typmap*typmap list

type cexp =
| ERR of string
| BIN of char
| HEX of int
| SHEX of int
| STRING of string
| FLT of float

type rw =
| UNKNOWN
| XML of rw list
| EITM of string * string * string * int * rw list
| IO of string * string * int * dirop * string * rw list
| VAR of string * string * int * string
| IVAR of string * string * int * rw list * int
| TMPVAR of string * string * int * rw list
| CNST of (int * cexp) * int * rw list
| VRF of string * rw list
| TYP of string * string * int * rw list
| FNC of string * string * int * rw list
| TASK of string * string * string * rw list
| INST of string * string * (string * rw list)
| SFMT of string * rw list
| SYS of string * string * rw list
| TPLSRGS of string * string * int * rw list
| VPLSRGS of string * int * rw list
| PORT of string * string * dirop * int * rw list
| CA of string * rw list
| UNRY of unaryop * rw list
| SEL of string * rw list
| ASEL of rw list
| SNITM of string * rw list
| ASGNDLY of string * rw list
| ASGN of string * rw list
| ARITH of arithop * rw list
| LOGIC of logop * rw list
| CMP of cmpop * rw list
| FRF of string * string * rw list
| XRF of string * string * string * string * dirop ref
| PKG of string * string * rw list
| CAT of string * rw list
| CPS of string * rw list
| CND of string * rw list
| REPL of string * int * rw list
| MODUL of string * string * string * rw list
| BGN of string * rw list
| RNG of rw list
| ALWYS of string * rw list
| SNTRE of rw list
| IF of string * rw list
| INIT of string * string * rw list
| IRNG of string * rw list
| IFC of string * string * rw list
| IMP of string * string * rw list
| IMRF of string * string * string * rw list
| JMPL of string * rw list
| JMPG of string * rw list
| CS of string * rw list
| CSITM of string * rw list
| WHL of rw list
| FORSTMT of (string * cmpop * string * (int * cexp) * (int * cexp) * (int * cexp) * rw list)
| ARG of rw list
| DSPLY of string * string * rw list
| FILS of string * rw list
| FIL of string * string
| NTL of rw list
| CELLS of rw list
| CELL of string * string * string * string * rw list
| POSPOS of string*string
| POSNEG of string*string
| NEGNEG of string*string
| POSEDGE of string
| NEGEDGE of string
| COMB
| MODPORTFTR of string * string

type token =
| SP
| SEMI
| COLON
| COMMA
| AT
| DOT
| QUERY
| QUOTE
| DQUOTE
| PLUS
| MINUS
| STAR
| NL
| SRC of (string*int)
| IDENT of string
| NUM of cexp
| SIZED of (int * cexp)
| DIR of dirop
| BEGIN
| END
| DEFAULT
| LPAREN
| RPAREN
| LBRACK
| RBRACK
| LCURLY
| RCURLY
| LCOMMENT
| RCOMMENT
| LSHIFT
| RSHIFT
| IFF
| ELSE
| ASSIGN
| ASSIGNMENT
| ASSIGNDLY
| CMPOP of cmpop
| CASE
| ENDCASE
| WHILE
| FOR
| ALWAYS
| POSEDGE
| NEGEDGE
| RETURN
| LOGIC
| FUNCTION
| ENDFUNCTION
| TASK
| ENDTASK
| MODULE
| ENDMODULE
| INITIAL
| FINAL

type itms = { 
  io: (string*(int*dirop*string*(int*cexp) list)) list ref;
  v: (string*(int*string*int)) list ref;
  iv: (string*(int*rw list*int)) list ref;
  ir: (string*int) list ref;
  ca: (rw*rw) list ref;
  typ: (string*string*int) list ref;
  alwys: (string*rw*rw list) list ref;
  init: (token*rw list) list ref;
  func: (string*int*rw list*itms) list ref;
  task: (string*rw list*itms) list ref;
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
let tskothlst = ref []
let optothlst = ref []
let xrflst = ref []
let forlst = ref []
let ternlst = ref []
let ternothlst = ref []
let optitmlst = ref []
let widthlst = ref []

let matchcnt = ref 0

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
|"extends" -> Uextends
|"lognot" -> Ulognot
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

let dbg i ch h = if false then print_endline (string_of_int i^":"^String.make 1 ch^":"^string_of_int h)
       
let decode str =
  let h = ref 0 and str' = Bytes.make (String.length str / 2) ' ' in
  String.iteri (fun ix -> function
    | '0'..'9' as ch -> dbg ix ch !h;
        h := !h * 16 + (int_of_char ch - int_of_char '0');
        if ix land 1 = 1 then begin Bytes.set str' (ix/2) (char_of_int !h); h := 0; dbg ix ch !h;end
    | 'a'..'f' as ch -> dbg ix ch !h;
        h := !h * 16 + (int_of_char ch - int_of_char 'a' + 10);
        if ix land 1 = 1 then begin Bytes.set str' (ix/2) (char_of_int !h); h := 0; dbg ix ch !h; end
    | _ -> h := -1) str;
  STRING (Bytes.to_string str')

let cexp exp = try Scanf.sscanf exp "%d'h%x" (fun b n -> (b, HEX n)) with err ->
    try Scanf.sscanf exp "%d'sh%x" (fun b n -> (b, SHEX n)) with err ->
    try Scanf.sscanf exp "%d'bx" (fun b -> (b, BIN 'x')) with err ->
    try Scanf.sscanf exp "%d'h%s" (fun b n -> (b, decode n)) with err ->
    try Scanf.sscanf exp "%f" (fun f -> (64, FLT f)) with err -> (-1,ERR exp)

let rec typmap = function
| [] -> TYPNONE
| [("left", lft); ("right", rght)] -> TYPRNG(int_of_string lft, int_of_string rght)
| oth -> mapothlst := oth :: !mapothlst; failwith "mapothlst"

let rec subtypmap = function
| RNG [CNST ((b,HEX n), _, []); CNST ((b',SHEX n'), _, [])] -> TYPRNG(n,n')
| EITM ("enumitem", itm, "", n, [CNST ((w',SHEX n'), _, [])]) -> TYPENUM(itm, n, (w',n'))
| TYP ("memberdtype", nam, idx, []) -> TYPMEMBER(idx, nam, int_of_string nam)
| oth -> subothlst := oth :: !subothlst; failwith "subothlst"

let fortailmatch ix' = function
| ASGN (_, ARITH (Aadd, CNST (inc, _, []) :: VRF (ix'', []) :: []) :: VRF (ix''', []) :: []) :: tl -> (ix'=ix'') && (ix''=ix''')
| _ -> false

let forinc = function
| ASGN (_, ARITH (Aadd, CNST (inc, _, []) :: VRF (ix'', []) :: []) :: VRF (ix''', []) :: []) :: tl -> (inc,List.rev tl)
| tl -> ((0,ERR ""),List.rev tl)

let rec rw' errlst = function
| Xml.Element ("verilator_xml", [], xlst) -> XML (List.map (rw' errlst) xlst)
| Xml.Element ("files"|"module_files" as fils, [], xlst) -> FILS (fils, List.map (rw' errlst) xlst)
| Xml.Element ("file", [("id", encoding); ("filename", nam); ("language", "1800-2017")], []) -> FIL (encoding, nam)
| Xml.Element ("netlist", [], xlst) -> NTL (List.rev(List.map (rw' errlst) xlst))
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
               IO (origin, nam, int_of_string tid, dirop dir, typ, List.map (rw' errlst) xlst)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", ("ifaceref" as typ)); ("origName", nam')], []) ->
	       let (vif, sub) = chkvif nam in
	       if vif then
	       VAR (origin, sub, int_of_string tid, typ)
	       else
	       IO (origin, nam, int_of_string tid, Dunknown, typ, [])
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               VAR (origin, nam, int_of_string tid, typ)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
               [Xml.Element ("const", [("fl", _); ("name", _); ("dtype_id", cid)], []) as lev]) ->
                             IVAR (origin, nam, int_of_string tid, [rw' errlst lev], int_of_string cid)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
	       [Xml.Element ("initarray", [("fl", _); ("dtype_id", cid)], initlst)]) ->
                             IVAR (origin, nam, int_of_string tid, List.map (rw' errlst) initlst, int_of_string cid)
| Xml.Element ("const", [("fl", _); ("name", value); ("dtype_id", tid)], xlst) ->
               
               CNST (cexp value, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("contassign", [("fl", origin); ("dtype_id", tid)], xlst) -> CA (origin, List.map (rw' errlst) xlst)
| Xml.Element ("not"|"negate"|"extend"|"extends"|"lognot" as op, [("fl", origin); ("dtype_id", tid)], xlst) -> UNRY (unaryop op, List.map (rw' errlst) xlst)
| Xml.Element ("varref", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> VRF (snd (chkvif nam), List.map (rw' errlst) xlst)
| Xml.Element ("instance", [("fl", origin); ("name", nam); ("defName", dnam); ("origName", nam')], xlst) ->
               INST (origin, nam, (dnam, List.map (rw' errlst) xlst))
| Xml.Element ("range", [("fl", _)], xlst) -> RNG (List.map (rw' errlst) xlst)
| Xml.Element ("port", [("fl", origin); ("name", nam); ("direction", dir); ("portIndex", idx)], xlst) ->
               PORT (origin, nam, dirop dir, int_of_string idx, List.map (rw' errlst) xlst)
| Xml.Element ("port", [("fl", origin); ("name", nam); ("portIndex", idx)], xlst) -> let (vif,sub) = chkvif nam in
               PORT (origin, sub, Dvif, int_of_string idx, List.map (rw' errlst) xlst)
| Xml.Element ("sel", [("fl", origin); ("dtype_id", tid)], xlst) -> SEL (origin, List.map (rw' errlst) xlst)
| Xml.Element ("arraysel", [("fl", origin); ("dtype_id", tid)], xlst) -> ASEL (List.map (rw' errlst) xlst)
| Xml.Element ("always", [("fl", src)], xlst) -> ALWYS (src, List.map (rw' errlst) xlst)
| Xml.Element ("sentree", [("fl", origin)], xlst) -> SNTRE (List.map (rw' errlst) xlst)
| Xml.Element ("senitem", [("fl", origin); ("edgeType", etyp)], xlst) -> SNITM (etyp, List.map (rw' errlst) xlst)
| Xml.Element ("begin", [("fl", _); ("name", namedblk)], xlst) -> BGN (namedblk, List.map (rw' errlst) xlst)
| Xml.Element ("begin", [("fl", origin)], xlst) -> let xlst' = List.map (rw' errlst) xlst in
(match xlst' with
       | ASGN (_, CNST (strt, _, []) :: VRF (ix, []) :: []) ::
            WHL
             (CMP (cmpop, CNST (stop, _, []) :: VRF (ix', []) :: []) ::
              stmtlst) :: [] when (ix=ix') && fortailmatch ix (List.rev stmtlst) ->
                let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin, cmpop,ix,strt,stop,inc,stmts)
       | ASGN (_, a :: WHL (b :: stmtlst) :: []) :: [] -> forlst := (a,b,stmtlst) :: !forlst; BGN ("", xlst')
       | _ -> BGN ("", xlst'))
| Xml.Element ("assigndly", [("fl", origin); ("dtype_id", tid)], xlst) -> let xlst' = List.map (rw' errlst) xlst in
(match xlst' with
       | CND (_, cnd :: lft :: rght :: []) :: dst :: [] ->
    ternlst := (cnd,lft,rght,dst) :: !ternlst;
    IF(origin, cnd :: ASGNDLY (origin, lft :: dst :: []) :: ASGNDLY (origin, rght :: dst :: []) :: [])
       | (SEL (_, (UNRY (Uextends, expr1 :: []) as ext) ::
    (CNST ((_,HEX lo'), _, []) as lo) :: (CNST ((_,HEX wid'), _, []) as wid) :: []) ::
    (SEL (_, VRF _ :: CNST _ :: CNST _ :: []) as dst) :: []) ->
        begin
        let idx' = lo'+wid' in
        let t = "__tmp"^string_of_int idx' in
        let tmp = VRF (t, []) and idx = -idx' in
        Hashtbl.replace typetable idx ("basicdtype", "logic", TYPRNG(idx'-1,0), []); 
        TMPVAR(origin, t, idx, ASGN (origin, ext :: tmp :: []) :: ASGNDLY (origin, SEL (origin, tmp :: lo :: wid :: []) :: dst :: []) :: [])
        end    
       | _ -> ternothlst := xlst' :: !ternothlst; ASGNDLY (origin, xlst'))
| Xml.Element ("if", [("fl", origin)], xlst) -> IF (origin, List.map (rw' errlst) xlst)
| Xml.Element ("add"|"sub"|"mul"|"muls" as op, [("fl", _); ("dtype_id", tid)], xlst) -> ARITH (arithop op, List.map (rw' errlst) xlst)
| Xml.Element ("and"|"redand"|"or"|"redor"|"xor"|"redxor"|"xnor"|"redxnor"|"shiftl"|"shiftr"|"shiftrs" as log,
               [("fl", _); ("dtype_id", tid)], xlst) -> LOGIC (logop log, List.map (rw' errlst) xlst)
| Xml.Element ("eq"|"neq"|"gt"|"gts"|"gte"|"gtes"|"eqwild"|"neqwild"|"ltes"|"lte"|"lt"|"lts" as cmp, [("fl", _); ("dtype_id", tid)], xlst) -> CMP (cmpop cmp, List.map (rw' errlst) xlst)
| Xml.Element ("initial"|"final" as action, [("fl", origin)], xlst) -> INIT (origin, action, List.map (rw' errlst) xlst)
| Xml.Element ("assign", [("fl", origin); ("dtype_id", tid)], xlst) -> ASGN (origin, List.map (rw' errlst) xlst)
| Xml.Element ("package", [("fl", orig); ("name", nam); ("origName", nam')], xlst) -> PKG (orig, nam, List.map (rw' errlst) xlst)
| Xml.Element ("typedef" as dtyp, [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> TYP (dtyp, nam, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("func", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) -> FNC (origin, nam, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("jumplabel", [("fl", origin)], xlst) -> JMPL (origin, List.map (rw' errlst) xlst)
| Xml.Element ("jumpgo", [("fl", origin)], xlst) -> JMPG (origin, List.map (rw' errlst) xlst)
| Xml.Element ("concat", [("fl", origin); ("dtype_id", tid)], xlst) -> CAT (origin, List.map (rw' errlst) xlst)
| Xml.Element ("cvtpackstring", [("fl", origin); ("dtype_id", tid)], xlst) -> CPS (origin, List.map (rw' errlst) xlst)
| Xml.Element ("cond", [("fl", origin); ("dtype_id", tid)], xlst) -> CND (origin, List.map (rw' errlst) xlst)
| Xml.Element ("sformatf", [("fl", _); ("name", fmt); ("dtype_id", tid)], xlst) -> SFMT (fmt, List.map (rw' errlst) xlst)
| Xml.Element ("module", ("fl", origin) :: ("name", nam) :: ("origName", nam') :: attr, xlst) -> MODUL (origin, nam, nam', List.map (rw' errlst) xlst)
| Xml.Element ("case", [("fl", origin)], xlst) -> CS (origin, List.map (rw' errlst) xlst)
| Xml.Element ("caseitem", [("fl", origin)], xlst) -> CSITM (origin, List.map (rw' errlst) xlst)
| Xml.Element ("while", [("fl", _)], xlst) -> WHL (List.map (rw' errlst) xlst)
| Xml.Element ("insiderange", [("fl", origin); ("dtype_id", tid)], xlst) -> IRNG (origin, List.map (rw' errlst) xlst)
| Xml.Element ("funcref", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) -> FRF (origin, nam, List.map (rw' errlst) xlst)
| Xml.Element ("varxref", [("fl", origin); ("name", nam); ("dtype_id", tid); ("dotted", dotted)], []) ->
    XRF (origin, nam, tid, dotted, ref Dunknown)
| Xml.Element ("arg", [("fl", _)], xlst) -> ARG (List.map (rw' errlst) xlst)
| Xml.Element ("initarray"|"streaml"|"powsu"|"realtobits"|"itord" as op, [("fl", origin); ("dtype_id", tid)], xlst) -> SYS (origin, "$"^op, List.map (rw' errlst) xlst)
| Xml.Element ("replicate", [("fl", origin); ("dtype_id", tid)], xlst) -> REPL (origin, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("iface", [("fl", src); ("name", bus); ("origName", bus')], xlst) -> IFC (src, bus, List.map (rw' errlst) xlst)
| Xml.Element ("ifacerefdtype" as ifr, [("fl", _); ("id", num); ("modportname", nam)], xlst) ->
    let xlst' = List.map (rw' errlst) xlst and idx = int_of_string num in
    Hashtbl.add typetable idx (ifr, nam, TYPNONE, List.map subtypmap xlst'); TYP (ifr, nam, idx, xlst')
| Xml.Element ("modport", [("fl", origin); ("name", port)], xlst) -> IMP (origin, port, List.map (rw' errlst) xlst)
| Xml.Element ("modportvarref", [("fl", origin); ("name", member); ("direction", dir)], xlst) -> IMRF (origin, member, dir, List.map (rw' errlst) xlst)
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype" as dtyp, ("fl", _) :: ("id", num) :: rnglst, xlst) ->
    let xlst' = List.map (rw' errlst) xlst and idx = int_of_string num in
    (match rnglst with
      | ("name", nam) :: tl ->
          Hashtbl.add typetable idx (dtyp,nam,typmap tl,List.map subtypmap xlst')
      | _ ->
      Hashtbl.add typetable idx (dtyp,"",typmap rnglst,List.map subtypmap xlst'));
    TYP(dtyp, "", idx, xlst')
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp, [("fl", origin); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' errlst) xlst and idx = int_of_string num in
    Hashtbl.add typetable idx (dtyp,nam,SUBTYP (int_of_string subtype),List.map subtypmap xlst');
    TYP(dtyp, subtype, idx, xlst')
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp, [("fl", origin); ("id", num); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' errlst) xlst and idx = int_of_string num in

    Hashtbl.add typetable idx (dtyp,"",SUBTYP (int_of_string subtype),List.map subtypmap xlst');
    TYP(dtyp, subtype, idx, xlst')
| Xml.Element ("enumitem" as dtyp, [("fl", origin); ("name", nam); ("dtype_id", num)], xlst) -> EITM (dtyp, nam, "", int_of_string num, List.map (rw' errlst) xlst)
| Xml.Element ("cells", [], xlst) -> CELLS(List.map (rw' errlst) xlst)
| Xml.Element ("cell", [("fl", origin); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    CELL(origin, nam, subnam, hier, List.map (rw' errlst) xlst)
| Xml.Element ("display", [("fl", origin); ("displaytype", nam)], xlst) -> DSPLY (origin, nam, List.map (rw' errlst) xlst)
| Xml.Element ("readmem", [("fl", origin)], xlst) -> SYS (origin, "$readmemh", List.map (rw' errlst) xlst)
| Xml.Element (("fopen"|"fclose"|"typetable"|"finish"|"stop" as sys), [("fl", origin)], xlst) -> SYS (origin, "$"^sys, List.map (rw' errlst) xlst)
| Xml.Element (("task"|"taskref") as tsk, [("fl", origin); ("name", nam)], xlst) -> TASK(origin, tsk, nam, List.map (rw' errlst) xlst)
| Xml.Element ("valueplusargs", [("fl", origin); ("dtype_id", tid)], xlst) -> VPLSRGS(origin, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("testplusargs", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) ->
    TPLSRGS(origin, nam, int_of_string tid, List.map (rw' errlst) xlst)
| Xml.Element ("modportftaskref", [("fl", origin); ("name", nam)], []) -> MODPORTFTR (origin, nam)
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> errlst := err :: !errlst; failwith str

let top = ref []
let empty_itms () = {
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
let copy_itms prev = {
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

let unaryopv = function
| Unknown -> "???"
| Unot -> " ~ "
| Ulognot -> " ! "
| Unegate -> " - "
| Uextend -> "$unsigned"
| Uextends -> "$signed"

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
| Lshiftrs -> " >>> "

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

let rec cadd = function
| [] -> HEX 0
| ERR err :: tl -> ERR err
| HEX n :: [] -> HEX n
| SHEX n :: [] -> SHEX n
| FLT f :: [] -> FLT f
| FLT f :: _ -> ERR "addflt"
| STRING _ :: tl -> ERR "addstr"
| BIN 'x' :: tl -> cadd (BIN 'x' :: tl)
| BIN _ :: tl -> cadd (BIN 'x' :: tl)
| HEX n :: HEX m :: tl -> cadd (HEX (n+m) :: tl)
| SHEX n :: HEX m :: tl -> cadd (HEX (n+m) :: tl)
| HEX n :: SHEX m :: tl -> cadd (HEX (n+m) :: tl)
| SHEX n :: SHEX m :: tl -> cadd (SHEX (n+m) :: tl)
| (HEX _ | SHEX _) ::(ERR _|BIN _|STRING _|FLT _):: tl -> ERR "cadd"

let avoid_dollar_unsigned = true

let rec expr = function
| VRF (id, []) -> IDENT id :: []
| CNST ((s,n), tid, []) -> SIZED (s,n) :: []
| UNRY (Uextend, expr1 :: []) when avoid_dollar_unsigned -> LCURLY :: SIZED (1, BIN '0') :: COMMA :: expr expr1 @ [RCURLY]
| UNRY ((Uextend|Uextends) as op, expr1 :: []) -> IDENT (unaryopv op) :: LPAREN :: expr expr1 @ [RPAREN]
| UNRY (op, expr1 :: []) -> LPAREN :: IDENT (unaryopv op) :: expr expr1 @ [RPAREN]
| CMP ((Clts|Cltes|Cgtes|Cgts) as op, expr1 :: expr2 :: []) ->
    LPAREN :: expr (UNRY (Uextends, expr1 :: [])) @ CMPOP op :: expr (UNRY (Uextends, expr2 :: [])) @ [RPAREN]
| CMP (op, expr1 :: expr2 :: []) -> LPAREN :: expr expr1 @ CMPOP op :: expr expr2 @ [RPAREN]
| LOGIC (op, expr1 :: []) -> LPAREN :: IDENT (logopv op) :: expr expr1 @ [RPAREN]
| LOGIC (Lshiftrs as op, expr1 :: expr2 :: []) ->
    LPAREN :: expr (UNRY (Uextends, expr1 :: [])) @ (IDENT (logopv op) :: expr expr2) @[RPAREN]
| LOGIC (op, expr1 :: expr2 :: []) -> LPAREN :: expr expr1 @ (IDENT (logopv op) :: expr expr2) @[RPAREN]
| ARITH (op, expr1 :: expr2 :: []) -> LPAREN :: expr expr1 @ (IDENT (arithopv op) :: expr expr2) @[RPAREN]
| SEL (o, ((VRF _ | XRF _) as expr1) :: (CNST((szlo,lo'),_,_)) :: (CNST((szw,wid'),_,_)) :: []) ->
    expr expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: NUM lo' :: RBRACK :: []
    | _ -> LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: [])
| SEL (o, VRF(expr1,_) :: expr2 :: CNST((szw,wid'),_,_) :: []) ->
    IDENT expr1 :: (match wid' with HEX 1 | SHEX 1 -> LBRACK :: expr expr2 @ [RBRACK]
    | _ -> LBRACK :: expr expr2 @ [PLUS;COLON] @ (NUM wid' :: RBRACK :: []))
| SEL (o, expr1 :: CNST ((32,SHEX 0), _, []) :: (CNST _) :: []) -> expr expr1
| SEL (o, expr1 :: CNST ((szlo,lo'), _, []) :: (CNST _) :: []) ->
    LPAREN :: expr expr1 @ (RSHIFT :: NUM lo' :: RPAREN :: [])
| ASEL (VRF (lval, []) :: expr1 :: []) -> IDENT lval :: LBRACK :: expr expr1 @ [RBRACK]
| CND (o, expr1 :: lft :: rght :: []) -> LPAREN :: expr expr1 @ [QUERY] @ expr lft @ [COLON] @ expr rght @ [RPAREN]
| CAT (o, expr1 :: expr2 :: []) -> LCURLY :: expr expr1 @ [COMMA] @ expr expr2 @ [RCURLY]
| FRF (o, fref, arglst) -> let delim = ref LPAREN in
    IDENT fref :: List.flatten (List.map (function ARG (arg :: []) -> let lst = !delim :: expr arg in delim := COMMA; lst| _ -> [QUERY]) arglst) @ [RPAREN];
| REPL (o, tid, arg :: CNST ((sz,n'),_,_) :: []) -> LCURLY :: NUM n' :: LCURLY :: expr arg @ [RCURLY;RCURLY]
| IRNG (o, expr2 :: expr1 :: []) -> LBRACK :: expr expr1 @ [COLON] @ expr expr2 @ [RBRACK]
| XRF (o, id, tid, dotted, dirop) as xrf -> xrflst := xrf :: !xrflst; IDENT (dotted^(match !dirop with Dinam _ -> "_" | _ -> ".")^id) :: []
| TPLSRGS (o, id, tid, []) -> IDENT "$test$plusargs" :: LPAREN :: DQUOTE :: IDENT id :: DQUOTE :: RPAREN :: []
| VPLSRGS (o, tid, CNST ((len, fmt), _, []) :: VRF (arg, []) :: []) -> IDENT "$value$plusargs" :: LPAREN :: NUM fmt :: COMMA :: IDENT arg :: RPAREN :: []
| SYS (o, fn, arglst) -> IDENT fn :: LPAREN :: eiter SP arglst @ [RPAREN]
| oth -> exprothlst := oth :: !exprothlst; failwith "exprothlst"
and eiter tok lst =
    let delim = ref tok in
    List.flatten (List.map (fun itm -> let lst' = !delim :: expr itm in delim := COMMA; lst') lst)

let rec portconn = function
| VRF (id, []) -> DOT :: IDENT id :: []
| PORT (o, id_o, dir, idx, []) -> DOT :: IDENT id_o :: LPAREN :: RPAREN :: []
| PORT (o, id_i, dir, idx, expr1 :: []) -> DOT :: IDENT id_i :: LPAREN :: expr expr1 @ [RPAREN]
| RNG [CNST ((_,lft), _, []); CNST ((_,rght), _, [])] -> LCOMMENT :: NUM lft :: COLON :: NUM rght :: RCOMMENT :: []
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst, _, []) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

let stmtdly = function
| false -> SP :: ASSIGNMENT :: SP :: []
| true -> SP :: ASSIGNDLY :: SP :: []

let tokenout fd indent = function
| SP -> output_string fd " "
| SEMI -> output_string fd ";"
| COLON -> output_string fd ":"
| COMMA -> output_string fd ","
| LPAREN -> output_string fd "("
| RPAREN -> output_string fd ")"
| LBRACK -> output_string fd "["
| RBRACK -> output_string fd "]"
| LCURLY -> output_string fd "{"
| RCURLY -> output_string fd "}"
| LCOMMENT -> output_string fd " /* "
| RCOMMENT -> output_string fd " */ "
| LSHIFT -> output_string fd "<<"
| RSHIFT -> output_string fd ">>"
| AT -> output_string fd "@"
| DOT -> output_string fd "."
| PLUS -> output_string fd "+"
| MINUS -> output_string fd "-"
| STAR -> output_string fd "*"
| QUERY -> output_string fd "?"
| QUOTE -> output_string fd "'"
| DQUOTE -> output_string fd "\""
| NL -> output_string fd ("\n"^if !indent > 0 then String.make (!indent*4) ' ' else "")
| SRC (str1,int1) -> output_string fd ("\n/* "^str1^":"^string_of_int int1^" */\n")
| DEFAULT -> output_string fd "default"
| IDENT str -> output_string fd str
| NUM (BIN n) -> output_string fd (String.make 1 n)
| NUM (HEX n) -> output_string fd (string_of_int n)
| NUM (SHEX n) -> output_string fd (string_of_int n)
| NUM (STRING s) -> output_string fd ("\""^String.escaped s^"\"")
| NUM (FLT f) -> output_string fd (string_of_float f)
| NUM (ERR err) -> output_string fd ("NumberError:"^err)
| SIZED (w,n) -> output_string fd ((function
        | BIN b -> string_of_int w^"'b"^String.make w b
	| HEX n -> Printf.sprintf "%d'h%x" w n
	| SHEX n -> Printf.sprintf "%d'sh%x" w n
	| STRING s -> "\""^String.escaped s^"\""
	| FLT f -> string_of_float f
        | ERR err -> ("NumberError:"^err)) n)
| DIR str -> output_string fd (diropv str)
| BEGIN -> incr indent; output_string fd "    begin"
| END -> output_string fd "end"; decr indent
| IFF -> output_string fd "if"
| ELSE -> output_string fd "else"
| ASSIGN -> output_string fd "assign"
| ASSIGNMENT -> output_string fd "="
| ASSIGNDLY -> output_string fd "<="
| CASE -> output_string fd "case"; incr indent
| ENDCASE -> output_string fd "endcase"; decr indent
| CMPOP op -> output_string fd (cmpopv op)
| WHILE -> output_string fd "while"
| FOR -> output_string fd "for"
| ALWAYS -> output_string fd "always"
| POSEDGE -> output_string fd "posedge"
| NEGEDGE -> output_string fd "negedge"
| RETURN -> output_string fd "return"
| LOGIC -> output_string fd "logic"
| FUNCTION -> output_string fd "function"; incr indent
| ENDFUNCTION -> output_string fd "endfunction"; decr indent
| TASK -> output_string fd "task"; incr indent
| ENDTASK -> output_string fd "endtask"; decr indent
| MODULE -> output_string fd "module"; incr indent
| ENDMODULE -> output_string fd "endmodule"; decr indent
| INITIAL -> output_string fd "initial"
| FINAL -> output_string fd "final"

let rec reformat0 = function
| [] -> []
| END :: NL :: SEMI :: NL :: ENDCASE :: tl -> END :: reformat0 (NL :: ENDCASE :: tl)
| ENDCASE :: SEMI :: ELSE :: tl -> ENDCASE :: reformat0 ( NL :: ELSE :: tl )
| END :: NL :: SEMI :: ELSE :: tl -> END :: reformat0 ( NL :: ELSE :: tl )
| SEMI :: NL :: SEMI :: NL :: END :: tl -> SEMI :: NL :: reformat0 ( END :: tl )
| oth :: tl -> oth :: reformat0 tl

let rec reformat1 = function
| [] -> []
| prior :: (BEGIN|END|ELSE as tok) :: tl when prior <> NL -> prior :: NL :: tok :: reformat1 (NL :: tl)
| (BEGIN|END|ELSE as tok) :: post :: tl when post <> NL && post <> COLON -> NL :: tok :: NL :: post :: reformat1 tl
| DIR dir :: tl -> NL :: DIR dir :: reformat1 tl
| DOT :: tl -> NL :: DOT :: reformat1 tl
| SEMI :: IFF :: tl -> SEMI :: NL :: IFF :: reformat1 tl
| NL :: NL :: tl -> reformat1 (NL :: tl)
| oth :: tl -> oth :: reformat1 tl

let rec reformat2 = function
| [] -> []
| BEGIN :: NL :: NL :: tl -> reformat2 (BEGIN :: NL :: tl)
| ENDCASE :: SEMI :: NL :: END :: tl -> ENDCASE :: reformat2 ( NL :: END :: tl )
| END :: NL :: SEMI :: NL :: END :: tl -> reformat2 (END :: NL :: END :: tl)
| END :: NL :: SEMI :: NL :: ALWAYS :: tl -> END :: NL :: NL :: reformat2 (ALWAYS :: tl)
| END :: NL :: SEMI :: NL :: tl -> END :: reformat2 (SEMI :: NL :: tl)
| ELSE :: NL :: IFF :: tl -> ELSE :: SP :: IFF :: reformat2 tl
| END :: NL :: NL :: ELSE :: tl -> END :: reformat2 (NL :: ELSE :: tl)

| oth :: tl -> oth :: reformat2 tl

let reviter lst =
    let delim = ref COLON in
    List.rev (List.flatten (List.map (fun itm -> let lst' = !delim :: expr itm in delim := COMMA; lst') lst))

let fold1 fn = function
| [] -> 0 (* should never occur, just to keep type system happy *)
| (hd::tl) -> List.fold_left fn hd tl

let num x = NUM (HEX x)

let rec iter2 dly lst =
    List.flatten (List.map (fun itm -> cstmt dly itm @ [SEMI;NL]) lst)

and csitm dly = function
    | CSITM (o, cexp :: (BGN _ as st) :: []) -> expr cexp @ COLON :: cstmt dly st @ NL :: []
    | CSITM (o, cexp :: st :: []) -> expr cexp @ COLON :: BEGIN :: cstmt dly st @ SEMI :: END :: NL :: []
    | CSITM (o, st :: []) -> DEFAULT :: COLON :: cstmt dly st @ SEMI :: []
    | CSITM (o, cexplst) -> (match List.rev cexplst with
			| ((BGN _ as hd)::tl) -> reviter tl @ BEGIN :: cstmt dly hd @ [SEMI;END;NL]
			| (hd::tl) -> reviter tl @ BEGIN :: cstmt dly hd @ [SEMI;END;NL]
                        | [] -> [])
    | oth -> csothlst := oth :: !csothlst; failwith "csothlst"

and ifcond = function
| LPAREN :: tl -> IFF :: SP :: LPAREN :: tl
| oth -> IFF :: SP :: LPAREN :: oth @ (RPAREN :: [])

and ewidth = function
| CAT (_,lst) -> fold1 (+) (List.map ewidth lst)
| SEL (_, VRF (_, []) :: CNST ((_,_), _, []) :: CNST ((_,HEX wid), _, []) :: []) -> wid
| SEL (_, UNRY (Uextends, _) :: CNST ((_,_), _, []) :: CNST ((_,HEX wid), _, []) :: []) -> wid
| CNST ((n, _), _, []) -> n
| oth -> widthlst := oth :: !widthlst; failwith "widthlst"

and cstmt dly = function
| JMPG (_,[]) -> []
| BGN(str1, rw_lst) -> BEGIN :: iter2 dly rw_lst @ [END;NL]
| IF(o, cnd :: then_stmt :: []) -> ifcond (expr cnd) @ (SP :: cstmt dly then_stmt)
| IF(o, cnd :: (BGN _ as then_stmt) :: else_stmt :: []) ->
    ifcond (expr cnd) @ (SP :: cstmt dly then_stmt) @ [ELSE] @ cstmt dly else_stmt
| IF(o, cnd :: then_stmt :: else_stmt :: []) ->
    ifcond (expr cnd) @ (SP :: cstmt dly then_stmt) @ [SEMI;ELSE] @ cstmt dly else_stmt
| ASGNDLY (o, 
    src ::
    SEL (_,
        ASEL (VRF (lval, []) ::
        expr1 ::
        []) ::
    CNST ((szlo,lo'), _, []) ::
    CNST ((szw,wid'), _, []) ::
    []) ::
 []) -> 
    IDENT lval :: LBRACK :: expr expr1 @ RBRACK :: LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: stmtdly true @ expr src
| ASGNDLY(o, src :: dst :: []) -> expr dst @ stmtdly true @ expr src
| ASGN (o, src :: dst :: []) -> expr dst @ stmtdly false @ expr src
| CS (o, sel :: lst) -> CASE :: LPAREN :: expr sel @ (RPAREN :: NL :: List.flatten (List.map (csitm dly) lst)) @ [NL;ENDCASE]
| CA(o, rght::lft::[]) -> ASSIGN :: SP :: expr lft @ stmtdly dly @ expr rght
| VAR (o, id, _, kind) -> IDENT kind :: IDENT id :: []
| FORSTMT (o,cnd,ix,strt,stop,inc,stmts) ->
    FOR :: LPAREN :: IDENT ix :: ASSIGNMENT :: SIZED strt :: SEMI ::
    SIZED stop :: CMPOP cnd :: IDENT ix :: SEMI ::
    IDENT ix :: ASSIGNMENT :: IDENT ix :: PLUS :: SIZED inc :: RPAREN ::
    BEGIN :: iter2 dly stmts @ [END]
| DSPLY (o, typ, SFMT (fmt, arglst) :: []) -> IDENT typ :: LPAREN :: DQUOTE :: IDENT fmt :: DQUOTE :: eiter COMMA arglst @ [RPAREN]
| DSPLY (o, typ, SFMT (fmt, arglst) :: expr1 :: []) ->
    IDENT typ :: LPAREN :: expr expr1 @ (COMMA :: IDENT fmt :: COMMA :: reviter arglst) @ [RPAREN]
| SYS (o, fn, arglst) -> IDENT fn :: LPAREN :: eiter SP arglst @ [RPAREN]
| CNST((s,n), _, []) -> SIZED (s,n) :: []
| TASK (o, "taskref", nam, arglst) -> IDENT nam :: (if arglst <> [] then eiter LPAREN arglst @ [RPAREN] else [])
| JMPL(o, rw_lst) -> BEGIN :: iter2 dly rw_lst @ [NL;END;NL]
| TMPVAR (o, nam, wid, stmtlst) -> cstmt dly (BGN ("", stmtlst))
| oth -> stmtothlst := oth :: !stmtothlst; failwith "stmtothlst"

let flatten1 dly = function
| BGN _ :: tl as lst -> let delim = ref SP in List.flatten (List.map (fun itm -> let lst' = !delim :: cstmt dly itm in delim := SEMI; lst') lst)
| lst -> let delim = ref BEGIN in List.flatten (List.map (fun itm -> let lst' = !delim :: cstmt dly itm in delim := SEMI; lst') lst) @ [SEMI;END;NL;NL]

let find_source origin =
    let last = ref 0 in
    for i = String.length origin - 1 downto 0 do
        match origin.[i] with '0'..'9' -> last := i | _ -> ();
    done;
    let k = String.sub origin 0 !last in
    let source = if Hashtbl.mem files k then Hashtbl.find files k else "origin_unknown" in
    let line = String.sub origin !last (String.length origin - !last) in
    (source, int_of_string line)

let rec cell_hier = function
| CELL (_, nam, subnam, hier, rw_lst) ->
   let hier_lst = List.flatten (List.map cell_hier rw_lst) in
   print_endline ("Cell: "^subnam);
   Hashtbl.replace hierarchy subnam hier_lst;
   (nam,subnam) :: hier_lst
| oth -> cellothlst := oth :: !cellothlst; failwith "cellothlst"

let catch_escapes = ref false

let rec optim2 = function
| [] -> []
| ASGN (o1, expr :: tmp1 :: []) :: ASGNDLY (o2, SEL (_, tmp1' :: lst) :: dst :: []) ::
  ASGN (o3, expr' :: tmp2 :: []) :: ASGNDLY (o4, SEL (_, tmp2' :: lst') :: dst' :: []) :: tl when (tmp1=tmp1') && (tmp2=tmp2') && (expr=expr') ->
    (match optim2 (ASGN (o1, expr :: tmp1 :: []) :: ASGNDLY (o2, SEL (o2, tmp1 :: lst) :: dst :: []) :: tl) with
        | hd0::hd1::tl -> hd0::hd1::ASGNDLY (o4, SEL (o4, tmp1 :: lst') :: dst' :: []) :: tl
	| _ -> failwith "optim2")
| hd :: tl -> hd :: optim2 tl

let rec optitm3 = function
| [] -> []
| BGN ("", tl) :: BGN ("", tl') :: tl'' -> optitm3 (BGN ("", tl @ tl') :: tl'')
| BGN ("", tl) :: tl' -> BGN ("", optitm3 tl) :: optitm3 tl'
| TMPVAR (o1,a,b,lst1) :: TMPVAR (o2,a',b',lst2) :: tl when b <= b' -> optitm3 (TMPVAR(o1,a,b,lst1@lst2) :: tl)
| TMPVAR (o,a,b,lst) :: tl -> let optlst = optim2 lst in optitmlst := (lst,optlst) :: !optitmlst; optlst @ optitm3 tl
| CS(o,rw_lst) :: tl -> CS(o,optitm3 rw_lst) :: optitm3 tl
| CSITM(o,rw_lst) :: tl -> CSITM(o,optitm3 rw_lst) :: optitm3 tl
| WHL(rw_lst) :: tl -> WHL(optitm3 rw_lst) :: optitm3 tl
| FORSTMT(o,cmpop,ix,strt,stop,inc,rw_lst) :: tl -> FORSTMT(o,cmpop,ix,strt,stop,inc,optitm3 rw_lst) :: optitm3 tl
| TASK(o, tsk, nam, rw_lst) :: tl -> TASK(o, tsk, nam, optitm3 rw_lst) :: optitm3 tl
| ASGN _ as oth :: tl -> oth :: optitm3 tl
| ASGNDLY _ as oth :: tl -> oth :: optitm3 tl
| IF(o, cnd :: then_stmt :: []) :: tl -> IF (o, cnd :: BGN("", optitm3 [then_stmt]) :: []) :: optitm3 tl
| IF(o, cnd :: then_stmt :: else_stmt :: []) :: tl -> IF (o, cnd :: BGN("", optitm3 [then_stmt]) :: BGN("", optitm3 [else_stmt]) :: []) :: optitm3 tl
| (CNST _ | VRF _ | LOGIC _ | SEL _ | DSPLY _ | SYS _ | UNRY _) as oth :: tl -> oth :: optitm3 tl
| oth :: tl when !catch_escapes -> optothlst := oth :: !optothlst; failwith "optothlst3"
| hd :: tl -> hd :: optitm3 tl
                                                         
let rec optitm4 = function
| BGN ("", BGN ("", tl) :: []) -> optitm4 (BGN("", tl)) 
| BGN ("", tl) -> BGN ("", List.map optitm4 tl)
| IF(o, cnd :: then_else_stmt_lst) -> IF (o, cnd :: List.map optitm4 then_else_stmt_lst)
| CS (o, sel :: lst) -> CS (o, sel :: List.map optitm4 lst)
| CSITM(o, rw_lst) -> CSITM(o, List.map optitm4 rw_lst)
| WHL(rw_lst) -> WHL(List.map optitm4 rw_lst)
| FORSTMT(o,cmpop,ix,strt,stop,inc,rw_lst) -> FORSTMT(o,cmpop,ix,strt,stop,inc,List.map optitm4 rw_lst)
| TASK(o, tsk, nam, rw_lst) -> TASK(o, tsk, nam, List.map optitm4 rw_lst)
| (ASGN _  | ASGNDLY _ | CNST _ | VRF _ | LOGIC _ | SEL _ | DSPLY _ | SYS _) as oth -> oth
| oth -> optothlst := oth :: !optothlst; failwith "optothlst4"

let optitm lst = let lst' = optitm3 lst in let lst'' = List.map optitm4 lst' in lst''

let rec catitm pth itms = function
| IO(o, str1, int1, Dunknown, "ifaceref", []) ->
    let (dtype, dir, _, _) = Hashtbl.find typetable int1 in
    itms.io := (str1, (int1, Dinam dir, "logic", [])) :: !(itms.io)
| IO(o, str1, int1, dir, str3, clst) -> itms.io := (str1, (int1, dir, str3, List.map ioconn clst)) :: !(itms.io)
| VAR(o, str1, int1, "ifaceref") -> itms.ir := (str1, int1) :: !(itms.ir)
| VAR(o, str1, int1, str2) -> itms.v := (str1, (int1, str2, -1)) :: !(itms.v)
| IVAR(o, str1, int1, rwlst, int2) -> itms.iv := (str1, (int1, rwlst, int2)) :: !(itms.iv)
| TMPVAR(o, str1, int1, stmtlst) ->
    List.iter (catitm pth itms) stmtlst;
    if not (List.mem_assoc str1 !(itms.v)) then
        itms.v := (str1, (int1, str1, -1)) :: !(itms.v)
| CA(o, rght::lft::[]) -> itms.ca := (lft, rght) :: !(itms.ca)
| TYP(str1, str2, int1, []) -> itms.typ := (str1,str2,int1) :: !(itms.typ)
| INST(o, str1, (str2, port_lst)) -> let pth = if String.length pth > 0 then pth^"_"^str1 else str1 in
    itms.inst := (pth, (str2, List.rev port_lst)) :: !(itms.inst)
| ALWYS(src, SNTRE(SNITM ("POS", [VRF (ck, [])]) :: SNITM ("POS", [VRF (rst, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (src, POSPOS(ck,rst), optitm rw_lst) :: !(itms.alwys)    
| ALWYS(src, SNTRE(SNITM ("POS", [VRF (ck, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (src, POSEDGE(ck), optitm rw_lst) :: !(itms.alwys)
| ALWYS(src, SNTRE(SNITM ("NEG", [VRF (ck, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (src, NEGEDGE(ck), optitm rw_lst) :: !(itms.alwys)
| ALWYS(src, SNTRE(SNITM (("POS"|"NEG") as edg, [VRF (ck, [])]) :: SNITM ("NEG", [VRF (rst, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    let rw_lst' = (function
       | BGN(lbl, (IF(o, VRF(rst',[]) :: thn :: els :: []) :: [])) :: [] ->
           BGN("", (IF(o, UNRY(Unot, VRF(rst',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | IF(o, VRF(rst',[]) :: thn :: els :: []) :: [] ->
           BGN("", (IF(o, UNRY(Unot, VRF(rst',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | oth -> posneglst := oth :: !posneglst; oth) rw_lst in
    itms.alwys := (src, (match edg with "POS" -> POSNEG(ck,rst) | "NEG" -> NEGNEG(ck,rst) | _ -> UNKNOWN), optitm rw_lst') :: !(itms.alwys)
| ALWYS(src, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (src, COMB, rw_lst) :: !(itms.alwys)
| INIT (o, "initial", rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.init := (INITIAL, rw_lst) :: !(itms.init)
| INIT (o, "final", rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.init := (FINAL, rw_lst) :: !(itms.init)
| BGN(str1, rw_lst) -> let pth' = String.map (function ('A'..'Z' | 'a'..'z' | '0'..'9') as ch -> ch | _ -> '_') str1 in
    List.iter (catitm (pth^"_"^pth') itms) rw_lst
| FNC(o, str1, idx1, rw_lst) ->
    let itms' = empty_itms () in
    List.iter (catitm pth itms') rw_lst;
    itms.func := (str1, idx1, rw_lst, itms') :: !(itms.func)
| IF(o, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.gen := rw_lst :: !(itms.gen)
| IMP(o, str1, rw_lst) ->
    itms.imp := (List.map (function
    | IMRF(_, str1, str2, []) -> (str1,str2)
    | MODPORTFTR (_,str1) -> (str1,str1)
    | oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst") rw_lst) :: !(itms.imp)
| IMRF(o, str1, str2, []) -> ()
| TASK (o, "task", str1, rw_lst) ->
    let itms' = empty_itms () in
    List.iter (catitm pth itms') rw_lst;
    itms.task := (str1, rw_lst, itms') :: !(itms.task)
| NTL(rw_lst)
| RNG(rw_lst)
| SNTRE(rw_lst)
| IRNG(_,rw_lst)
| JMPL(_,rw_lst)
| JMPG(_,rw_lst)
| CS(_,rw_lst)
| CSITM(_,rw_lst)
| WHL(rw_lst)
| FORSTMT(_,_,_,_,_,_,rw_lst)
| ARG(rw_lst)
| FILS(_, rw_lst)
| XML(rw_lst)
| EITM(_, _, _, _, rw_lst)
| CNST(_, _, rw_lst)
| VRF(_, rw_lst)
| TASK(_, _, _, rw_lst)
| SFMT(_, rw_lst)
| SYS(_,_, rw_lst)
| PORT(_, _, _, _, rw_lst)
| UNRY(_, rw_lst)
| SEL(_, rw_lst)
| ASEL(rw_lst)
| SNITM(_, rw_lst)
| ASGN(_, rw_lst)
| ASGNDLY(_, rw_lst)
| ARITH(_, rw_lst)
| LOGIC(_, rw_lst)
| CMP(_, rw_lst)
| FRF(_, _, rw_lst)
| CAT(_, rw_lst)
| CPS(_, rw_lst)
| CND(_, rw_lst)
| DSPLY(_, _, rw_lst)
| VPLSRGS(_, _, rw_lst)
| REPL(_, _, rw_lst) -> List.iter (catitm pth itms) rw_lst
| XRF(o, str1, str2, str3, dirop) ->
    if List.mem_assoc str3 !(itms.io) then
        begin
        match List.assoc str3 !(itms.io) with
         | (int1, dir, logic, _) -> dirop := dir
        end
    else
    print_endline (str3^" is not io");
| MODUL(origin, str1, str2, rw_lst) ->
    let (source, line) = find_source origin in
    print_endline (str1^":"^source);
    let itms = empty_itms () in
    List.iter (catitm "" itms) rw_lst;
    if Hashtbl.mem hierarchy str1 then Hashtbl.add modules str1 (source, line, itms)
    else if Hashtbl.mem hierarchy str2 then Hashtbl.add modules str2 (source, line, itms)
    else print_endline ("Module "^str1^"/"^str2^" not in cell hierarchy");
| PKG(origin, str1, rw_lst) ->
    let (source, line) = find_source origin in
    let itms = empty_itms () in
    List.iter (catitm str1 itms) rw_lst;
    Hashtbl.add packages str1 (source, line, itms)
| IFC(origin, str1, rw_lst) ->
    let (source, line) = find_source origin in
    let itms = empty_itms () in
    List.iter (catitm str1 itms) rw_lst;
    Hashtbl.add interfaces str1 (source, line, itms, rw_lst)
| FIL(enc, fil) ->
    Hashtbl.add files enc fil
| CELLS(rw_lst) ->
    top := List.flatten(List.map cell_hier rw_lst)
| TPLSRGS (_, id, tid, []) -> ()
| TYP (dtyp, subtype, idx, lst) -> ()
| oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst"

let rec cntbasic = function
| ("structdtype",_,typmap,rw_lst) -> fold1 (+) (List.flatten (List.map cntmembers rw_lst)) :: []
| ("uniondtype",_,typmap,rw_lst) -> fold1 (max) (List.flatten (List.map cntmembers rw_lst)) :: []
| ("basicdtype", ("logic"|"integer"|"int"|"bit"), TYPRNG(hi, lo), []) -> hi - lo + 1 :: []
| ("basicdtype", ("logic"|"bit"), TYPNONE, []) -> 1 :: []
| ("basicdtype", ("real"), TYPNONE, []) -> 64 :: []
| ("ifacerefdtype", _, TYPNONE, []) -> 0 :: []
| ("packarraydtype", "", SUBTYP subtyp, [TYPRNG(n,n')]) -> List.map (fun itm -> itm * (n - n' + 1)) (findmembers subtyp)
| ("unpackarraydtype", "", SUBTYP subtyp, [TYPRNG (n,n')]) -> (n - n' + 1) :: findmembers subtyp
| ("memberdtype", id, SUBTYP subtyp, []) -> 1 :: []
| oth -> typothlst := oth :: !typothlst; failwith "typothlst"

and cntmembers = function
| TYPMEMBER (idx1, field1, idx2) -> findmembers idx2
| oth -> memothlst := oth :: !memothlst; failwith "memothlst"

and findmembers idx = if Hashtbl.mem typetable idx then
    cntbasic (Hashtbl.find typetable idx) else []

let iolst delim dir idx io =
    let wid = fold1 ( * ) (findmembers idx) in
    !delim @ (DIR dir :: SP :: LOGIC ::
	     (if wid > 1 then LBRACK :: num (wid-1) :: COLON :: num 0 :: RBRACK :: [] else []) @ [SP; IDENT io])

let varlst delim idx id =
    let widlst = findmembers idx in
    let expand delim = fun w -> let lst = !delim :: num w :: [] in delim := STAR; lst in
    !delim @ LOGIC :: SP :: match widlst with
                | [] -> IDENT id :: []
                | 1 :: [] when idx >= 0 -> IDENT id :: []
                | n :: [] -> LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: []
                | n :: m :: [] -> LBRACK :: num (m-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: SP :: LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: []
                | oth -> let delim = ref LBRACK in
    List.flatten (List.map (expand delim) widlst) @ MINUS :: num 1 :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: []
	     
let rec fnstmt dly nam delim = function
| IO (o, io, idx, dir, kind', lst) ->
    let lst = iolst delim dir idx io in delim := COMMA :: []; lst
| VAR (o, id, idx, kind') -> 
  if !delim <> SEMI :: [] then delim := RPAREN :: SEMI :: [];
    let lst = varlst delim idx id in
    delim := SEMI :: []; lst
| JMPL(o, (BGN _ :: tl as rw_lst)) -> let dlm = !delim in delim := []; dlm @ (List.flatten (List.map (fnstmt dly nam delim) rw_lst))
| JMPL(o, rw_lst) -> let dlm = !delim in delim := [BEGIN]; dlm @ (List.flatten (List.map (fnstmt dly nam delim) rw_lst)) @ [SEMI;END]
| JMPG (_,[]) -> []
| itm ->
  let lst = !delim @ cstmt dly itm in
  delim := SEMI :: NL :: [];
  lst

let rec taskstmt dly nam = function
| BGN(_,rw_lst) -> List.flatten (List.map (taskstmt dly nam) rw_lst)
| itm -> cstmt dly itm @ SEMI :: NL :: []

let outnam f = f^"_translate.v"
let outnamopt f = let l = String.length f in f^(if l < 4 || String.sub f (l-4) 4 <> "_opt" then "_opt" else "")^"_translate.v"
let outtok f = f^"_tokens.txt"
let outtcl f = "./"^f^"_fm.tcl"

let dump f (source, line, modul) =
  let appendlst = ref [] in
  let append lst = appendlst := lst :: !appendlst in
  if true then print_endline ("f \""^f^"\";; /* "^outnam f^" versus "^source^":"^string_of_int line^" "^outtcl f ^" */");
  let delim = ref [NL; MODULE; SP; IDENT f; LPAREN] in List.iter (fun (io, (idx, dir, kind', lst)) -> 
    let lst = iolst delim dir idx io in
    delim := [COMMA];
    append lst
    ) (List.rev !(modul.io));
  append [RPAREN;SEMI;NL];
  List.iter (fun (id, (idx, kind', n)) -> append (varlst (ref []) idx id @ SEMI :: NL :: []);
                 ) (List.rev !(modul.v));
  List.iter (fun (nam, idx, lst, itms') ->
		 let lst = (varlst (ref (SEMI :: NL :: FUNCTION :: SP :: [])) idx nam) @
		 List.flatten (List.map (fnstmt false nam (ref [LPAREN])) (List.tl lst)) @ [ENDFUNCTION] in
		 append lst;
                 ) (List.rev !(modul.func));
  List.iter (fun (nam, lst, itms') ->
		 let lst = List.flatten (List.map (taskstmt false nam) lst) in
		 append (TASK :: SP :: IDENT nam :: SEMI :: NL :: BEGIN :: lst @ END :: ENDTASK :: NL :: []);
                 ) (List.rev !(modul.task));
  List.iter (fun (dst, src) ->
                 append (ASSIGN :: SP :: expr dst @ (SP :: ASSIGNMENT :: SP:: expr src @ SEMI :: NL :: []));
                 ) (List.rev !(modul.ca));
  List.iter (function
    | (src, COMB, lst) ->
      append (SRC src :: ALWAYS :: AT :: STAR :: flatten1 false lst);
    | (src, POSNEG (ck, rst), lst) ->
      append (SRC src :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 true lst);
    | (src, NEGNEG (ck, rst), lst) ->
      append (SRC src :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 true lst);
    | (src, POSEDGE (ck), lst) ->
      append (SRC src :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 true lst);
    | (src, NEGEDGE (ck), lst) ->
      append (SRC src :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 true lst);
    | (src, _, lst) -> failwith "not implemented";
    ) (List.rev (List.map (fun (src,edg,lst) -> (find_source src, edg, lst)) !(modul.alwys)));
  List.iter (fun (inst, (kind, lst)) ->
                 let delim = ref SP in
                 let lst = List.flatten (List.map (fun term -> let lst = !delim :: portconn term in delim := COMMA; lst) lst) in
                 append (NL :: NL :: IDENT kind :: SP :: IDENT inst :: LPAREN :: lst @ [NL;RPAREN;SEMI]);
                 ) !(modul.inst);
  List.iter (fun (tok, lst) -> append (NL :: tok :: flatten1 false lst);
                 ) !(modul.init);
  append [NL;ENDMODULE;NL;NL];
  List.flatten (List.rev !appendlst)

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
           List.iter2 (fun ((_, (ix, idir, typ, ilst)) as inr) -> function
		       | VRF (id, []) ->
                           newiolst := PORT("", id, idir, ix, [VRF(id, [])]) :: !newiolst;
                           newinnerlst := inr :: !newinnerlst;
		       | PORT (o, id_i, Dvif, idx, VRF (id, []) :: []) as pat ->
                           if List.mem_assoc id !(modul.inst) then
			       let (inam,_) = List.assoc id !(modul.inst) in
			       if Hashtbl.mem interfaces inam then (match idir with Dinam iport ->
				  begin
				  let (src, lin, intf, imp) = Hashtbl.find interfaces inam in
				  let imp' = List.filter (function IMP _ -> true | _ -> false) imp in 
                                  let imp'' = List.map (function IMP(_, str, lst) -> (str,lst) | _ -> ("",[])) imp' in
                                  if List.mem_assoc iport imp'' then
				  List.iter (function IMRF (o, nam, dir, []) ->
                                        (* print_endline (inam^":"^iport^":"^nam^":"^id_i); *)
                                        let (idx, kind, _) = List.assoc nam !(intf.v) in
					newiolst := PORT(o, id_i^"_"^nam, dirop dir, idx, [VRF(id^"_"^nam, [])]) :: !newiolst;
				        newinnerlst := (id_i^"_"^nam, (idx, dirop dir, typ, ilst)) :: !newinnerlst;
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
(*
		       | RNG [CNST (lft, _, []); CNST (rght, _, [])] -> fprintf stdout "%s" ("/* ["^lft^":"^rght^"] */")
*)
		       | oth -> portothlst := oth :: !portothlst; failwith "portothlst"
		       ) previolst iolst;
           let newinnerlst = List.rev !newinnerlst in
	   let kind_opt = kind^"_opt" in
           if not (Hashtbl.mem modules_opt kind_opt) then
               begin
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

let dumpform f f' source = 
    let fd = open_out (outtcl f') in
    let srcpath = try Sys.getenv "XMLSRCPATH" with err -> "." in
    Printf.fprintf fd "#!/opt/synopsys/fm_vO-2018.06-SP3/bin/fm_shell -f\n";
    Printf.fprintf fd "set hdlin_warn_on_mismatch_message \"FMR_ELAB-115 FMR_ELAB-117 FMR_ELAB-146 FMR_ELAB-147 FMR_VLOG-928\"\n";
    Printf.fprintf fd "read_sverilog -container r -libname WORK -12 { \\\n";
    let plst = ref [] in Hashtbl.iter (fun _ (s,_,_) -> plst := s :: !plst) packages;
    let iflst = List.map snd (if Hashtbl.mem hierarchy f then Hashtbl.find hierarchy f else []) in
    let hlst = List.sort_uniq compare (source :: List.map (fun k -> let (s,l,_) = if Hashtbl.mem modules k then Hashtbl.find modules k else ("not_found", 0, empty_itms ()) in s) iflst) in
    let slst = !plst @ hlst in
    List.iter (fun src -> if src.[0] == '/' then Printf.fprintf fd "%s \\\n" src else Printf.fprintf fd "%s/%s \\\n" srcpath src) slst;
    Printf.fprintf fd "}\n";
    Printf.fprintf fd "set_top r:/WORK/%s\n" f;
    Printf.fprintf fd "read_sverilog -container i -libname WORK -12 { \\\n";
    let hlst' = List.sort_uniq compare (f' :: iflst) in
    List.iter (fun nam -> Printf.fprintf fd "%s \\\n" (outnamopt nam)) hlst';
    Printf.fprintf fd "}\n";
    Printf.fprintf fd "set_top i:/WORK/%s\n" f';
    Printf.fprintf fd "match\n";
    Printf.fprintf fd "report_potentially_constant_registers\n";
    Printf.fprintf fd "verify\n";
    Printf.fprintf fd "report_failing_points -inputs unmatched -inputs undriven\n";
    Printf.fprintf fd "analyze_points -all\n";
    Printf.fprintf fd "quit\n";
    close_out fd;
    Unix.chmod (outtcl f') 0o740
    
let translate errlst xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    let (line,range) = match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos) | None -> (0, (0,0)) in
    let rwxml = rw' errlst xml in
    catitm "" (empty_itms ()) rwxml;
    let top = snd(List.hd !top) in
    print_endline ("toplevel is "^top);
    let (topsrc, topline, topmodul) as tophash = Hashtbl.find modules top in
    iterate top tophash;
    let top_opt = top^"_opt" in
    dumpform top top_opt topsrc;
    let mods = ref [] in
    Hashtbl.iter (fun k x -> let d = reformat0 (dump k x) in mods := reformat2 (reformat1 d) :: !mods) modules_opt;
    let mods = List.sort compare !mods in
    let separate = try int_of_string (Sys.getenv "VXML_SEPARATE") > 0 with _ -> false in
    let indent = ref 0 in
    if separate then
        begin
        List.iter (function (NL :: MODULE :: SP :: IDENT k :: tl) as lst ->
        let fd = open_out (outnam k) in
        List.iter (tokenout fd indent) lst;
        close_out fd
        | _ -> ()) mods
        end
    else
        begin
        let fd = open_out (outnam top_opt) in
        output_string fd "`default_nettype none\n";
        List.iter (fun itm ->
            tokenout fd indent itm;
        ) (List.flatten mods);
        close_out fd;
        end;
    (line,range,rwxml,xml)
    

