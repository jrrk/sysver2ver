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
| TYPMEMBER of int*int
| TYPENUM of string * int * (int*int)
| TYPCONST

type typenc =
| UNKDTYP
| PACKADTYP
| UNPACKADTYP
| CNSTDTYP
| BASDTYP
| STRDTYP
| UNIDTYP
| REFDTYP
| ENUMDTYP
| MEMBDTYP
| PARMTDTYP
| IFCRFDTYP of string
| TYPDF of string

type typetable_t = typenc*string*typmap*typmap list

type cexp =
| ERR of string
| BIN of char
| HEX of int
| SHEX of int
| STRING of string
| FLT of float
| BIGINT of Big_int.big_int

type stage =
| FIRSTG
| IOSTG
| VARSTG
| JMPSTG
| BDYSTG

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
| TYP of typenc * int * int * rw list
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
| ASGN of bool * string * rw list
| ARITH of arithop * rw list
| LOGIC of logop * rw list
| CMP of cmpop * rw list
| FRF of string * string * rw list
| XRF of string * string * string * string * dirop
| PKG of string * string * rw list
| CAT of string * rw list
| CPS of string * rw list
| CND of string * rw list
| REPL of string * int * rw list
| MODUL of string * string * string * rw list
| BGN of string option * rw list
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
| FORSTMT of (string * string * cmpop * string * (int * cexp) * (int * cexp) * (int * cexp) * rw list)
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
| BEGIN of string option
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
| LOGIC
| WIRE
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
| FUNCTION
| ENDFUNCTION
| TASK
| ENDTASK
| MODULE
| ENDMODULE
| INITIAL
| FINAL

type itms = { 
  io: (string*(string*int*dirop*string*(int*cexp) list)) list ref;
  v: (string*(string*int*string*int)) list ref;
  iv: (string*(string*int*rw list*int)) list ref;
  ir: (string*string*int) list ref;
  ca: (string*rw*rw) list ref;
  typ: (string*int*int) list ref;
  alwys: (string*rw*rw list) list ref;
  init: (string*token*rw list) list ref;
  func: (string*(string*int*rw list*itms)) list ref;
  task: (string*string*rw list*itms) list ref;
  gen: (string*rw list) list ref;
  imp : (string*string*string) list list ref;
  inst: (string*(string*string*rw list)) list ref;
  cnst: (string*(int*cexp)) list ref;
  needed: string list ref;
}

type xmlattr = {
    anchor: string;
    errlst: Xml.xml list ref;
    names: (string*int) list ref;
    }
    
let modules = Hashtbl.create 255
let modules_opt = Hashtbl.create 255
let packages = Hashtbl.create 255
let interfaces = Hashtbl.create 255
let files = Hashtbl.create 255
let hierarchy = Hashtbl.create 255
let functable = Hashtbl.create 255
let (typetable : (int, typetable_t) Hashtbl.t) = Hashtbl.create 255

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
let smpothlst = ref []
let xrflst = ref []
let smplopt = ref None
let selopt = ref None
let optopt = ref None
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

let dumptyp = function
| UNKDTYP -> "UNKDTYP"
| PACKADTYP -> "PACKADTYP"
| UNPACKADTYP -> "UNPACKADTYP"
| CNSTDTYP -> "CNSTDTYP"
| BASDTYP -> "BASDTYP"
| STRDTYP -> "STRDTYP"
| UNIDTYP -> "UNIDTYP"
| REFDTYP -> "REFDTYP"
| ENUMDTYP -> "ENUMDTYP"
| MEMBDTYP -> "MEMBDTYP"
| PARMTDTYP -> "PARMTDTYP"
| IFCRFDTYP str -> "IFCRFDTYP "^str
| TYPDF str -> "TYPDF "^str

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

let hex_to_bigint s = let rslt = ref Big_int.zero_big_int in String.iter (function
| '0'..'9' as ch -> rslt := Big_int.add_int_big_int (int_of_char ch - int_of_char '0') (Big_int.mult_int_big_int 16 !rslt)
| 'a'..'f' as ch -> rslt := Big_int.add_int_big_int (int_of_char ch - int_of_char 'a' + 10) (Big_int.mult_int_big_int 16 !rslt)
| ch -> failwith (String.make 1 ch)) s; !rslt

let rec hex_of_bigint w n =
let (q,r) = Big_int.quomod_big_int n (Big_int.big_int_of_int 16) in
(if (w > 4) then hex_of_bigint (w-4) q else "")^String.make 1 ("0123456789abcdef".[Big_int.int_of_big_int r])

let decode str = try
  let h = ref 0 and str' = Bytes.make (String.length str / 2) ' ' in
  let mychar_of_int x = if x >= 32 && x <= 127 then char_of_int x else failwith "ascii" in
  String.iteri (fun ix -> function
    | '0'..'9' as ch -> dbg ix ch !h;
        h := !h * 16 + (int_of_char ch - int_of_char '0');
        if ix land 1 = 1 then begin Bytes.set str' (ix/2) (mychar_of_int !h); h := 0; dbg ix ch !h;end
    | 'a'..'f' as ch -> dbg ix ch !h;
        h := !h * 16 + (int_of_char ch - int_of_char 'a' + 10);
        if ix land 1 = 1 then begin Bytes.set str' (ix/2) (mychar_of_int !h); h := 0; dbg ix ch !h; end
    | _ -> h := -1) str;
  STRING (Bytes.to_string str') with err -> BIGINT (hex_to_bigint str)

let cexp exp = match exp.[0] with
| '"' -> let n = String.length exp - 2 in let s = String.sub exp 1 n in (n, STRING s)
| _ -> try Scanf.sscanf exp "%d'h%x" (fun b n -> (b, HEX n)) with err ->
    try Scanf.sscanf exp "%d'sh%x" (fun b n -> (b, SHEX n)) with err ->
    try Scanf.sscanf exp "%d'bx" (fun b -> (b, BIN 'x')) with err ->
    try Scanf.sscanf exp "%d'h%s" (fun b s -> (b, decode s)) with err ->
    try Scanf.sscanf exp "%f" (fun f -> (64, FLT f)) with err -> (-1,ERR exp)

let rec typmap = function
| [] -> TYPNONE
| [("left", lft); ("right", rght)] -> TYPRNG(int_of_string lft, int_of_string rght)
| oth -> mapothlst := oth :: !mapothlst; failwith "mapothlst"

let rec subtypmap = function
| RNG [CNST ((b,(HEX n|SHEX n)), _, []); CNST ((b',(HEX n'|SHEX n')), _, [])] -> TYPRNG(n,n')
| EITM ("enumitem", itm, "", n, [CNST ((w',(HEX n'|SHEX n')), _, [])]) -> TYPENUM(itm, n, (w',n'))
| TYP (MEMBDTYP, sub, idx, []) -> TYPMEMBER(idx, sub)
| oth -> subothlst := oth :: !subothlst; failwith "subothlst"

let fortailmatch ix' = function
| ASGN(dly, _, ARITH (Aadd, CNST (inc, _, []) :: VRF (ix'', []) :: []) :: VRF (ix''', []) :: []) :: tl -> (ix'=ix'') && (ix''=ix''')
| _ -> false

let forinc = function
| ASGN(dly,_, ARITH (Aadd, CNST (inc, _, []) :: VRF (ix'', []) :: []) :: VRF (ix''', []) :: []) :: tl -> (inc,List.rev tl)
| tl -> ((0,ERR ""),List.rev tl)

let fold1 fn = function
| [] -> failwith "should never occur, just to keep type system happy"
| (hd::tl) -> List.fold_left fn hd tl

let rec jump_opt origin = function
| JMPL (_, JMPL (orig, lst) :: []) :: [] -> jump_opt origin (JMPL (orig, lst) :: [])
| JMPL (orig, lst) :: [] -> JMPL (orig, lst)
| tl -> JMPL (origin, tl)

let while_opt origin lbl = function
| VAR (_, ix'', _, ("int"|"integer"|"logic" as kind)) :: ASGN(dly, _, CNST (strt, _, []) :: VRF (ix, []) :: []) ::
        JMPL (_, 
           (WHL
            (CMP (cmpop, CNST (stop, _, []) :: VRF (ix', []) :: []) ::
             stmtlst)) :: []) :: [] when (ix=ix') && (ix'=ix'') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,kind,cmpop,ix,strt,stop,inc,stmts)
| VAR (_, ix'', _, ("int"|"integer"|"logic" as kind)) :: ASGN(dly,_, CNST (strt, _, []) :: VRF (ix, []) :: []) ::
           WHL
            (CMP (cmpop, CNST (stop, _, []) :: VRF (ix', []) :: []) ::
             stmtlst) :: [] when (ix=ix') && (ix'=ix'') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,kind,cmpop,ix,strt,stop,inc,stmts)
| VAR _ :: ASGN(dly,_, a :: WHL (b :: stmtlst) :: []) :: [] as xlst' -> forlst := (a,b,stmtlst) :: !forlst; BGN (lbl, xlst')
| ASGN(dly,_, CNST (strt, _, []) :: VRF (ix, []) :: []) ::
           WHL
            (CMP (cmpop, CNST (stop, _, []) :: VRF (ix', []) :: []) ::
             stmtlst) :: [] when (ix=ix') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,"",cmpop,ix,strt,stop,inc,stmts)
| ASGN(dly,_, a :: WHL (b :: stmtlst) :: []) :: [] as xlst' -> forlst := (a,b,stmtlst) :: !forlst; BGN (lbl, xlst')
| xlst' -> BGN (lbl, xlst')

let dlyenc = function
| "assign" -> false
| "assigndly" -> true
| _ -> failwith "dlyenc"

let typenc = function
| "packarraydtype" -> PACKADTYP
| "unpackarraydtype" -> UNPACKADTYP
| "constdtype" -> CNSTDTYP
| "basicdtype" -> BASDTYP
| "structdtype" -> STRDTYP
| "uniondtype" -> UNIDTYP
| "refdtype" -> REFDTYP
| "enumdtype" -> ENUMDTYP
| "memberdtype" -> MEMBDTYP
| "paramtypedtype" -> PARMTDTYP
| _ -> failwith "typenc"

let rec simplify_exp anchor tmpl = function
| VRF (_, []) as vrf -> vrf
| CNST _ as cst -> cst
| UNRY (op, expr1 :: []) -> UNRY (op, simplify_exp anchor tmpl expr1 :: [])
| CMP (op, expr1 :: expr2 :: []) -> CMP (op, simplify_exp anchor tmpl expr1 :: simplify_exp anchor tmpl expr2 :: [])
| LOGIC (op, exprlst) -> LOGIC (op, List.map (simplify_exp anchor tmpl) exprlst)
| ARITH (op, exprlst) -> ARITH (op, List.map (simplify_exp anchor tmpl) exprlst)
| ASEL (VRF _ as vrf :: expr1 :: []) -> ASEL (vrf :: simplify_exp anchor tmpl expr1 :: [])
| ASEL (ASEL _ as multi :: expr :: []) -> ASEL (simplify_exp anchor tmpl multi :: simplify_exp anchor tmpl expr :: [])
| CND (origin, exprlst) -> CND (origin, List.map (simplify_exp anchor tmpl) exprlst)
| CPS (origin, exprlst) -> CPS (origin, List.map (simplify_exp anchor tmpl) exprlst)
| CAT (origin, exprlst) -> CAT (origin, List.map (simplify_exp anchor tmpl) exprlst) 
| REPL (origin, tid, arg :: (CNST _ as cst) :: []) -> REPL (origin, tid, simplify_exp anchor tmpl arg :: cst :: [])
| IRNG (origin, exprlst) -> IRNG (origin, List.map (simplify_exp anchor tmpl) exprlst)
| FRF (origin, fref, arglst) -> FRF (origin, fref, List.map (simplify_exp anchor tmpl) arglst)
| SFMT (fmt, exprlst) -> SFMT (fmt, List.map (simplify_exp anchor tmpl) exprlst)
| XRF _ as xrf -> xrf
| SEL (orig, (VRF _ | XRF _ |ASEL _ as expr1) :: expr2 :: (CNST _ as expr3) :: []) ->
    SEL (orig, List.map (simplify_exp anchor tmpl) (expr1 :: expr2 :: expr3 :: []))
| SEL (orig, (oth :: (CNST ((_, HEX lo'), _, []) as lo) :: (CNST ((_, HEX wid'), _, []) as wid) :: [])) ->
        let idx' = lo'+wid' in
        let uniq = 1000000 + List.length !tmpl * 1000 + idx' in
        let t = "__tmp"^string_of_int uniq in
        let tmp = VRF (t, []) and idx = -uniq in
        Hashtbl.replace typetable idx (BASDTYP, "logic", TYPRNG(idx'-1,0), []); 
        let smpl = simplify_exp anchor tmpl oth in
        tmpl := !tmpl @ TMPVAR(anchor, t, idx, []) :: ASGN(false, orig, smpl :: tmp :: []) :: [];
        SEL (orig, tmp :: lo :: wid :: [])
| SEL (origin, expr1 :: lo :: wid :: []) as sel -> smplopt := Some sel; failwith "simplify_exp: smplopt"
| oth -> smpothlst := oth :: !smpothlst; oth

let simplify_asgn dly' anchor dst = function
| CND (origin, cnd :: lft :: rght :: []) -> 
            IF(origin, cnd :: ASGN(dly', origin, lft :: dst :: []) :: ASGN(dly', origin, rght :: dst :: []) :: [])
| src ->
        let tmpl = ref [] in
        let src' = simplify_exp anchor tmpl src in
        let dst' = simplify_exp anchor tmpl dst in
        ternothlst := (src,dst) :: !ternothlst;
        match !tmpl with
            | [] -> ASGN(dly', anchor, src::dst::[])
            | TMPVAR(_, t, idx, []) :: ASGN(false, _, smpl :: tmp :: []) :: [] when dst=dst' ->
                TMPVAR(anchor, t, idx, ASGN(false, anchor, smpl :: tmp :: []) :: ASGN(dly', anchor, src' :: dst :: []) :: [])
            | TMPVAR(_, t, idx, []) :: ASGN(false, _, smpl :: tmp :: []) :: [] when src=src' ->
                TMPVAR(anchor, t, idx, ASGN(false, anchor, smpl :: tmp :: []) :: ASGN(dly', anchor, src :: dst' :: []) :: [])
            | TMPVAR(_, t, idx, []) :: ASGN(false, _, smpl :: tmp :: []) ::
              TMPVAR(_, t', idx', []) :: ASGN(false, _, smpl' :: tmp' :: []) :: [] ->
                BGN (None, TMPVAR(anchor, t, idx, ASGN(false, anchor, smpl :: tmp :: []) :: []) ::
                            TMPVAR(anchor, t', idx', ASGN(false, anchor, smpl' :: tmp' :: []) :: []) :: ASGN(dly', anchor, src' :: dst' :: []) :: [])
            | _ -> failwith "simplify_asgn"

let rec rw' attr = function
| Xml.Element ("verilator_xml", [], xlst) -> XML (List.map (rw' attr) xlst)
| Xml.Element ("files"|"module_files" as fils, [], xlst) -> FILS (fils, List.map (rw' attr) xlst)
| Xml.Element ("file", [("id", encoding); ("filename", nam); ("language", "1800-2017")], []) -> FIL (encoding, nam)
| Xml.Element ("netlist", [], xlst) -> NTL (List.map (rw' attr) (List.rev xlst))
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
    IO (origin, nam, int_of_string tid, dirop dir, typ, List.map (rw' attr) xlst)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", ("ifaceref" as typ)); ("origName", nam')], []) ->
    let (vif, sub) = chkvif nam in
    if vif then
       VAR (origin, sub, int_of_string tid, typ)
       else
       begin
           let idx = int_of_string tid in
           let found = Hashtbl.mem typetable idx in
           print_endline (typ^":"^nam^":"^string_of_bool found);
           let (dtype, dir, _, _) = Hashtbl.find typetable idx in
           attr.names := (nam, idx) :: !(attr.names);
	   IO (origin, nam, idx, Dinam dir, "logic", [])
       end
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               VAR (origin, nam, int_of_string tid, typ)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],
               [Xml.Element ("const", [("fl", _); ("name", _); ("dtype_id", cid)], []) as lev]) ->
                             IVAR (origin, nam, int_of_string tid, [rw' attr lev], int_of_string cid)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')],

	       [Xml.Element ("initarray", [("fl", _); ("dtype_id", cid)], initlst)]) ->
                             IVAR (origin, nam, int_of_string tid, List.map (rw' attr) initlst, int_of_string cid)
| Xml.Element ("const", [("fl", _); ("name", value); ("dtype_id", tid)], xlst) ->
               
               CNST (cexp value, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("contassign", [("fl", origin); ("dtype_id", tid)], xlst) -> CA (origin, List.map (rw' attr) xlst)
| Xml.Element ("not"|"negate"|"extend"|"extends"|"lognot" as op, [("fl", origin); ("dtype_id", tid)], xlst) -> UNRY (unaryop op, List.map (rw' attr) xlst)
| Xml.Element ("varref", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> VRF (snd (chkvif nam), List.map (rw' attr) xlst)
| Xml.Element ("instance", [("fl", origin); ("name", nam); ("defName", dnam); ("origName", nam')], xlst) ->
               INST (origin, nam, (dnam, List.map (rw' attr) xlst))
| Xml.Element ("range", [("fl", _)], xlst) -> RNG (List.map (rw' attr) xlst)
| Xml.Element ("port", [("fl", origin); ("name", nam); ("direction", dir); ("portIndex", idx)], xlst) ->
               PORT (origin, nam, dirop dir, int_of_string idx, List.map (rw' attr) xlst)
| Xml.Element ("port", [("fl", origin); ("name", nam); ("portIndex", idx)], xlst) -> let (vif,sub) = chkvif nam in
               PORT (origin, sub, Dvif, int_of_string idx, List.map (rw' attr) xlst)
| Xml.Element ("sel", [("fl", origin); ("dtype_id", tid)], xlst) -> SEL (origin, List.map (rw' attr) xlst)
| Xml.Element ("arraysel", [("fl", origin); ("dtype_id", tid)], xlst) -> ASEL (List.map (rw' attr) xlst)
| Xml.Element ("always", [("fl", origin)], xlst) -> ALWYS (origin, List.map (rw' {attr with anchor=origin}) xlst)
| Xml.Element ("sentree", [("fl", origin)], xlst) -> SNTRE (List.map (rw' attr) xlst)
| Xml.Element ("senitem", [("fl", origin); ("edgeType", etyp)], xlst) -> SNITM (etyp, List.map (rw' attr) xlst)
| Xml.Element ("begin", [("fl", origin); ("name", namedblk)], xlst) -> while_opt origin (Some namedblk) (List.map (rw' attr) xlst)
| Xml.Element ("begin", [("fl", origin)], xlst) -> while_opt origin None (List.map (rw' attr) xlst)
| Xml.Element (("assign"|"assigndly") as dly, [("fl", origin); ("dtype_id", tid)], hd::tl::[]) ->
    let src = rw' attr hd and dst = rw' attr tl in
    simplify_asgn (dlyenc dly) attr.anchor dst src
| Xml.Element ("if", [("fl", origin)], xlst) -> IF (origin, List.map (rw' attr) xlst)
| Xml.Element ("add"|"sub"|"mul"|"muls" as op, [("fl", _); ("dtype_id", tid)], xlst) -> ARITH (arithop op, List.map (rw' attr) xlst)
| Xml.Element ("and"|"redand"|"or"|"redor"|"xor"|"redxor"|"xnor"|"redxnor"|"shiftl"|"shiftr"|"shiftrs" as log,
               [("fl", _); ("dtype_id", tid)], xlst) -> LOGIC (logop log, List.map (rw' attr) xlst)
| Xml.Element ("eq"|"neq"|"gt"|"gts"|"gte"|"gtes"|"eqwild"|"neqwild"|"ltes"|"lte"|"lt"|"lts" as cmp, [("fl", _); ("dtype_id", tid)], xlst) ->
    CMP (cmpop cmp, List.map (rw' attr) xlst)
| Xml.Element ("initial"|"final" as action, [("fl", origin)], xlst) -> INIT (origin, action, List.map (rw' attr) xlst)
| Xml.Element ("package", [("fl", orig); ("name", nam); ("origName", nam')], xlst) -> PKG (orig, nam, List.map (rw' attr) xlst)
| Xml.Element ("typedef", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) -> let idx = int_of_string tid in TYP (TYPDF nam, idx, idx, List.map (rw' attr) xlst)
| Xml.Element ("func", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) ->
    FNC (origin, nam, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("jumplabel", [("fl", origin)], xlst) -> jump_opt origin (List.map (rw' attr) xlst)
| Xml.Element ("jumpgo", [("fl", origin)], xlst) -> JMPG (origin, List.map (rw' attr) xlst)
| Xml.Element ("concat", [("fl", origin); ("dtype_id", tid)], xlst) -> CAT (origin, List.map (rw' attr) xlst)
| Xml.Element ("cvtpackstring", [("fl", origin); ("dtype_id", tid)], xlst) -> CPS (origin, List.map (rw' attr) xlst)
| Xml.Element ("cond", [("fl", origin); ("dtype_id", tid)], xlst) -> CND (origin, List.map (rw' attr) xlst)
| Xml.Element ("sformatf", [("fl", _); ("name", fmt); ("dtype_id", tid)], xlst) -> SFMT (fmt, List.map (rw' attr) xlst)
| Xml.Element ("module", ("fl", origin) :: ("name", nam) :: ("origName", nam') :: attr', xlst) ->
    MODUL (origin, nam, nam', List.map (rw' {attr with anchor=origin;names=ref []}) xlst)
| Xml.Element ("case", [("fl", origin)], xlst) -> CS (origin, List.map (rw' attr) xlst)
| Xml.Element ("caseitem", [("fl", origin)], xlst) -> CSITM (origin, List.map (rw' attr) xlst)
| Xml.Element ("while", [("fl", _)], xlst) -> WHL (List.map (rw' attr) xlst)
| Xml.Element ("insiderange", [("fl", origin); ("dtype_id", tid)], xlst) -> IRNG (origin, List.map (rw' attr) xlst)
| Xml.Element ("funcref", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) -> FRF (origin, nam, List.map (rw' attr) xlst)
| Xml.Element ("varxref", [("fl", origin); ("name", nam); ("dtype_id", tid); ("dotted", dotted)], []) ->
    let dirop = if List.mem_assoc dotted !(attr.names) then
        begin
        let idx = List.assoc dotted !(attr.names) in
        let (dtype, _, _, _) = Hashtbl.find typetable idx in
        match dtype with
            | IFCRFDTYP ifc -> print_endline ifc; Dinam ifc
(* *)
            | oth -> print_endline (dumptyp oth); Dunknown
(* *)
        end
    else
        begin
        print_endline (dotted^" is not io");
        Dunknown
        end in
    XRF (origin, nam, tid, dotted, dirop)
| Xml.Element ("arg", [("fl", _)], xlst) -> ARG (List.map (rw' attr) xlst)
| Xml.Element ("initarray"|"streaml"|"powsu"|"realtobits"|"itord"|"rand" as op, [("fl", origin); ("dtype_id", tid)], xlst) -> SYS (origin, "$"^op, List.map (rw' attr) xlst)
| Xml.Element ("replicate", [("fl", origin); ("dtype_id", tid)], xlst) -> REPL (origin, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("iface", [("fl", origin); ("name", bus); ("origName", bus')], xlst) -> IFC (origin, bus, List.map (rw' attr) xlst)
| Xml.Element ("ifacerefdtype", [("fl", _); ("id", num); ("modportname", nam)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num in
    Hashtbl.add typetable idx (IFCRFDTYP nam, nam, TYPNONE, List.map subtypmap xlst'); TYP (IFCRFDTYP nam, idx, 0, xlst')
| Xml.Element ("modport", [("fl", origin); ("name", port)], xlst) -> IMP (origin, port, List.map (rw' attr) xlst)
| Xml.Element ("modportvarref", [("fl", origin); ("name", member); ("direction", dir)], xlst) -> IMRF (origin, member, dir, List.map (rw' attr) xlst)
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype" as dtyp', ("fl", _) :: ("id", num) :: rnglst, xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and dtyp = typenc dtyp' in
    (match rnglst with
      | ("name", nam) :: tl ->
          Hashtbl.add typetable idx (dtyp,nam,typmap tl,List.map subtypmap xlst')
      | _ ->
      Hashtbl.add typetable idx (dtyp,"",typmap rnglst,List.map subtypmap xlst'));
    TYP(dtyp, idx, idx, xlst')
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp', [("fl", origin); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and sub = int_of_string subtype and dtyp = typenc dtyp' in
    Hashtbl.add typetable idx (dtyp,nam,SUBTYP sub,List.map subtypmap xlst');
    TYP(dtyp, sub, idx, xlst')
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp', [("fl", origin); ("id", num); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and sub = int_of_string subtype and typid = typenc dtyp' in    
    Hashtbl.add typetable idx (typid,"",SUBTYP sub,List.map subtypmap xlst');
    TYP(typid, sub, idx, xlst')
| Xml.Element ("enumitem" as dtyp, [("fl", origin); ("name", nam); ("dtype_id", num)], xlst) -> EITM (dtyp, nam, "", int_of_string num, List.map (rw' attr) xlst)
| Xml.Element ("cells", [], xlst) -> CELLS(List.map (rw' attr) xlst)
| Xml.Element ("cell", [("fl", origin); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    CELL(origin, nam, subnam, hier, List.map (rw' attr) xlst)
| Xml.Element ("display", [("fl", origin); ("displaytype", nam)], xlst) -> DSPLY (origin, nam, List.map (rw' attr) xlst)
| Xml.Element ("readmem", [("fl", origin)], xlst) -> SYS (origin, "$readmemh", List.map (rw' attr) xlst)
| Xml.Element (("fopen"|"fclose"|"typetable"|"finish"|"stop" as sys), [("fl", origin)], xlst) -> SYS (origin, "$"^sys, List.map (rw' attr) xlst)
| Xml.Element (("task"|"taskref") as tsk, [("fl", origin); ("name", nam)], xlst) -> TASK(origin, tsk, nam, List.map (rw' attr) xlst)
| Xml.Element ("valueplusargs", [("fl", origin); ("dtype_id", tid)], xlst) -> VPLSRGS(origin, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("testplusargs", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) ->
    TPLSRGS(origin, nam, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("modportftaskref", [("fl", origin); ("name", nam)], []) -> MODPORTFTR (origin, nam)
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> attr.errlst := err :: !(attr.errlst); failwith ("XML element "^str^" not supported")

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
inst=ref [];
cnst=ref [];
needed=ref [] }

let rev_itms prev = {
io=ref (List.rev !(prev.io));
v=ref (List.rev !(prev.v));
iv=ref (List.rev !(prev.iv));
ir=ref (List.rev !(prev.ir));
ca=ref (List.rev !(prev.ca));
typ=ref (List.rev !(prev.typ));
alwys=ref (List.rev !(prev.alwys));
init=ref (List.rev !(prev.init));
func=ref (List.rev !(prev.func));
task=ref (List.rev !(prev.task));
gen=ref (List.rev !(prev.gen));
imp=ref (List.rev !(prev.imp));
inst=ref (List.rev !(prev.inst));
cnst=ref (List.rev !(prev.cnst));
needed=ref (List.rev !(prev.needed)) }

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
inst=ref !(prev.inst);
cnst=ref !(prev.cnst);
needed=ref !(prev.needed) }

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
| BIGINT n :: [] -> BIGINT n
| BIGINT n :: BIGINT m :: tl -> cadd (BIGINT (Big_int.add_big_int n m) :: tl)
| (HEX n | SHEX n) :: BIGINT m :: tl -> cadd (BIGINT (Big_int.add_big_int (Big_int.big_int_of_int n) m) :: tl)
| BIGINT n :: (HEX m | SHEX m) :: tl -> cadd (BIGINT (Big_int.add_big_int n (Big_int.big_int_of_int m)) :: tl)
| BIN 'x' :: tl -> cadd (BIN 'x' :: tl)
| BIN _ :: tl -> cadd (BIN 'x' :: tl)
| HEX n :: HEX m :: tl -> cadd (HEX (n+m) :: tl)
| SHEX n :: HEX m :: tl -> cadd (HEX (n+m) :: tl)
| HEX n :: SHEX m :: tl -> cadd (HEX (n+m) :: tl)
| SHEX n :: SHEX m :: tl -> cadd (SHEX (n+m) :: tl)
| (HEX _ | SHEX _ | BIGINT _) ::(ERR _|BIN _|STRING _|FLT _):: tl -> ERR "cadd"

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
| SEL (origin, ((VRF _ | XRF _ | ASEL _) as expr1) :: (CNST((szlo,lo'),_,_)) :: (CNST((szw,wid'),_,_)) :: []) ->
    expr expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: NUM lo' :: RBRACK :: []
    | _ -> LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: [])
| SEL (origin, ((VRF _ | XRF _ | ASEL _) as expr1) :: expr2 :: CNST((szw,wid'),_,_) :: []) ->
    expr expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: expr expr2 @ [RBRACK]
    | _ -> LBRACK :: expr expr2 @ [PLUS;COLON] @ (NUM wid' :: RBRACK :: []))
| SEL (origin, expr1 :: CNST ((32,(HEX 0 | SHEX 0)), idx, []) :: (CNST((szw,HEX wid'),_,_)) :: []) ->
    expr (LOGIC (Land, expr1 :: CNST ((wid', HEX ((1 lsl wid') -1)), idx, []) :: []))
| SEL (o, CND (origin, expr1 :: lft :: rght :: []) :: expr2 :: expr3 :: []) ->
    LPAREN :: expr expr1 @ [QUERY] @ expr (SEL (o, lft :: expr2 :: expr3 :: [])) @ [COLON] @ expr (SEL (o, rght :: expr2 :: expr3 :: [])) @ [RPAREN]
| SEL (orig, (UNRY (Uextend, VRF (id, []) :: []) :: (CNST _ as expr2) :: (CNST _ as expr3) :: [])) ->
    expr (SEL (orig, VRF (id, []) :: expr2 :: expr3 :: []))
| SEL (orig, (CNST ((32, SHEX n), idx, []) :: CNST ((32, HEX lo'), _, []) :: CNST ((32, HEX wid'), _, []) :: [])) ->
    SIZED (wid', SHEX (n asr lo')) :: []
    
| ASEL (VRF (lval, []) :: expr1 :: []) -> IDENT lval :: LBRACK :: expr expr1 @ [RBRACK]
| ASEL (ASEL _ as multi :: expr' :: []) -> expr multi @ LBRACK :: expr expr' @ [RBRACK]
| CND (origin, expr1 :: lft :: rght :: []) -> LPAREN :: expr expr1 @ [QUERY] @ expr lft @ [COLON] @ expr rght @ [RPAREN]
| CAT (origin, expr1 :: expr2 :: []) -> LCURLY :: expr expr1 @ [COMMA] @ expr expr2 @ [RCURLY]
| FRF (origin, fref, arglst) -> let delim = ref LPAREN in
    let lst = IDENT fref :: List.flatten (List.map (function
        | ARG (arg :: []) -> let lst = !delim :: expr arg in delim := COMMA; lst
        | _ -> [QUERY]) arglst) in
    lst @ (if !delim = LPAREN then [LPAREN;RPAREN] else [RPAREN])
| REPL (origin, tid, arg :: CNST ((sz,n'),_,_) :: []) -> LCURLY :: NUM n' :: LCURLY :: expr arg @ [RCURLY;RCURLY]
| IRNG (origin, expr2 :: expr1 :: []) -> LBRACK :: expr expr1 @ [COLON] @ expr expr2 @ [RBRACK]
| XRF (origin, id, tid, dotted, dirop) as xrf ->
    xrflst := xrf :: !xrflst;
    IDENT (dotted^(match dirop with Dinam _ -> "_" | _ -> ".")^id) :: []
| TPLSRGS (origin, id, tid, []) -> IDENT "$test$plusargs" :: LPAREN :: DQUOTE :: IDENT id :: DQUOTE :: RPAREN :: []
| VPLSRGS (origin, tid, CNST ((len, fmt), _, []) :: VRF (arg, []) :: []) -> IDENT "$value$plusargs" :: LPAREN :: NUM fmt :: COMMA :: IDENT arg :: RPAREN :: []
| SYS (origin, fn, arglst) -> IDENT fn :: LPAREN :: eiter SP arglst @ [RPAREN]

| SEL (origin, expr1 :: lo :: wid :: []) as sel -> selopt := Some sel; failwith "expr: selopt"

| oth -> exprothlst := oth :: !exprothlst; failwith "exprothlst"
and eiter tok lst =
    let delim = ref tok in
    List.flatten (List.map (fun itm -> let lst' = !delim :: expr itm in delim := COMMA; lst') lst)

let rec portconn = function
| VRF (id, []) -> DOT :: IDENT id :: []
| PORT (origin, id_o, dir, idx, []) -> DOT :: IDENT id_o :: LPAREN :: RPAREN :: []
| PORT (origin, id_i, dir, idx, expr1 :: []) -> DOT :: IDENT id_i :: LPAREN :: expr expr1 @ [RPAREN]
| RNG [CNST ((_,lft), _, []); CNST ((_,rght), _, [])] -> LCOMMENT :: NUM lft :: COLON :: NUM rght :: RCOMMENT :: []
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst, _, []) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

let stmtdly = function
| false -> SP :: ASSIGNMENT :: SP :: []
| true -> SP :: ASSIGNDLY :: SP :: []

let oldsrc = ref ("",-1)

let dumpsized w = function
| BIN b -> string_of_int w^"'b"^String.make w b
| HEX n -> Printf.sprintf "%d'h%x" w n
| SHEX n -> Printf.sprintf "%d'sh%x" w n
| BIGINT n -> Printf.sprintf "%d'h%s" w (hex_of_bigint w n)
| STRING s -> "\""^String.escaped s^"\""
| FLT f -> string_of_float f
| ERR err -> ("NumberError:"^err)

let rec tokenout fd indent = function
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
| SRC (str1,int1) ->
    if (str1,int1) <> !oldsrc then begin output_string fd ("\n/* "^str1^":"^string_of_int int1^" */"); tokenout fd indent NL; end;
    oldsrc := (str1,int1);
| DEFAULT -> output_string fd "default"
| IDENT str -> output_string fd str
| NUM (BIN n) -> output_string fd (String.make 1 n)
| NUM (HEX n) -> output_string fd (string_of_int n)
| NUM (SHEX n) -> output_string fd (string_of_int n)
| NUM (BIGINT n) -> output_string fd (Big_int.string_of_big_int n)
| NUM (STRING s) -> output_string fd ("\""^String.escaped s^"\"")
| NUM (FLT f) -> output_string fd (string_of_float f)
| NUM (ERR err) -> output_string fd ("NumberError:"^err)
| SIZED (w,n) -> output_string fd (dumpsized w n)
| DIR str -> output_string fd (diropv str)
| BEGIN None -> incr indent; output_string fd "    begin"
| BEGIN (Some lbl) -> incr indent; output_string fd ("    begin:"^lbl)
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
| WIRE -> output_string fd "wire"
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
| prior :: (BEGIN _|END|ELSE as tok) :: tl when prior <> NL -> prior :: NL :: tok :: reformat1 (NL :: tl)
| (BEGIN _|END|ELSE as tok) :: post :: tl when post <> NL && post <> COLON -> NL :: tok :: NL :: post :: reformat1 tl
| DIR dir :: tl -> NL :: DIR dir :: reformat1 tl
| DOT :: tl -> NL :: DOT :: reformat1 tl
| SEMI :: IFF :: tl -> SEMI :: NL :: IFF :: reformat1 tl
| NL :: NL :: tl -> reformat1 (NL :: tl)
| oth :: tl -> oth :: reformat1 tl

let rec reformat2 = function
| [] -> []
| BEGIN lbl :: NL :: NL :: tl -> reformat2 (BEGIN lbl :: NL :: tl)
| ENDCASE :: SEMI :: NL :: END :: tl -> ENDCASE :: reformat2 ( NL :: END :: tl )
| END :: NL :: SEMI :: NL :: (END|ENDCASE as end') :: tl -> reformat2 (END :: NL :: end' :: tl)
| END :: NL :: SEMI :: NL :: ALWAYS :: tl -> END :: NL :: NL :: reformat2 (ALWAYS :: tl)
| END :: NL :: SEMI :: NL :: tl -> END :: reformat2 (SEMI :: NL :: tl)
| ELSE :: NL :: IFF :: tl -> ELSE :: SP :: IFF :: reformat2 tl
| END :: NL :: NL :: ELSE :: tl -> END :: reformat2 (NL :: ELSE :: tl)

| oth :: tl -> oth :: reformat2 tl

let reviter lst =
    let delim = ref COLON in
    List.rev (List.flatten (List.map (fun itm -> let lst' = !delim :: expr itm in delim := COMMA; lst') lst))

let num x = NUM (HEX x)

let rec cntbasic = function
| (STRDTYP,_,typmap,rw_lst) -> fold1 (+) (List.flatten (List.map cntmembers rw_lst)) :: []
| (UNIDTYP,_,typmap,rw_lst) -> fold1 (max) (List.flatten (List.map cntmembers rw_lst)) :: []
| (BASDTYP, ("logic"|"integer"|"int"|"bit"), TYPRNG(hi, lo), []) -> hi - lo + 1 :: []
| (BASDTYP, ("logic"|"bit"), TYPNONE, []) -> 1 :: []
| (BASDTYP, ("real"), TYPNONE, []) -> 64 :: []
| (BASDTYP, ("string"), TYPNONE, []) -> 1 :: []
| (IFCRFDTYP _, _, TYPNONE, []) -> 0 :: []
| (PACKADTYP, "", SUBTYP subtyp, TYPRNG(n,n')::_) -> List.map (fun itm -> itm * (n - n' + 1)) (findmembers subtyp)
| (UNPACKADTYP, "", SUBTYP subtyp, TYPRNG (n,n')::_) -> (n - n' + 1) :: findmembers subtyp
| (MEMBDTYP, id, SUBTYP subtyp, []) -> 1 :: []
| oth -> typothlst := oth :: !typothlst; failwith "typothlst"

and cntmembers = function
| TYPMEMBER (idx1, idx2) -> findmembers idx2
| oth -> memothlst := oth :: !memothlst; failwith "memothlst"

and findmembers' idx = if Hashtbl.mem typetable idx then
        let (t,s,m1,ml) as tab = Hashtbl.find typetable idx in
        (cntbasic tab, List.mem TYPCONST ml, match m1 with TYPRNG _ -> true | _ -> false) else ([], false, false)

and findmembers idx = let (widlst,cnst,rng) = findmembers' idx in widlst

let varlst modul delim idx id =
    let (widlst,cnst,rng) = findmembers' idx in
    let expand delim = fun w -> let lst = !delim :: num w :: [] in delim := STAR; lst in
    let decl = !delim :: (if cnst then WIRE else LOGIC) :: SP :: match widlst with
                | [] -> IDENT id :: []
                | 1 :: [] when idx >= 0 && not rng -> IDENT id :: []
                | n :: [] -> LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: []
                | n :: m :: [] -> LBRACK :: num (m-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: SP :: LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: []
                | oth -> let delim = ref LBRACK in List.flatten (List.map (expand delim) widlst) @ MINUS :: num 1 :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: [] in
    decl @ (if cnst && List.mem_assoc id !(modul.cnst) then SP :: ASSIGNMENT :: SP :: SIZED (List.assoc id !(modul.cnst)) :: [] else [])

let rec iter2 modul dly lst =
    List.flatten (List.map (fun itm -> cstmt modul dly itm @ [SEMI;NL]) lst)

and csitm modul dly = function
    | CSITM (origin, []) -> SEMI :: []
    | CSITM (origin, IRNG (_, [CNST ((w, HEX lo), idx, []); CNST ((w', HEX hi), idx', [])]) :: (BGN _ as st) :: []) ->
        eiter SP (Array.to_list (Array.init (hi-lo+1) (fun x -> CNST ((w, HEX (x+lo)), idx, [])))) @ COLON :: cstmt modul dly st @ NL :: []
    | CSITM (origin, cexp :: (BGN _ as st) :: []) -> expr cexp @ COLON :: cstmt modul dly st @ NL :: []
    | CSITM (origin, (CNST _ as cexp) :: []) -> expr cexp @ COMMA :: []
    | CSITM (origin, st :: []) -> DEFAULT :: COLON :: cstmt modul dly st @ SEMI :: []
    | CSITM (origin, cexplst) -> let lbl, stmts = List.partition (function CNST _ | VRF _ | LOGIC _ -> true | _ -> false) cexplst in
                                 eiter SP lbl @ COLON :: BEGIN None :: iter2 modul dly stmts @ [END;NL]
    | oth -> failwith "csothlst"

and ifcond = function
| LPAREN :: tl -> IFF :: SP :: LPAREN :: tl
| oth -> IFF :: SP :: LPAREN :: oth @ (RPAREN :: [])

and ewidth = function
| CAT (_,lst) -> fold1 (+) (List.map ewidth lst)
| SEL (_, VRF (_, []) :: CNST ((_,_), _, []) :: CNST ((_,HEX wid), _, []) :: []) -> wid
| SEL (_, UNRY (Uextends, _) :: CNST ((_,_), _, []) :: CNST ((_,HEX wid), _, []) :: []) -> wid
| CNST ((n, _), _, []) -> n
| oth -> widthlst := oth :: !widthlst; failwith "widthlst"

and cstmt modul dly = function
| JMPG (_,[]) -> []
| BGN(lbl, rw_lst) -> BEGIN lbl :: iter2 modul dly rw_lst @ [END;NL]
| IF(origin, cnd :: then_stmt :: []) -> ifcond (expr cnd) @ (SP :: cstmt modul dly then_stmt)
| IF(origin, cnd :: (BGN _ as then_stmt) :: else_stmt :: []) ->
    ifcond (expr cnd) @ (SP :: cstmt modul dly then_stmt) @ [ELSE] @ cstmt modul dly else_stmt
| IF(origin, cnd :: then_stmt :: else_stmt :: []) ->
    ifcond (expr cnd) @ (SP :: cstmt modul dly then_stmt) @ [SEMI;ELSE] @ cstmt modul dly else_stmt
| ASGN(true, origin, 
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
| ASGN(dly, origin, src :: dst :: []) ->
    expr dst @ stmtdly dly @ expr src
| CS (origin, sel :: lst) -> CASE :: LPAREN :: expr sel @ (RPAREN :: NL :: List.flatten (List.map (csitm modul dly) lst)) @ [NL;ENDCASE]
| CA(origin, rght::lft::[]) -> ASSIGN :: SP :: expr lft @ stmtdly dly @ expr rght
| VAR (origin, id, idx, _) -> varlst modul (ref SP) idx id
| FORSTMT (o,kind,cnd,ix,strt,stop,inc,stmts) ->
    FOR :: LPAREN :: (if kind <> "" then IDENT kind :: SP :: [] else []) @
    IDENT ix :: ASSIGNMENT :: SIZED strt :: SEMI ::
    SIZED stop :: CMPOP cnd :: IDENT ix :: SEMI ::
    IDENT ix :: ASSIGNMENT :: IDENT ix :: PLUS :: SIZED inc :: RPAREN ::
    BEGIN None :: iter2 modul dly stmts @ [END]
| DSPLY (origin, typ, SFMT (fmt, arglst) :: []) -> IDENT typ :: LPAREN :: DQUOTE :: IDENT fmt :: DQUOTE :: eiter COMMA arglst @ [RPAREN]
| DSPLY (origin, typ, SFMT (fmt, arglst) :: expr1 :: []) ->
    IDENT typ :: LPAREN :: expr expr1 @ (COMMA :: DQUOTE :: IDENT fmt :: DQUOTE :: COMMA :: eiter SP arglst) @ [RPAREN]
| SYS (origin, fn, arglst) -> IDENT fn :: LPAREN :: eiter SP arglst @ [RPAREN]
| CNST((s,n), _, []) -> SIZED (s,n) :: []
| TASK (origin, "taskref", nam, arglst) -> IDENT nam :: (if arglst <> [] then eiter LPAREN arglst @ [RPAREN] else [])
| JMPL(origin, rw_lst) -> BEGIN None :: iter2 modul dly rw_lst @ [NL;END;NL]
| TMPVAR (origin, nam, wid, stmtlst) -> cstmt modul dly (BGN (None, stmtlst))
(*
| WHL
    (CMP (cmpop, (CNST (stop, _, []) :: VRF (ix, []) :: [])) ::
     stmt ::
     ASGN _ :: [])
(*
 (ARITH (Aadd, (CNST (inc, _, []) :: VRF (ix', []) :: []) :: VRF (ix'', []) :: []) :: []) :: []) :: [])
*)
    -> WHILE :: []
*)
| oth -> stmtothlst := oth :: !stmtothlst; failwith "stmtothlst"

let flatten1 modul dly = function
| BGN _ :: tl as lst -> let delim = ref SP in List.flatten (List.map (fun itm -> let lst' = !delim :: cstmt modul dly itm in delim := SEMI; lst') lst)
| lst -> let delim = ref (BEGIN None) in List.flatten (List.map (fun itm -> let lst' = !delim :: cstmt modul dly itm in delim := SEMI; lst') lst) @ [SEMI;END;NL;NL]

let find_source origin =
    let last = ref 0 in
    for i = String.length origin - 1 downto 0 do
        match origin.[i] with '0'..'9' -> last := i | _ -> ();
    done;
    let k = String.sub origin 0 !last in
    let source = if Hashtbl.mem files k then Hashtbl.find files k else (origin^": origin_unknown") in
    let line = String.sub origin !last (String.length origin - !last) in
    (source, try int_of_string line with err -> 0)

let fsrc src = SRC (find_source src)

let rec cell_hier = function
| CELL (_, nam, subnam, hier, rw_lst) ->
   let hier_lst = List.flatten (List.map cell_hier rw_lst) in
   print_endline ("Cell: "^subnam);
   Hashtbl.replace hierarchy subnam hier_lst;
   (nam,subnam) :: hier_lst
| oth -> cellothlst := oth :: !cellothlst; failwith "cellothlst"

let catch_escapes = ref true

let rec optim2 = function
| [] -> []
| ASGN(dly, o1, expr :: tmp1 :: []) :: ASGN(dly', o2, SEL (_, tmp1' :: lst) :: dst :: []) ::
  ASGN(dly'', o3, expr' :: tmp2 :: []) :: ASGN(dly''', o4, SEL (_, tmp2' :: lst') :: dst' :: []) :: tl when (tmp1=tmp1') && (tmp2=tmp2') && (expr=expr') && (dly=false) && (dly'=true) && (dly''=false) && (dly'''=true) ->
    (match optim2 (ASGN(false, o1, expr :: tmp1 :: []) :: ASGN(true, o2, SEL (o2, tmp1 :: lst) :: dst :: []) :: tl) with
        | hd0::hd1::tl -> hd0::hd1::ASGN(true, o4, SEL (o4, tmp1 :: lst') :: dst' :: []) :: tl
	| _ -> failwith "optim2")
| hd :: tl -> hd :: optim2 tl

let rec optitm3 = function
| [] -> []
| BGN (Some lbl, tl) :: tl' -> BGN (Some lbl, optitm3 tl) :: optitm3 tl'
| BGN (None, tl) :: BGN (None, tl') :: tl'' -> optitm3 (BGN (None, tl @ tl') :: tl'')
| BGN (None, tl) :: tl' -> BGN (None, optitm3 tl) :: optitm3 tl'
| TMPVAR (o1,a,b,lst1) :: TMPVAR (o2,a',b',lst2) :: tl when b <= b' -> optitm3 (TMPVAR(o1,a,b,lst1@lst2) :: tl)
| TMPVAR (o,a,b,lst) :: tl -> let optlst = optim2 lst in optitmlst := (lst,optlst) :: !optitmlst; optlst @ optitm3 tl
| CS(o,rw_lst) :: tl -> CS(o,optitm3 rw_lst) :: optitm3 tl
| CAT(o,rw_lst) :: tl -> CAT(o,optitm3 rw_lst) :: optitm3 tl
| CSITM(o,rw_lst) :: tl -> CSITM(o,optitm3 rw_lst) :: optitm3 tl
| WHL(rw_lst) :: tl -> WHL(optitm3 rw_lst) :: optitm3 tl
| FORSTMT(o,kind,cmpop,ix,strt,stop,inc,rw_lst) :: tl -> FORSTMT(o,kind,cmpop,ix,strt,stop,inc,optitm3 rw_lst) :: optitm3 tl
| TASK(origin, tsk, nam, rw_lst) :: tl -> TASK(origin, tsk, nam, optitm3 rw_lst) :: optitm3 tl
| ASGN _ as oth :: tl -> oth :: optitm3 tl
| IF(origin, cnd :: then_stmt :: []) :: tl -> IF (origin, cnd :: BGN(None, optitm3 [then_stmt]) :: []) :: optitm3 tl
| IF(origin, cnd :: then_stmt :: else_stmt :: []) :: tl -> IF (origin, cnd :: BGN(None, optitm3 [then_stmt]) :: BGN(None, optitm3 [else_stmt]) :: []) :: optitm3 tl
| (CNST _ | VAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _ | UNRY _) as oth :: tl -> oth :: optitm3 tl
| oth :: tl when !catch_escapes -> optothlst := oth :: !optothlst; failwith "optothlst3"
| hd :: tl -> hd :: optitm3 tl

let lvalidate = function
| ('A'..'Z' | 'a'..'z' | '0'..'9') as ch -> ch
| _ -> '_'

let lcombine = function
| None, None -> None
| None, Some lbl -> Some (String.map lvalidate lbl)
| Some lbl, None -> Some (String.map lvalidate lbl)
| Some lbl, Some lbl' -> Some (String.map lvalidate (lbl^"_"^lbl'))

let rec optitm4 = function
| BGN (lbl, BGN (lbl', tl) :: []) -> optitm4 (BGN(lcombine(lbl, lbl'), tl)) 
| BGN (lbl, tl) -> BGN (lbl, List.map optitm4 tl)
| IF(origin, cnd :: then_else_stmt_lst) -> IF (origin, cnd :: List.map optitm4 then_else_stmt_lst)
| CS (origin, sel :: lst) -> CS (origin, sel :: List.map optitm4 lst)
| CSITM(origin, rw_lst) -> CSITM(origin, List.map optitm4 rw_lst)
| WHL(rw_lst) -> WHL(List.map optitm4 rw_lst)
| FORSTMT(o,kind,cmpop,ix,strt,stop,inc,rw_lst) -> FORSTMT(o,kind,cmpop,ix,strt,stop,inc,List.map optitm4 rw_lst)
| TASK(origin, tsk, nam, rw_lst) -> TASK(origin, tsk, nam, List.map optitm4 rw_lst)
| (ASGN _  | CNST _ | VAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _) as oth -> oth
| oth -> optothlst := oth :: !optothlst; failwith "optothlst4"

let optitm lst =
    let lst' = optitm3 lst in
    optopt := Some (lst,lst');
    let lst'' = List.map optitm4 lst' in
    lst''

let rec catitm (pth:string option) itms = function
| IO(origin, str1, int1, dir, str3, clst) -> itms.io := (str1, (origin, int1, dir, str3, List.map ioconn clst)) :: !(itms.io)
| VAR(origin, str1, int1, "ifaceref") -> itms.ir := (origin, str1, int1) :: !(itms.ir)
| VAR(origin, str1, int1, str2) -> itms.v := (str1, (origin, int1, str2, -1)) :: !(itms.v)
| IVAR(origin, str1, int1, rwlst, int2) -> itms.iv := (str1, (origin, int1, rwlst, int2)) :: !(itms.iv)
| TMPVAR(origin, str1, int1, stmtlst) ->
    List.iter (catitm pth itms) stmtlst;
    if not (List.mem_assoc str1 !(itms.v)) then
        itms.v := (str1, (origin, int1, str1, -1)) :: !(itms.v)
| CA(origin, rght::lft::[]) -> itms.ca := (origin, lft, rght) :: !(itms.ca)
| TYP(id1, int1, int2, []) ->
    if id1 = CNSTDTYP then
        let (t,s,m1,ml) = Hashtbl.find typetable int1 in
        print_endline (string_of_int int1^":"^string_of_int int2^":"^s);
        Hashtbl.replace typetable int1 (t,s,m1,ml@[TYPCONST])
| INST(origin, str1, (str2, port_lst)) ->
    let pth' = match lcombine(pth,Some str1) with Some s -> s | None -> failwith "lcombine" in
    itms.inst := (pth', (origin, str2, port_lst)) :: !(itms.inst)
| ALWYS(origin, SNTRE(SNITM ("POS", [VRF (ck, [])]) :: SNITM ("POS", [VRF (rst, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (origin, POSPOS(ck,rst), optitm rw_lst) :: !(itms.alwys)    
| ALWYS(origin, SNTRE(SNITM ("POS", [VRF (ck, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (origin, POSEDGE(ck), optitm rw_lst) :: !(itms.alwys)
| ALWYS(origin, SNTRE(SNITM ("NEG", [VRF (ck, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (origin, NEGEDGE(ck), optitm rw_lst) :: !(itms.alwys)
| ALWYS(origin, SNTRE(SNITM (("POS"|"NEG") as edg, [VRF (ck, [])]) :: SNITM ("NEG", [VRF (rst, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    let rw_lst' = (function
       | BGN(lbl, (IF(origin, VRF(rst',[]) :: thn :: els :: []) :: [])) :: [] ->
           BGN(lbl, (IF(origin, UNRY(Unot, VRF(rst',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | IF(origin, VRF(rst',[]) :: thn :: els :: []) :: [] ->
           BGN(None, (IF(origin, UNRY(Unot, VRF(rst',[]) :: []) :: els :: thn :: []) :: [])) :: []
       | oth -> posneglst := oth :: !posneglst; oth) rw_lst in
    itms.alwys := (origin, (match edg with "POS" -> POSNEG(ck,rst) | "NEG" -> NEGNEG(ck,rst) | _ -> UNKNOWN), optitm rw_lst') :: !(itms.alwys)
| ALWYS(origin, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (origin, COMB, rw_lst) :: !(itms.alwys)
| INIT (origin, "initial", rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    (match rw_lst with
        | ASGN (false, _, (CNST (cnst, _, [])) :: VRF (id, []) :: []) :: [] ->
             itms.cnst := (id,cnst) :: !(itms.cnst);
             print_endline ("initial found : "^id);
        | _ -> itms.init := (origin, INITIAL, rw_lst) :: !(itms.init));
| INIT (origin, "final", rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.init := (origin, FINAL, rw_lst) :: !(itms.init)
| BGN(pth', rw_lst) ->
    List.iter (catitm (lcombine (pth,pth')) itms) rw_lst
| FNC(origin, nam, idx1, rw_lst) ->
    let itms' = empty_itms () in
    List.iter (catitm pth itms') rw_lst;
    let fn = (origin, idx1, rw_lst, itms') in
    itms.func := (nam, fn) :: !(itms.func);
    Hashtbl.add functable nam fn;
| IF(origin, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.gen := (origin,rw_lst) :: !(itms.gen)
| IMP(origin, str1, rw_lst) ->
    itms.imp := (List.map (function
    | IMRF(_, str1, str2, []) -> (origin,str1, str2)
    | MODPORTFTR (_,str1) -> (origin, str1, str1)
    | oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst") rw_lst) :: !(itms.imp)
| IMRF(origin, str1, str2, []) -> ()
| TASK (origin, "task", str1, rw_lst) ->
    let itms' = empty_itms () in
    List.iter (catitm pth itms') rw_lst;
    itms.task := (origin, str1, rw_lst, itms') :: !(itms.task)
| NTL(rw_lst)
| RNG(rw_lst)
| SNTRE(rw_lst)
| IRNG(_,rw_lst)
| JMPL(_,rw_lst)
| JMPG(_,rw_lst)
| CS(_,rw_lst)
| CSITM(_,rw_lst)
| WHL(rw_lst)
| FORSTMT(_,_,_,_,_,_,_,rw_lst)
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
| ASGN(_, _, rw_lst)
| ARITH(_, rw_lst)
| LOGIC(_, rw_lst)
| CMP(_, rw_lst)
| CAT(_, rw_lst)
| CPS(_, rw_lst)
| CND(_, rw_lst)
| DSPLY(_, _, rw_lst)
| VPLSRGS(_, _, rw_lst)
| REPL(_, _, rw_lst) -> List.iter (catitm pth itms) rw_lst
| FRF(_, nam, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    print_endline ("Func: "^nam);
    if not (List.mem nam !(itms.needed)) then
        itms.needed := nam :: !(itms.needed);
| XRF(origin, str1, str2, str3, dirop) -> ()
| MODUL(origin, str1, str2, rw_lst) ->
    let itms = empty_itms () in
    List.iter (catitm None itms) rw_lst;
    let itms' = rev_itms itms in
    if Hashtbl.mem hierarchy str1 then Hashtbl.add modules str1 (origin, itms', rw_lst)
    else if Hashtbl.mem hierarchy str2 then Hashtbl.add modules str2 (origin, itms', rw_lst)
    else print_endline ("Module "^str1^"/"^str2^" not in cell hierarchy");
| PKG(origin, str1, rw_lst) ->
    let itms = empty_itms () in
    List.iter (catitm (Some str1) itms) rw_lst;
    Hashtbl.add packages str1 (origin, itms)
| IFC(origin, str1, rw_lst) ->
    let itms = empty_itms () in
    List.iter (catitm (Some str1) itms) rw_lst;
    Hashtbl.add interfaces str1 (origin, itms, rw_lst)
| FIL(enc, fil) ->
    Hashtbl.add files enc fil
| CELLS(rw_lst) ->
    top := List.flatten(List.map cell_hier rw_lst)
| TPLSRGS (_, id, tid, []) -> ()
| TYP (dtyp, subtype, idx, lst) -> ()
| oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst"

let iolst delim dir idx io =
    let (widlst,cnst,rng) = findmembers' idx in
    let wid = fold1 ( * ) widlst in
    !delim :: (DIR dir :: SP :: LOGIC ::
	     (if wid > 1 || rng then LBRACK :: num (wid-1) :: COLON :: num 0 :: RBRACK :: [] else []) @ [SP; IDENT io])
	     
let rec fnstmt modul dly stg = function
| IO (origin, io, idx, dir, _, lst) ->
    let lst = iolst (ref (if !stg = FIRSTG then LPAREN else COMMA)) dir idx io in
    stg := IOSTG;
    lst
| VAR (origin, id, idx, _) -> 
    let dlm = match !stg with FIRSTG -> LPAREN::RPAREN::SEMI::[] | IOSTG -> RPAREN::SEMI::[] | VARSTG | JMPSTG | BDYSTG -> [] in
    stg := VARSTG;
    dlm @ varlst modul (ref NL) idx id
| JMPL(origin, rw_lst) ->
    let dlm = match !stg with FIRSTG -> LPAREN::RPAREN::SEMI::[] | IOSTG -> RPAREN::SEMI::[] | VARSTG -> SEMI::[] | JMPSTG | BDYSTG -> [] in
    stg := JMPSTG;
    dlm @ BEGIN None :: (List.flatten (List.map (fnstmt modul dly stg) rw_lst)) @ [SEMI;END]
| JMPG (_,[]) -> []
| itm ->
    let dlm = match !stg with FIRSTG -> LPAREN::RPAREN::SEMI::[] | IOSTG -> RPAREN::SEMI::[] | JMPSTG -> [] | VARSTG | BDYSTG -> SEMI::[] in
    stg := BDYSTG;
    dlm @ cstmt modul dly itm

let rec taskstmt modul dly nam = function
| BGN(_,rw_lst) -> List.flatten (List.map (taskstmt modul dly nam) rw_lst)
| itm -> cstmt modul dly itm @ SEMI :: NL :: []

let outnam f = f^"_translate.v"
let outnamopt f = let l = String.length f in f^(if l < 4 || String.sub f (l-4) 4 <> "_opt" then "_opt" else "")^"_translate.v"
let outtok f = f^"_tokens.txt"
let outtcl f = "./"^f^"_fm.tcl"

let dump f (origin, modul, _) =
  let appendlst = ref [] in
  let append lst = appendlst := lst :: !appendlst in
  if true then print_endline ("f \""^f^"\";; /* "^outnam f^" : "^outtcl f ^" */");
  let head = ref [fsrc origin; MODULE; SP; IDENT f] in
  let delim = ref LPAREN in
  List.iter (fun (io, (origin, idx, dir, kind', lst)) -> 
    head := !head @ iolst delim dir idx io;
    delim := COMMA;
    ) (List.rev !(modul.io));
  head := !head @ [RPAREN;SEMI;NL];
  List.iter (fun (id, (origin, idx, kind', n)) -> append (fsrc origin :: varlst modul (ref SP) idx id @ SEMI :: NL :: []);
                 ) (List.rev !(modul.v));
  List.iter (fun nam ->
    print_endline nam;
    let found = List.mem_assoc nam !(modul.func) in
    let (origin, idx, lst, itms') = if found then
        List.assoc nam !(modul.func)
    else
        Hashtbl.find functable nam in
    let lst = fsrc origin :: NL :: FUNCTION :: SP :: (varlst modul (ref SP) idx nam) @
    List.flatten (List.map (fnstmt modul false (ref FIRSTG)) (List.tl lst)) @ [ENDFUNCTION] in
    append lst
  ) (List.rev !(modul.needed));
  List.iter (fun (origin, nam, lst, itms') ->
		 let lst = List.flatten (List.map (taskstmt modul false nam) lst) in
		 append (fsrc origin :: TASK :: SP :: IDENT nam :: SEMI :: NL :: BEGIN None :: lst @ END :: ENDTASK :: NL :: []);
                 ) (List.rev !(modul.task));
  List.iter (fun (origin, dst, src) ->
                 append (fsrc origin :: ASSIGN :: SP :: expr dst @ (SP :: ASSIGNMENT :: SP:: expr src @ SEMI :: NL :: []));
                 ) (List.rev !(modul.ca));
  List.iter (function
    | (origin, COMB, lst) ->
      append (fsrc origin :: ALWAYS :: AT :: STAR :: flatten1 modul false lst);
    | (origin, POSNEG (ck, rst), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | (origin, NEGNEG (ck, rst), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | (origin, POSEDGE (ck), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 modul true lst);
    | (origin, NEGEDGE (ck), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 modul true lst);
    | (origin, _, lst) -> failwith "not implemented";
    ) (List.rev (List.map (fun (origin,edg,lst) -> (origin, edg, lst)) !(modul.alwys)));
  List.iter (fun (inst, (origin, kind, lst)) ->
                 let delim = ref SP in
                 let lst = List.flatten (List.map (fun term -> let lst = !delim :: portconn term in delim := COMMA; lst) lst) in
                 append (fsrc origin :: IDENT kind :: SP :: IDENT inst :: LPAREN :: lst @ [NL;RPAREN;SEMI]);
                 ) !(modul.inst);
  List.iter (fun (origin, tok, lst) -> append (fsrc origin :: tok :: flatten1 modul false lst);
                 ) !(modul.init);
  !head @ List.flatten (List.sort compare !appendlst) @ [NL;ENDMODULE;NL;NL]

let rec iterate f (modorig, modul, xml) =
    let newitms = copy_itms modul in
    newitms.ir := [];
    newitms.inst := [];
    List.iter (fun (inst, (origin, kind, iolst)) ->
        if Hashtbl.mem interfaces kind then
           begin
           let (_, intf, _) = Hashtbl.find interfaces kind in
           List.iter (fun (nam, (origin, idx, kind, n)) ->
                 let pth = inst^"_"^nam in
                 newitms.v := (pth, (origin, idx, kind, n)) :: !(newitms.v);
                ) !(intf.v);
           end
        else if Hashtbl.mem modules kind then
           begin
           let (kindorig, itms, _) = Hashtbl.find modules kind in
           let newiolst = ref [] in
           let newinnerlst = ref [] in
	   let previolst = !(itms.io) in
           List.iter2 (fun ((_, (origin, ix, idir, typ, ilst)) as inr) -> function
		       | VRF (id, []) ->
                           newiolst := PORT(origin, id, idir, ix, [VRF(id, [])]) :: !newiolst;
                           newinnerlst := inr :: !newinnerlst;
		       | PORT (origin, id_i, Dvif, idx, VRF (id, []) :: []) as pat ->
                           if List.mem_assoc id !(modul.inst) then
			       let (_,inam,_) = List.assoc id !(modul.inst) in
			       if Hashtbl.mem interfaces inam then (match idir with Dinam iport ->
				  begin
				  let (_, intf, imp) = Hashtbl.find interfaces inam in
				  let imp' = List.filter (function IMP _ -> true | _ -> false) imp in 
                                  let imp'' = List.map (function IMP(_, str, lst) -> (str,lst) | _ -> ("",[])) imp' in
                                  if List.mem_assoc iport imp'' then
				  List.iter (function IMRF (origin, nam, dir, []) ->
                                        (* print_endline (inam^":"^iport^":"^nam^":"^id_i); *)
                                        let (_, idx, kind, _) = List.assoc nam !(intf.v) in
					newiolst := PORT(origin, id_i^"_"^nam, dirop dir, idx, [VRF(id^"_"^nam, [])]) :: !newiolst;
				        newinnerlst := (id_i^"_"^nam, (origin, idx, dirop dir, typ, ilst)) :: !newinnerlst;
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
		       | oth -> portothlst := oth :: !portothlst; failwith "portothlst"
		       ) previolst iolst;
           let newiolst = List.rev !newiolst in
           let newinnerlst = List.rev !newinnerlst in
	   let kind_opt = kind^"_opt" in
           if not (Hashtbl.mem modules_opt kind_opt) then
               begin
               let newinneritms = copy_itms itms in
               newinneritms.io := newinnerlst;
               let newhash = (kindorig, newinneritms, xml) in
	       iterate kind newhash
               end;
           newitms.inst := (inst, (origin, kind_opt, newiolst)) :: !(newitms.inst);
           end
        ) !(modul.inst);
    newitms.inst := List.rev (!(newitms.inst));
    Hashtbl.replace modules_opt (f^"_opt") (modorig, newitms, xml);
    print_endline (f^" done")

let dumpform f f' separate = 
    let fd = open_out (outtcl f') in
    let srcpath = try Sys.getenv "XMLSRCPATH" with err -> "." in
    Printf.fprintf fd "#!/opt/synopsys/fm_vO-2018.06-SP3/bin/fm_shell -f\n";
    Printf.fprintf fd "set hdlin_warn_on_mismatch_message \"FMR_ELAB-115 FMR_ELAB-117 FMR_ELAB-146 FMR_ELAB-147 FMR_VLOG-928\"\n";
    Printf.fprintf fd "read_sverilog -container r -libname WORK -12 { \\\n";
    let plst = ref [] in Hashtbl.iter (fun _ (s,_) -> plst := fst (find_source s) :: !plst) packages;
    let iflst = List.map snd (if Hashtbl.mem hierarchy f then Hashtbl.find hierarchy f else []) in
    let hlst = List.sort_uniq compare (List.map (fun k -> let (s, _, _) = if Hashtbl.mem modules k then Hashtbl.find modules k else (k, empty_itms (), []) in fst (find_source s)) (f::iflst)) in
    let slst = !plst @ hlst in
    List.iter (fun src -> if src.[0] == '/' then Printf.fprintf fd "%s \\\n" src else Printf.fprintf fd "%s/%s \\\n" srcpath src) slst;
    Printf.fprintf fd "}\n";
    Printf.fprintf fd "set_top r:/WORK/%s\n" f;
    Printf.fprintf fd "read_sverilog -container i -libname WORK -12 { \\\n";
    let hlst' = List.sort_uniq compare (f' :: (if separate then iflst else [])) in
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

let dumps s = "\""^s^"\""
let dumpi n = string_of_int n
let dumpcnst (w,n) = dumpsized w n

let dumpu = function
| Unknown -> "Uunknown"
| Unot -> "Unot"
| Ulognot -> "Ulognot"
| Unegate -> "Unegate"
| Uextend -> "Uextend"
| Uextends -> "Uextends"

let dumparith = function
| Aadd -> "Aadd"
| Asub -> "Asub"
| Amul -> "Amul"
| Amuls -> "Amuls"
| Aunknown -> "Aunknown"

let dumplog = function
| Lunknown -> "Lunknown"
| Land -> "Land"
| Lredand -> "Lredand"
| Lor -> "Lor"
| Lredor -> "Lredor"
| Lxor -> "Lxor"
| Lxnor -> "Lxnor"
| Lredxor -> "Lredxor"
| Lredxnor -> "Lredxnor"
| Lshiftl -> "Lshiftl"
| Lshiftr -> "Lshiftr"
| Lshiftrs -> "Lshiftrs"

let dumpcmp = function
| Cunknown -> "Cunknown"
| Ceq -> "Ceq"
| Cneq -> "Cneq"
| Cgt -> "Cgt"
| Cgts -> "Cgts"
| Cgte -> "Cgte"
| Cgtes -> "Cgtes"
| Ceqwild -> "Ceqwild"
| Cneqwild -> "Cneqwild"
| Cltes -> "Cltes"
| Clte -> "Clte"
| Clt -> "Clt"
| Clts -> "Clts"

let dumpb b = string_of_bool b
let dumpstrlst lst = "["^String.concat ";\n\t" (List.map dumps lst)^"]"

let rec dumpdir = function
| Dinput -> "Dinput"
| Doutput -> "Doutput"
| Dinout -> "Dinout"
| Dvif -> "Dvif"
| Dinam str -> "Dinam \""^str^"\""
| Dport(str1, int1, dirop, str2, str_lst) ->
     "Dport("^dumps str1 ^", "^ dumpi int1 ^", "^ dumpdir dirop ^", "^ dumps str2 ^", "^ dumpstrlst str_lst^")"
| Dunknown -> "Dunknown"

let rec dumpitm = function
| VRF (id, []) -> "VRF (\""^id^"\", [])"
| UNKNOWN -> "UNKNOWN"
| XML (rw_lst) -> "XML("^dumplst rw_lst^")"
| EITM (str1, str2, str3, int2, rw_lst) -> "EITM("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumpi int2^", "^dumplst rw_lst^")"
| IO (str1, str2, int2, dirop, str3, rw_lst) -> "IO("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumpdir dirop^", "^dumps str3^", "^dumplst rw_lst^")"
| VAR (str1, str2, int2, str3) -> "VAR"^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumps str3^")"
| IVAR (str1, str2, int2, rw_lst, int3) -> "IVAR("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^", "^dumpi int3^")"
| TMPVAR (str1, str2, int2, rw_lst) -> "TMPVAR("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^")"
| CNST ((int, cexp), int2, rw_lst) -> "CNST("^dumpcnst (int, cexp)^", "^dumpi int2^", "^dumplst rw_lst^")"
| VRF (str1, rw_lst) -> "VRF("^dumps str1^", "^dumplst rw_lst^")"
| TYP (typenc, int1, int2, rw_lst) -> "TYP("^dumptyp typenc^", "^dumpi int1^", "^dumpi int2^", "^dumplst rw_lst^")"
| FNC (str1, str2, int2, rw_lst) -> "FNC("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^")"
| TASK (str1, str2, str3, rw_lst) -> "TASK("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumplst rw_lst^")"
| INST (str1, str2, (str3, rw_lst)) -> "INST("^dumps str1^", "^dumps str2^"("^", "^dumps str3^", "^", "^dumplst rw_lst^"))"
| SFMT (str1, rw_lst) -> "SFMT("^dumps str1^", "^dumplst rw_lst^")"
| SYS (str1, str2, rw_lst) -> "SYS("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| TPLSRGS (str1, str2, int2, rw_lst) -> "TPLSRGS("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^")"
| VPLSRGS (str1, int2, rw_lst) -> "VPLSRGS("^dumps str1^", "^dumpi int2^", "^dumplst rw_lst^")"
| PORT (str1, str2, dirop, int2, rw_lst) -> "PORT("^dumps str1^", "^dumps str2^", "^dumpdir dirop^", "^dumpi int2^", "^dumplst rw_lst^")"
| CA (str1, rw_lst) -> "CA("^dumps str1^", "^dumplst rw_lst^")"
| UNRY (unaryop, rw_lst) -> "UNRY("^dumpu unaryop^", "^dumplst rw_lst^")"
| SEL (str1, rw_lst) -> "SEL("^dumps str1^", "^dumplst rw_lst^")"
| ASEL (rw_lst) -> "ASEL("^dumplst rw_lst^")"
| SNITM (str1, rw_lst) -> "SNITM("^dumps str1^", "^dumplst rw_lst^")"
| ASGN (bool, str2, rw_lst) -> "ASGN("^dumpb bool^", "^dumps str2^", "^dumplst rw_lst^")"
| ARITH (arithop, rw_lst) -> "ARITH("^dumparith arithop^", "^dumplst rw_lst^")"
| LOGIC (logop, rw_lst) -> "LOGIC("^dumplog logop^", "^dumplst rw_lst^")"
| CMP (cmpop, rw_lst) -> "CMP("^dumpcmp cmpop^", "^dumplst rw_lst^")"
| FRF (str1, str2, rw_lst) -> "FRF("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| XRF (str1, str2, str3, str4, dirop) -> "XRF("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumps str4^", "^dumpdir dirop^")"
| PKG (str1, str2, rw_lst) -> "PKG("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| CAT (str1, rw_lst) -> "CAT("^dumps str1^", "^dumplst rw_lst^")"
| CPS (str1, rw_lst) -> "CPS("^dumps str1^", "^dumplst rw_lst^")"
| CND (str1, rw_lst) -> "CND("^dumps str1^", "^dumplst rw_lst^")"
| REPL (str1, int2, rw_lst) -> "REPL("^dumps str1^", "^dumpi int2^", "^dumplst rw_lst^")"
| MODUL (str1, str2, str3, rw_lst) -> "MODUL("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumplst rw_lst^")"
| BGN (None, rw_lst) -> "BGN(None,"^dumplst rw_lst^")"
| BGN (Some str1, rw_lst) -> "BGN(Some "^dumps str1^", "^dumplst rw_lst^")"
| RNG (rw_lst) -> "RNG("^dumplst rw_lst^")"
| ALWYS (str1, rw_lst) -> "ALWYS("^dumps str1^", "^dumplst rw_lst^")"
| SNTRE (rw_lst) -> "SNTRE("^dumplst rw_lst^")"
| IF (str1, rw_lst) -> "IF("^dumps str1^", "^dumplst rw_lst^")"
| INIT (str1, str2, rw_lst) -> "INIT("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IRNG (str1, rw_lst) -> "IRNG("^dumps str1^", "^dumplst rw_lst^")"
| IFC (str1, str2, rw_lst) -> "IFC("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IMP (str1, str2, rw_lst) -> "IMP("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IMRF (str1, str2, str3, rw_lst) -> "IMRF("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumplst rw_lst^")"
| JMPL (str1, rw_lst) -> "JMPL("^dumps str1^", "^dumplst rw_lst^")"
| JMPG (str1, rw_lst) -> "JMPG("^dumps str1^", "^dumplst rw_lst^")"
| CS (str1, rw_lst) -> "CS("^dumps str1^", "^dumplst rw_lst^")"
| CSITM (str1, rw_lst) -> "CSITM("^dumps str1^", "^dumplst rw_lst^")"
| WHL (rw_lst) -> "WHL("^dumplst rw_lst^")"
| FORSTMT (str1, str2, cmpop, str3, (int1, cexp1), (int2, cexp2), (int3, cexp3), rw_lst) ->
    "FORSTMT("^dumps str1^", "^dumps str2^", "^dumpcmp cmpop^", "^dumps str3^", "^dumpcnst (int1, cexp1)^", "^dumpcnst (int2, cexp2)^", "^dumpcnst (int3, cexp3)^", "^dumplst rw_lst^")"
| ARG (rw_lst) -> "ARG("^dumplst rw_lst^")"
| DSPLY (str1, str2, rw_lst) -> "DSPLY("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| FILS (str1, rw_lst) -> "FILS("^dumps str1^", "^dumplst rw_lst^")"
| FIL (str1, str2) -> "FIL"^dumps str1^", "^dumps str2^")"
| NTL (rw_lst) -> "NTL("^dumplst rw_lst^")"
| CELLS (rw_lst) -> "CELLS("^dumplst rw_lst^")"
| CELL (str1, str2, str3, str4, rw_lst) -> "CELL("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumps str4^", "^dumplst rw_lst^")" 
| POSPOS (str1, str2) -> "POSPOS"^dumps str1^", "^dumps str2^")"
| POSNEG (str1, str2) -> "POSNEG"^dumps str1^", "^dumps str2^")"
| NEGNEG (str1, str2) -> "NEGNEG"^dumps str1^", "^dumps str2^")"
| POSEDGE (str1) -> "POSEDGE"^dumps str1^")"
| NEGEDGE (str1) -> "NEGEDGE"^dumps str1^")"
| COMB -> "COMB"
| MODPORTFTR (str1, str2) -> "MODPORTFTR("^dumps str1^", "^dumps str2^")"
 
and dumplst lst = "["^String.concat ";\n\t" (List.map dumpitm lst)^"]"
and dumpcstlst lst = "["^String.concat ";\n\t" (List.map dumpcnst lst)^"]"

and dumpitms fd modul =
  Printf.fprintf fd "  {io =\n";
  Printf.fprintf fd "  {contents =\n    [";
  List.iter (fun (a,(b,c,d,e,lst)) ->
    Printf.fprintf fd "(%s, (%s, %d, %s, %s, %s));\n" (dumps a) (dumps b) c (dumpdir d) (dumps e) (dumpcstlst lst)) !(modul.io);
  Printf.fprintf fd "     ]};\n";
  Printf.fprintf fd " v = {contents = [";
  List.iter (fun (a,(b,c,d,e)) -> Printf.fprintf fd "(%s, (%s, %d, %s, %d));\n" (dumps a) (dumps b) c d e) !(modul.v);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " iv = {contents = [";
  List.iter (fun (a,(b,c,d,e)) -> Printf.fprintf fd "(%s, (%s, %d, %s, %d));\n" (dumps a) b c (dumplst d) e) !(modul.iv);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " ir = {contents = [";
  List.iter (fun (a,b,c) -> Printf.fprintf fd "(%s, %s, %s)\n" a (dumps b) (dumpi c)) !(modul.ir);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " ca = {contents = [";
  List.iter (fun (a,b,c) -> Printf.fprintf fd "(%s, %s, %s)\n" (dumps a) (dumpitm b) (dumpitm c)) !(modul.ca);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " typ = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " alwys = {contents = [";
  List.iter (fun (a,b,lst) -> Printf.fprintf fd "(%s, %s, %s)\n" (dumps a) (dumpitm b) (dumplst lst)) !(modul.alwys);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " init = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " func = {contents = [";
  List.iter (fun (a,(b,c,d,itms)) -> Printf.fprintf fd "    (%s,\n" (dumps a);
  Printf.fprintf fd "     (%s, %d, %s\n" (dumps b) c (dumplst d);
  dumpitms fd itms;
  Printf.fprintf fd "         ));\n" ) !(modul.func);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " task = {contents = [";
  List.iter (fun (b,c,d,itms) ->
  Printf.fprintf fd "     (%s, %s, %s\n" (dumps b) (dumps c) (dumplst d);
  dumpitms fd itms;
  Printf.fprintf fd "         ));\n" ) !(modul.task);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " gen = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " imp = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " inst =\n";
  Printf.fprintf fd "  {contents =\n    [";
  List.iter (fun (a,(b,c,lst)) -> Printf.fprintf fd "    (%s,\n" (dumps a);
  Printf.fprintf fd "     (%s, %s,\n" (dumps b) (dumps c);
  Printf.fprintf fd "       %s" (dumplst lst);
  Printf.fprintf fd "         ));\n" ) !(modul.inst);
  Printf.fprintf fd " cnst = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " needed = {contents = ";
  let delim = ref '[' in List.iter (fun (a) -> Printf.fprintf fd "%c%s" !delim (dumps a); delim := ',') !(modul.needed);
  Printf.fprintf fd "]}}\n";
  Printf.fprintf fd "  \n"
    
let rec debug f (origin, modul, xml) =
  let fd = open_out (f^".debug") in
  dumpitms fd modul;
  close_out fd

let translate errlst xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    let (line,range) = match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos) | None -> (0, (0,0)) in
    let rwxml = rw' {anchor="a1";errlst=errlst;names=ref []} xml in
    catitm None (empty_itms ()) rwxml;
    let top = snd(List.hd !top) in
    print_endline ("toplevel is "^top);
    let tophash = Hashtbl.find modules top in
    iterate top tophash;
    let top_opt = top^"_opt" in
    let separate = try int_of_string (Sys.getenv "VXML_SEPARATE") > 0 with _ -> true in
    dumpform top top_opt separate;
    let mods = ref [] in
    let debugtree = try int_of_string (Sys.getenv "VXML_DEBUGTREE") > 0 with _ -> false in
    if debugtree then
        begin
        Hashtbl.iter (fun k x -> debug k x) modules;
        Hashtbl.iter (fun k x -> debug k x) modules_opt;
        end;
    Hashtbl.iter (fun k x -> let d = reformat0 (dump k x) in mods := (k, reformat2 (reformat1 d)) :: !mods) modules_opt;
    let mods = List.sort compare !mods in
    let indent = ref 0 in
    if separate then
        begin
        List.iter (fun (k,lst) ->
        let fd = open_out (outnam k) in
        List.iter (tokenout fd indent) lst;
        close_out fd) mods
        end
    else
        begin
        let fd = open_out (outnam top_opt) in
(*
        output_string fd "`default_nettype none\n";
*)
        List.iter (fun (_,lst) ->
            List.iter (tokenout fd indent) lst;
        ) mods;
        close_out fd;
        end;
    (line,range,rwxml,xml,mods)
