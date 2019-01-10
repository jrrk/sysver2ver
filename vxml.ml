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
| Dvif of string ref
| Dinam of string ref
| Dport of (string * int * dirop * string * string list)

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

type typmap =
| TYPNONE
| SUBTYP of int
| TYPRNG of int*int
| TYPMEMBER of typetable_t
| TYPENUM of string * int * (int*int)
| TYPDEF
| RECTYP of typetable_t

and typetable_t = typenc*string ref*typmap*typmap list
and typ_t = typenc*string ref*typmap*rw list
and rw =
| UNKNOWN
| XML of rw list
| EITM of string * string * string * int * rw list
| IO of string * string list * typetable_t * dirop * string * rw list
| VAR of string * string list * typetable_t * string
| IVAR of string * string * int * rw list * int
| TMPVAR of string * string * typetable_t * rw list
| CNST of (int * cexp) * int * rw list
| VRF of string * rw list
| TYP of int * typ_t
| FNC of string * string * typetable_t * rw list
| TASK of string * string * rw list
| TASKRF of string * string * rw list
| INST of string * string list * (string * rw list)
| SFMT of string * rw list
| SYS of string * string * rw list
| TPLSRGS of string * string * int * rw list
| VPLSRGS of string * int * rw list
| PORT of string * string * dirop * rw list
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
| IFC of string * string * string * rw list
| IMP of string * string * rw list
| IMRF of string * string * dirop * rw list
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
| TYPETABLE of typetable_t array

type token =
| INVALID
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
| POW
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
| INTERFACE
| ENDINTERFACE
| MODPORT

type xmlattr = {
    anchor: string;
    errlst: Xml.xml list ref;
    names: (string*typetable_t) list ref;
    typetable: typetable_t array;
    intf: (string*string) list ref;
    instances: (string*token) list;
    modulexml: (string*(rw list*(string*typetable_t) list)) list ref;
    }
    
type itms = { 
  io: (string*(string*typetable_t*dirop*string*(int*cexp) list)) list ref;
  v: (string*(string*typetable_t*string*typetable_t)) list ref;
  iv: (string*(string*int*rw list*int)) list ref;
  ir: (string*string*typetable_t) list ref;
  ca: (string*rw*rw) list ref;
  typ: (string*int*int) list ref;
  alwys: (string*rw*rw list) list ref;
  init: (string*token*rw list) list ref;
  func: (string*(string*typetable_t*rw list*itms)) list ref;
  task: (string*(string*rw list*itms)) list ref;
  gen: (string*rw list) list ref;
  imp: (string*(string*(string*dirop) list)) list ref;
  inst: (string*(string*string*rw list)) list ref;
  cnst: (string*(bool*(int*cexp))) list ref;
  needed: (token*string) list ref;
  avoid_dollar_unsigned: bool;
  remove_interfaces: bool;
}

let modules = Hashtbl.create 255
let modules_opt = Hashtbl.create 255
let packages = Hashtbl.create 255
let interfaces = Hashtbl.create 255
let files = Hashtbl.create 255
let hierarchy = Hashtbl.create 255
let functable = Hashtbl.create 255
let tasktable = Hashtbl.create 255
let modtokens = Hashtbl.create 255

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
let rngopt = ref None
let typopt = ref None
let decopt = ref None
let portopt = ref None
let cellopt = ref None
let instopt = ref None
let forlst = ref []
let ternlst = ref []
let ternothlst = ref []
let optitmlst = ref []
let widthlst = ref []

let matchcnt = ref 0
let top = ref []
let empty_attr errlst = {anchor="a1";errlst=errlst;names=ref [];intf=ref [];instances=[];typetable=[||];modulexml=ref []}

let topattr = ref (empty_attr (ref []))

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

let dumps s = "\""^s^"\""

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
| IFCRFDTYP str -> "IFCRFDTYP "^dumps str
| TYPDF str -> "TYPDF "^dumps str

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

let expandbraket lo hi fn = Array.to_list (Array.init (hi-lo+1) (fun ix -> fn ("__BRA__"^string_of_int (ix+lo)^"__KET__")))
       
let dbg i ch h = if false then print_endline (string_of_int i^":"^String.make 1 ch^":"^string_of_int h)

let hex_to_bigint s = let rslt = ref Big_int.zero_big_int in String.iter (function
| '0'..'9' as ch -> rslt := Big_int.add_int_big_int (int_of_char ch - int_of_char '0') (Big_int.mult_int_big_int 16 !rslt)
| 'a'..'f' as ch -> rslt := Big_int.add_int_big_int (int_of_char ch - int_of_char 'a' + 10) (Big_int.mult_int_big_int 16 !rslt)
| ch -> failwith (String.make 1 ch)) s; !rslt

let rec hex_of_bigint w n =
let (q,r) = Big_int.quomod_big_int n (Big_int.big_int_of_int 16) in
(if (w > 4 && Big_int.sign_big_int q > 0) then hex_of_bigint (w-4) q else "")^String.make 1 ("0123456789abcdef".[Big_int.int_of_big_int r])

let hex_to_ascii len str =
  let bytes = String.length str in
  if len mod 8 = 0 && len >= bytes*4 then
    begin
    let h = ref 0 and str' = Bytes.make (bytes/2) ' ' in
    let mychar_of_int x = if x >= 32 && x <= 127 && (len > 8 || x >= int_of_char 'a') then char_of_int x else failwith "ascii" in
    String.iteri (fun ix -> function
      | '0'..'9' as ch -> dbg ix ch !h;
          h := !h * 16 + (int_of_char ch - int_of_char '0');
          if ix land 1 = 1 then begin Bytes.set str' (ix/2) (mychar_of_int !h); h := 0; dbg ix ch !h;end
      | 'a'..'f' as ch -> dbg ix ch !h;
          h := !h * 16 + (int_of_char ch - int_of_char 'a' + 10);
          if ix land 1 = 1 then begin Bytes.set str' (ix/2) (mychar_of_int !h); h := 0; dbg ix ch !h; end
      | _ -> h := -1) str;
    str'
    end
  else invalid_arg str

let decode len str =
  decopt := Some (len,str);
  try STRING (Bytes.to_string (hex_to_ascii len str))
  with err -> let num = hex_to_bigint str in try HEX(Big_int.int_of_big_int num) with err -> BIGINT num

let cexp exp = match exp.[0] with
| '"' -> let n = String.length exp - 2 in let s = String.sub exp 1 n in (n, STRING s)
| _ ->
    try Scanf.sscanf exp "%d'h%x" (fun b n -> (b, HEX n)) with err ->
    try Scanf.sscanf exp "%d'h%s" (fun b s -> (b, decode b s)) with err ->
    try Scanf.sscanf exp "%d'sh%x" (fun b n -> (b, SHEX n)) with err ->
    try Scanf.sscanf exp "%d'bx" (fun b -> (b, BIN 'x')) with err ->
    try Scanf.sscanf exp "%f" (fun f -> (64, FLT f)) with err -> (-1,ERR exp)

let rec typmap = function
| [] -> TYPNONE
| [("left", lft); ("right", rght)] -> TYPRNG(int_of_string lft, int_of_string rght)
| oth -> mapothlst := oth :: !mapothlst; failwith "mapothlst"

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
| VAR (_, [ix''], _, ("int"|"integer"|"logic" as kind)) :: ASGN(dly, _, CNST (strt, _, []) :: VRF (ix, []) :: []) ::
        JMPL (_, 
           (WHL
            (CMP (cmpop, CNST (stop, _, []) :: VRF (ix', []) :: []) ::
             stmtlst)) :: []) :: [] when (ix=ix') && (ix'=ix'') && fortailmatch ix (List.rev stmtlst) ->
               let (inc,stmts) = forinc (List.rev stmtlst) in FORSTMT (origin,kind,cmpop,ix,strt,stop,inc,stmts)
| VAR (_, [ix''], _, ("int"|"integer"|"logic" as kind)) :: ASGN(dly,_, CNST (strt, _, []) :: VRF (ix, []) :: []) ::
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

let rec cell_hier = function
| CELL (_, nam, subnam, hier, rw_lst) ->
   let hier_lst = List.flatten (List.map cell_hier rw_lst) in
   if false then print_endline ("Cell: "^subnam);
   Hashtbl.replace hierarchy subnam hier_lst;
   (nam,subnam) :: hier_lst
| oth -> cellothlst := oth :: !cellothlst; failwith "cellothlst"

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
        let tmp = VRF (t, []) in
        let smpl = simplify_exp anchor tmpl oth in
        tmpl := !tmpl @ TMPVAR(anchor, t, (BASDTYP, ref "logic", TYPRNG(idx'-1,0), []), []) :: ASGN(false, orig, smpl :: tmp :: []) :: [];
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

let dumpsized w = function
| BIN b -> string_of_int w^"'b"^String.make w b
| HEX n -> Printf.sprintf "%d'h%x" w n
| SHEX n -> Printf.sprintf "%d'sh%x" w n
| BIGINT n -> Printf.sprintf "%d'h%s" w (hex_of_bigint w n)
| STRING s -> "\""^String.escaped s^"\""
| FLT f -> string_of_float f
| ERR err -> ("NumberError:"^err)

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

let rec dumpmap = function
| TYPNONE -> "TYPNONE"
| SUBTYP int1 -> "SUBTYP "^string_of_int int1
| TYPRNG(int1,int2) -> "TYPRNG("^string_of_int int1^", "^string_of_int int2^")"
| TYPMEMBER (tab) -> "TYPMEMBER"^dumptab tab
| TYPENUM(str1, int1, (int2,int3)) -> "TYPENUM("^dumps str1^", "^string_of_int int1^", ("^string_of_int int2^", "^string_of_int int3^"))"
| TYPDEF -> "TYPDEF"
| RECTYP tab -> "RECTYP"^dumptab tab

and dumptab (typenc, str1, map, typmaplst) = "("^dumptyp typenc^", "^dumps !str1^", "^dumpmap map^", "^dumpmlst typmaplst^")"
and dumpmlst lst = "["^String.concat ";\n\t" (List.map dumpmap lst)^"]"
let dumptablst lst = "["^String.concat ";\n\t" (List.map dumptab lst)^"]"
let dumpb b = string_of_bool b
let dumpstrlst lst = "["^String.concat ";\n\t" (List.map dumps lst)^"]"

let rec dumpdir = function
| Dinput -> "Dinput"
| Doutput -> "Doutput"
| Dinout -> "Dinout"
| Dvif s -> "Dvif "^dumps !s
| Dinam str -> "Dinam "^dumps !str
| Dport(str1, int1, dirop, str2, str_lst) ->
     "Dport("^dumps str1 ^", "^ dumpi int1 ^", "^ dumpdir dirop ^", "^ dumps str2 ^", "^ dumpstrlst str_lst^")"
| Dunknown -> "Dunknown"

let oldsrc = ref ("",-1)

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

let diropv = function
| Dinput -> "input"
| Doutput -> "output"
| Dinout -> "inout"
| Dvif _ -> "vif"
| Dinam str -> !str
| Dport _ -> "ifport"
| Dunknown -> "inout"

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
| POW -> output_string fd "**"
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
| INTERFACE -> output_string fd "interface"; incr indent
| ENDINTERFACE -> output_string fd "endinterface"; decr indent
| MODPORT -> output_string fd "modport"
| INVALID -> failwith "invalid token"

let rec dumpitm = function
| VRF (id, []) -> "VRF (\""^id^"\", [])"
| UNKNOWN -> "UNKNOWN"
| XML (rw_lst) -> "XML("^dumplst rw_lst^")"
| EITM (str1, str2, str3, int2, rw_lst) -> "EITM("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumpi int2^", "^dumplst rw_lst^")"
| IO (str1, str2lst, typ2, dirop, str3, rw_lst) -> "IO("^dumps str1^", "^dumpstrlst str2lst^", "^dumptab typ2^", "^dumpdir dirop^", "^dumps str3^", "^dumplst rw_lst^")"
| VAR (str1, str2lst, typ2, str3) -> "VAR"^dumps str1^", "^dumpstrlst str2lst^", "^dumptab typ2^", "^dumps str3^")"
| IVAR (str1, str2, int2, rw_lst, int3) -> "IVAR("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^", "^dumpi int3^")"
| TMPVAR (str1, str2, typ2, rw_lst) -> "TMPVAR("^dumps str1^", "^dumps str2^", "^dumptab typ2^", "^dumplst rw_lst^")"
| CNST ((int, cexp), int2, rw_lst) -> "CNST("^dumpcnst (int, cexp)^", "^dumpi int2^", "^dumplst rw_lst^")"
| VRF (str1, rw_lst) -> "VRF("^dumps str1^", "^dumplst rw_lst^")"
| TYP (idx, (typenc, str1, typmap, typ_lst)) -> "TYP("^dumptyp typenc^", "^dumps !str1^", "^dumpmap typmap^", "^dumplst typ_lst^")"
| FNC (str1, str2, typ2, rw_lst) -> "FNC("^dumps str1^", "^dumps str2^", "^dumptab typ2^", "^dumplst rw_lst^")"
| TASK (str1, str2, rw_lst) -> "TASK("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| TASKRF (str1, str2, rw_lst) -> "TASKRF("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| INST (str1, str2lst, (str3, rw_lst)) -> "INST("^dumps str1^", "^dumpstrlst str2lst^"("^", "^dumps str3^", "^", "^dumplst rw_lst^"))"
| SFMT (str1, rw_lst) -> "SFMT("^dumps str1^", "^dumplst rw_lst^")"
| SYS (str1, str2, rw_lst) -> "SYS("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| TPLSRGS (str1, str2, int2, rw_lst) -> "TPLSRGS("^dumps str1^", "^dumps str2^", "^dumpi int2^", "^dumplst rw_lst^")"
| VPLSRGS (str1, int2, rw_lst) -> "VPLSRGS("^dumps str1^", "^dumpi int2^", "^dumplst rw_lst^")"
| PORT (str1, str2, dirop, rw_lst) -> "PORT("^dumps str1^", "^dumps str2^", "^dumpdir dirop^", "^dumplst rw_lst^")"
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
| IFC (str1, str2, str3, rw_lst) -> "IFC("^dumps str1^", "^dumps str2^", "^dumps str3^", "^dumplst rw_lst^")"
| IMP (str1, str2, rw_lst) -> "IMP("^dumps str1^", "^dumps str2^", "^dumplst rw_lst^")"
| IMRF (str1, str2, dir, rw_lst) -> "IMRF("^dumps str1^", "^dumps str2^", "^dumpdir dir^", "^dumplst rw_lst^")"
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
| TYPETABLE arr -> "[|"^String.concat ";\n\t" (Array.to_list (Array.mapi (fun idx (typenc, str1, typmap, typ_lst) -> string_of_int idx^": TYP("^dumptyp typenc^", "^dumps !str1^", "^dumpmap typmap^", "^dumpmlst typ_lst^")") arr))^"|]"
 
and dumplst lst = "["^String.concat ";\n\t" (List.map dumpitm lst)^"]"
and dumpcstlst lst = "["^String.concat ";\n\t" (List.map dumpcnst lst)^"]"

and dumpitms fd modul =
  Printf.fprintf fd "  {io =\n";
  Printf.fprintf fd "  {contents =\n    [";
  List.iter (fun (a,(b,c,d,e,lst)) ->
    Printf.fprintf fd "(%s, (%s, %s, %s, %s, %s));\n" (dumps a) (dumps b) (dumptab c) (dumpdir d) (dumps e) (dumpcstlst lst)) !(modul.io);
  Printf.fprintf fd "     ]};\n";
  Printf.fprintf fd " v = {contents = [";
  List.iter (fun (a,(b,c,d,e)) -> Printf.fprintf fd "(%s, (%s, %s, %s, %s));\n" (dumps a) (dumps b) (dumptab c) d (dumptab e)) !(modul.v);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " iv = {contents = [";
  List.iter (fun (a,(b,c,d,e)) -> Printf.fprintf fd "(%s, (%s, %d, %s, %d));\n" (dumps a) b c (dumplst d) e) !(modul.iv);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " ir = {contents = [";
  List.iter (fun (a,b,c) -> Printf.fprintf fd "(%s, %s, %s)\n" a (dumps b) (dumptab c)) !(modul.ir);
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
  List.iter (fun (a,b,lst) ->
      Printf.fprintf fd "(%s, " (dumps a);
      tokenout fd (ref 0) b;
      Printf.fprintf fd ", %s)\n" (dumplst lst)) !(modul.init);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " func = {contents = [";
  List.iter (fun (a,(b,c,d,itms)) -> Printf.fprintf fd "    (%s,\n" (dumps a);
  Printf.fprintf fd "     (%s, %s, %s\n" (dumps b) (dumptab c) (dumplst d);
  dumpitms fd itms;
  Printf.fprintf fd "         ));\n" ) !(modul.func);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " task = {contents = [";
  List.iter (fun (b,(c,d,itms)) ->
  Printf.fprintf fd "     (%s, %s, %s\n" (dumps b) (dumps c) (dumplst d);
  dumpitms fd itms;
  Printf.fprintf fd "         ));\n" ) !(modul.task);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " gen = {contents = [";
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " imp = {contents = [";
  List.iter (fun (a,(b,lst)) ->
    Printf.fprintf fd "     (%s,%s, " (dumps a)  (dumps b);
    List.iter (fun (c,d) -> Printf.fprintf fd "     (%s, %s)\n" (dumps c) (dumpdir d)) lst;
  Printf.fprintf fd "         ));\n" ) !(modul.imp);
  Printf.fprintf fd "]};\n";
  Printf.fprintf fd " inst =\n";
  Printf.fprintf fd "  {contents =\n    [";
  List.iter (fun (a,(b,c,lst)) -> Printf.fprintf fd "    (%s,\n" (dumps a);
  Printf.fprintf fd "     (%s, %s,\n" (dumps b) (dumps c);
  Printf.fprintf fd "       %s" (dumplst lst);
  Printf.fprintf fd "         ));\n" ) !(modul.inst);
  Printf.fprintf fd " cnst = {contents = [";
  Printf.fprintf fd "]};\n";
  if [] <> !(modul.needed) then
      begin
      Printf.fprintf fd " needed = {contents = ";
      let delim = ref '[' in
      List.iter (fun (a,b) ->
                     Printf.fprintf fd "%c(" !delim;
                     tokenout fd (ref 0) a;
                     Printf.fprintf fd ",%s)" (dumps b);
                     delim := ',') !(modul.needed);
                     Printf.fprintf fd "]};\n";
      end;
  Printf.fprintf fd "}\n";
  Printf.fprintf fd "  \n"

let rec cell_traverse attr (nam, subnam) =
    if List.mem_assoc subnam !(attr.modulexml) then
            let (xlst', names') = List.assoc subnam !(attr.modulexml) in
                begin
                print_endline ("Cell traverse: "^subnam);
                List.iter (function
                    | IO _ -> ()
                    | VAR (_, inflst, typ', "ifaceref") -> List.iter (fun itm -> print_endline ("Ifaceref: "^itm^":"^dumptab typ')) inflst
                    | VAR _ -> ()
                    | IVAR _ -> ()
                    | TYP _ -> ()
                    | FNC _ -> ()
                    | TASK _ -> ()
                    | CA _ -> ()
                    | BGN _ -> ()
                    | INIT _ -> ()
                    | ALWYS _ -> ()
                    | IMP _ -> ()
                    | INST (_, _, (kind, portlst)) ->
                         print_endline ("Searching: "^kind);
                         let (xlst''', names'') = List.assoc kind !(attr.modulexml) in
                         List.iter (function
                        | PORT (_, formal, Dvif bus, [VRF (actual, [])]) ->
			if List.mem_assoc formal names'' then
			    begin
			    let (typenc1,str1,typmap1,typmaplst1) as formtyp = List.assoc formal names'' in
			    if List.mem_assoc actual names' then
			        begin
				let (typenc2,str2,typmap2,typmaplst2) as actualtyp = List.assoc actual names' in
                                if !str2 = "" then str2 := !bus;
			        if !str1 <> !str2 then
				    begin
				    print_endline ("formal: "^formal^", actual: "^actual);
				    print_endline ("formaltype: "^dumptab formtyp^", actualtype: "^dumptab actualtyp);
				    str1 := !str2;
				    end;
				end
			    else
				begin
				print_endline ("Actual "^actual^" not found");
				List.iter (fun (k,_) -> print_endline ("List: "^k)) names'
				end
			    end    
			else
			    begin
			    print_endline ("Formal "^formal^" not found");
			    List.iter (fun (k,_) -> print_endline ("List: "^k)) names''
			    end
                        | _ -> ()) portlst
                    | oth -> cellopt := Some oth; failwith ("cellopt: "^dumpitm oth)) xlst'
                end
        else print_endline (subnam^":missing")

let namedcnt = ref 0

let rec rw' attr = function
| Xml.Element ("verilator_xml", [], xlst) ->
    let decl,hier = List.partition (function Xml.Element (("files"|"module_files"|"netlist"), _, _) -> true | _ -> false) xlst in
    XML (List.map (rw' attr) (decl@hier))
| Xml.Element ("files"|"module_files" as fils, [], xlst) -> FILS (fils, List.map (rw' attr) xlst)
| Xml.Element ("file", [("id", encoding); ("filename", nam); ("language", "1800-2017")], []) -> FIL (encoding, nam)
| Xml.Element ("netlist", [], xlst) ->
    let rlst = List.rev xlst in
    let tlst = List.tl rlst in
    let typtable = match rw' attr (List.hd rlst) with TYPETABLE typarr -> typarr | _ -> failwith "netlist" in
    let instances = List.map (function
        | Xml.Element (kw, ("fl", _) :: ("name", kind) :: _, _) -> (kind, match kw with
            | "module" -> MODULE
            | "iface" -> INTERFACE
            | _ -> INVALID)
        | oth -> instopt := Some oth; failwith "instopt") tlst in
    let attr' = {attr with typetable=typtable;instances=instances} in
    NTL (List.map (rw' attr') tlst)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("dir", dir); ("vartype", typ); ("origName", nam')], xlst) ->
    let typ' = attr.typetable.(int_of_string tid) in
    attr.names := (nam, typ') :: !(attr.names);
    IO (origin, [nam], typ', dirop dir, typ, List.map (rw' attr) xlst)
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", ("ifaceref" as typ)); ("origName", nam')], []) ->
    let (vif, sub) = chkvif nam in
    let nam' = if vif then sub else nam in
    let rslt = match attr.typetable.(int_of_string tid) with
               | (UNPACKADTYP, _, RECTYP attr', [TYPRNG (hi, lo)]) ->
                   (match attr' with
                       | (IFCRFDTYP _, dir, TYPNONE, []) as typ' ->
                           attr.names := expandbraket lo hi (fun istr ->
                               let nam' = nam'^istr in print_endline ("@"^nam'); (nam', typ')) @ !(attr.names);
                           let exp' = expandbraket lo hi (fun istr -> nam'^istr) in
                           if vif then VAR (origin, exp', attr', typ) else IO (origin, exp', attr', Dinam dir, "logic", [])
                       | oth -> typopt := Some oth; failwith "typopt;;582")
               | (IFCRFDTYP _, dir, TYPNONE, []) as typ' ->
                   print_endline ("@"^nam');
                   if not (List.mem_assoc nam' !(attr.names)) then
                       attr.names := (nam', typ') :: !(attr.names)
                   else
                       print_endline (dumptab typ'^":"^dumptab (List.assoc nam' !(attr.names)));
                   if vif then VAR (origin, [sub], typ', typ) else IO (origin, [nam], typ', Dinam dir, "logic", [])
               | oth -> typopt := Some oth; failwith "typopt;;587" in rslt
| Xml.Element ("var", [("fl", origin); ("name", nam); ("dtype_id", tid); ("vartype", typ); ("origName", nam')], []) ->
               let pat = "__Vconcswap" in
               let l = String.length nam and l' = String.length pat in
               let anchor = if l > l' && String.sub nam 0 l' = pat then attr.anchor else origin in
               VAR (anchor, [nam], attr.typetable.(int_of_string tid), typ)
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
               let instkind = List.assoc dnam attr.instances in
	       let rnglst, attrlst = List.partition (function RNG _ -> true | _ -> false) (List.map (rw' attr) xlst) in
               let exportlst = ref [] in
               List.iter (function
                   | PORT (orig, nam, Dvif _, [VRF (nam', [])]) as port when List.mem_assoc nam' !(attr.names) -> 
                       (match List.assoc nam' !(attr.names) with
                           | (IFCRFDTYP _, dir, TYPRNG(hi,lo), []) ->
                               exportlst := expandbraket lo hi (fun istr -> PORT (orig, nam^istr, Dvif dir, [VRF (nam'^istr, [])])) @ !exportlst
                           | (IFCRFDTYP _, dir, TYPNONE, []) -> exportlst := port :: !exportlst
                           | oth -> typopt := Some oth; failwith "typopt;;599")
                   | PORT (orig, nam, Dvif _, [ASEL (VRF (nam', []) :: CNST((_,HEX idx), _, []) :: [])]) when List.mem_assoc nam' !(attr.names) -> 
                       (match List.assoc nam' !(attr.names) with
                           | (IFCRFDTYP _, dir, TYPRNG(hi,lo), []) ->
                               exportlst := expandbraket idx idx (fun istr -> PORT (orig, nam, Dvif dir, [VRF (nam'^istr, [])])) @ !exportlst
                           | (IFCRFDTYP _, dir, TYPNONE, []) -> failwith ("indexing a scalar interface: "^nam)
                           | oth -> typopt := Some oth; failwith "typopt;;599")
                   | PORT (orig, nam, Dvif _, [VRF (nam', [])]) ->
                       print_endline ("vif interface "^nam'^" not found: ["^String.concat ";" (List.map (fun (k,_) -> k) !(attr.names))^"] ?")
                   | PORT (orig, nam, dir, connlst) as port -> exportlst := port :: !exportlst
                   | oth -> portopt := Some oth; failwith "portopt") attrlst;
               let attrlst = List.rev !exportlst in
               let inst = match (instkind,rnglst) with
                   | (INTERFACE, RNG (CNST ((_, (HEX hi|SHEX hi)), _, []) :: CNST ((_, (HEX lo|SHEX lo)), _, []) :: []) :: []) ->
                       begin
                       print_endline ("@"^nam^"["^string_of_int hi^":"^string_of_int lo^"]");
                       attr.names := (nam, (IFCRFDTYP nam, ref dnam, TYPRNG(hi,lo), [])) :: !(attr.names);
                       INST (origin, expandbraket lo hi (fun istr -> nam^istr), (dnam, attrlst))
                       end
                   | (INTERFACE, []) -> attr.names := (nam, (IFCRFDTYP nam, ref dnam, TYPNONE, [])) :: !(attr.names);
		           INST (origin, nam :: [], (dnam, attrlst))
                   | (MODULE, []) -> INST (origin, nam :: [], (dnam, attrlst))
                   | oth -> rngopt := Some oth; failwith "rngopt" in
               inst
| Xml.Element ("range", [("fl", _)], xlst) -> RNG (List.map (rw' attr) xlst)
| Xml.Element ("port", [("fl", origin); ("name", nam); ("direction", dir); ("portIndex", idx)], xlst) ->
               PORT (origin, nam, dirop dir, List.map (rw' attr) xlst)
| Xml.Element ("port", [("fl", origin); ("name", nam); ("portIndex", idx)], xlst) -> let (vif,sub) = chkvif nam in
               PORT (origin, sub, Dvif (ref sub), List.map (rw' attr) xlst)
| Xml.Element ("sel", [("fl", origin); ("dtype_id", tid)], xlst) -> SEL (origin, List.map (rw' attr) xlst)
| Xml.Element ("arraysel", [("fl", origin); ("dtype_id", tid)], xlst) -> ASEL (List.map (rw' attr) xlst)
| Xml.Element ("always", [("fl", origin)], xlst) -> ALWYS (origin, List.map (rw' {attr with anchor=origin}) xlst)
| Xml.Element ("sentree", [("fl", origin)], xlst) -> SNTRE (List.map (rw' attr) xlst)
| Xml.Element ("senitem", [("fl", origin); ("edgeType", etyp)], xlst) -> SNITM (etyp, List.map (rw' attr) xlst)
| Xml.Element ("begin", [("fl", origin); ("name", namedblk)], xlst) ->
    incr namedcnt;
    while_opt origin (Some (namedblk^"_"^string_of_int !namedcnt)) (List.map (rw' attr) xlst)
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
| Xml.Element ("typedef", [("fl", _); ("name", nam); ("dtype_id", tid)], xlst) ->
    let idx = int_of_string tid in TYP (idx, (TYPDF nam, ref "", TYPNONE, List.map (fun arg -> (rw' attr arg)) xlst))
| Xml.Element ("func", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) ->
    FNC (origin, nam, attr.typetable.(int_of_string tid), List.map (rw' attr) xlst)
| Xml.Element ("jumplabel", [("fl", origin)], xlst) -> jump_opt origin (List.map (rw' attr) xlst)
| Xml.Element ("jumpgo", [("fl", origin)], xlst) -> JMPG (origin, List.map (rw' attr) xlst)
| Xml.Element ("concat", [("fl", origin); ("dtype_id", tid)], xlst) -> CAT (origin, List.map (rw' attr) xlst)
| Xml.Element ("cvtpackstring", [("fl", origin); ("dtype_id", tid)], xlst) -> CPS (origin, List.map (rw' attr) xlst)
| Xml.Element ("cond", [("fl", origin); ("dtype_id", tid)], xlst) -> CND (origin, List.map (rw' attr) xlst)
| Xml.Element ("sformatf", [("fl", _); ("name", fmt); ("dtype_id", tid)], xlst) -> SFMT (fmt, List.map (rw' attr) xlst)
| Xml.Element ("module", ("fl", origin) :: ("name", nam) :: ("origName", nam') :: attr', xlst) ->
    let attr' = {attr with anchor=origin;names=ref []} in
    let decl,content = List.partition (function Xml.Element ("var", _, _) -> true | _ -> false) xlst in
    let xlst' = if false then decl@content else xlst in
    let xlst'' = List.map (rw' attr') xlst' in
    attr.modulexml := (nam, (xlst'', !(attr'.names))) :: !(attr.modulexml);
    let fd = open_out (nam^".elem") in
    output_string fd (dumplst xlst'');
    output_string fd ("\n["^String.concat ";\n " (List.map (fun (k,x) -> dumps k^", "^dumptab x) !(attr'.names))^"]\n");
    close_out fd;
    MODUL (origin, nam, nam', xlst'')
| Xml.Element ("case", [("fl", origin)], xlst) -> CS (origin, List.map (rw' attr) xlst)
| Xml.Element ("caseitem", [("fl", origin)], xlst) -> CSITM (origin, List.map (rw' attr) xlst)
| Xml.Element ("while", [("fl", _)], xlst) -> WHL (List.map (rw' attr) xlst)
| Xml.Element ("insiderange", [("fl", origin); ("dtype_id", tid)], xlst) -> IRNG (origin, List.map (rw' attr) xlst)
| Xml.Element ("funcref", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) -> FRF (origin, nam, List.map (rw' attr) xlst)
| Xml.Element ("varxref", [("fl", origin); ("name", nam); ("dtype_id", tid); ("dotted", dotted)], []) ->
    let typ' = attr.typetable.(int_of_string tid) in
    print_endline (dumptab typ');
    let dirop = if List.mem_assoc dotted !(attr.names) then
        begin
        let (dtype, _, _, _) as typ' = List.assoc dotted !(attr.names) in
        print_endline (dumptab typ');
        match dtype with
            | IFCRFDTYP ifc -> print_endline ifc; Dinam (ref ifc)
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
| Xml.Element ("initarray"|"streaml"|"powsu"|"realtobits"|"itord"|"rand" as op, [("fl", origin); ("dtype_id", tid)], xlst) ->
    SYS (origin, "$"^op, List.map (rw' attr) xlst)
| Xml.Element ("replicate", [("fl", origin); ("dtype_id", tid)], xlst) -> REPL (origin, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("iface", [("fl", origin); ("name", bus); ("origName", bus')], xlst) ->
    let attr' = {attr with anchor=origin;names=ref []} in
    let xlst' = List.map (rw' attr') xlst in
    attr.modulexml := (bus, (xlst', !(attr'.names))) :: !(attr.modulexml);
    IFC (origin, bus, bus', xlst')
| Xml.Element ("ifacerefdtype", [("fl", _); ("id", num); ("modportname", nam)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num in
    TYP(idx,(IFCRFDTYP nam, ref nam, TYPNONE, xlst'))
| Xml.Element ("modport", [("fl", origin); ("name", port)], xlst) -> IMP (origin, port, List.map (rw' attr) xlst)
| Xml.Element ("modportvarref", [("fl", origin); ("name", member); ("direction", dir)], xlst) -> IMRF (origin, member, dirop dir, List.map (rw' attr) xlst)
| Xml.Element ("basicdtype"|"structdtype"|"uniondtype" as dtyp', ("fl", _) :: ("id", num) :: rnglst, xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and dtyp = typenc dtyp' in
    (match rnglst with
      | ("name", nam) :: tl ->
          TYP(idx, (dtyp,ref nam,typmap tl,xlst'))
      | _ -> TYP(idx, (dtyp,ref "",typmap rnglst,xlst')));
| Xml.Element ("refdtype"|"enumdtype"|"memberdtype"|"paramtypedtype" as dtyp', [("fl", origin); ("id", num); ("name", nam); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and sub = int_of_string subtype and dtyp = typenc dtyp' in
    TYP(idx, (dtyp,ref nam,SUBTYP sub,xlst'))
| Xml.Element ("packarraydtype"|"unpackarraydtype"|"constdtype" as dtyp', [("fl", origin); ("id", num); ("sub_dtype_id", subtype)], xlst) ->
    let xlst' = List.map (rw' attr) xlst and idx = int_of_string num and sub = int_of_string subtype and typid = typenc dtyp' in    
    TYP(idx, (typid,ref "",SUBTYP sub,xlst'))
| Xml.Element ("enumitem" as dtyp, [("fl", origin); ("name", nam); ("dtype_id", num)], xlst) -> EITM (dtyp, nam, "", int_of_string num, List.map (rw' attr) xlst)
| Xml.Element ("cells", [], xlst) ->
    attr.intf := [];
    let xlst' = List.map (rw' attr) xlst in
    top := List.flatten(List.map cell_hier xlst');
    let topmod = List.filter (fun (_,kind) -> List.mem_assoc kind !(attr.modulexml)) !top in
    topattr := attr;
    List.iter (cell_traverse attr) topmod;    
    CELLS(xlst')
| Xml.Element ("cell", [("fl", origin); ("name", nam); ("submodname", subnam); ("hier", hier)], xlst) ->
    CELL(origin, nam, subnam, hier, List.map (rw' attr) xlst)
| Xml.Element ("display", [("fl", origin); ("displaytype", nam)], xlst) -> DSPLY (origin, nam, List.map (rw' attr) xlst)
| Xml.Element ("readmem", [("fl", origin)], xlst) -> SYS (origin, "$readmemh", List.map (rw' attr) xlst)
| Xml.Element (("fopen"|"fclose"|"finish"|"stop" as sys), [("fl", origin)], xlst) -> SYS (origin, "$"^sys, List.map (rw' attr) xlst)
| Xml.Element ("task", [("fl", origin); ("name", nam)], xlst) -> TASK(origin, nam, List.map (rw' attr) xlst)
| Xml.Element ("taskref", [("fl", origin); ("name", nam)], xlst) -> TASKRF(origin, nam, List.map (rw' attr) xlst)
| Xml.Element ("valueplusargs", [("fl", origin); ("dtype_id", tid)], xlst) -> VPLSRGS(origin, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("testplusargs", [("fl", origin); ("name", nam); ("dtype_id", tid)], xlst) ->
    TPLSRGS(origin, nam, int_of_string tid, List.map (rw' attr) xlst)
| Xml.Element ("modportftaskref", [("fl", origin); ("name", nam)], []) -> MODPORTFTR (origin, nam)
| Xml.Element ("typetable", [("fl", origin)], xlst) ->
    let types = List.map (fun itm -> (function TYP(ix,t) -> (ix,t) | _ -> (0,(UNKDTYP, ref "",TYPNONE, []))) (rw' attr itm)) xlst in
    let max = fold1 (max) (List.map (function (ix,_) -> ix) types) in
    let typarr = Array.make (max+1) (UNKDTYP, ref "", TYPNONE, []) in
    let rec subtypmap = function
    | RNG [CNST ((b,(HEX n|SHEX n)), _, []); CNST ((b',(HEX n'|SHEX n')), _, [])] -> TYPRNG(n,n')
    | EITM ("enumitem", itm, "", n, [CNST ((w',(HEX n'|SHEX n')), _, [])]) -> TYPENUM(itm, n, (w',n'))
    | TYP (idx, ((MEMBDTYP, id, SUBTYP idx', []) as typ')) -> typarr.(idx) <- maptyp' typ'; TYPMEMBER(maptyp idx')
    | oth -> subothlst := oth :: !subothlst; failwith "subothlst"
    and lookup ix = if List.mem_assoc ix types then List.assoc ix types else (UNKDTYP, ref "", TYPNONE, [])
    and maptyp' = function
        | (CNSTDTYP, s, SUBTYP idx', []) -> let (a,b,c,d) = maptyp idx' in b := "const"; (CNSTDTYP, s, RECTYP (a,b,c,d), [])
        | (t, s, SUBTYP idx', lst) -> (t, s, RECTYP (maptyp idx'), List.map subtypmap lst)
	| (t, s, typ, lst) -> (t, s, typ, List.map subtypmap lst)
    and maptyp ix = maptyp' (lookup ix) in
    List.iter (fun (ix, typ') -> typarr.(ix) <- maptyp ix) types;
    let fd = open_out "typetable.debug" in
    Array.iteri (fun ix itm -> output_string fd (string_of_int ix^":"^dumptab itm^"\n")) typarr;
    close_out fd;
    TYPETABLE typarr
| (Xml.Element (str, _, _) | Xml.PCData str) as err -> attr.errlst := err :: !(attr.errlst); failwith ("XML element "^str^" not supported")

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
needed=ref [];
avoid_dollar_unsigned = true;
remove_interfaces = false; }

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
needed=ref (List.rev !(prev.needed));
avoid_dollar_unsigned = prev.avoid_dollar_unsigned;
remove_interfaces = prev.remove_interfaces; }

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
needed=ref !(prev.needed);
avoid_dollar_unsigned = prev.avoid_dollar_unsigned;
remove_interfaces = prev.remove_interfaces; }

let num x = NUM (HEX x)

let rec expr modul = function
| VRF (id, []) -> IDENT id :: []
| CNST ((s,n), tid, []) -> SIZED (s,n) :: []
| UNRY (Uextend, expr1 :: []) when modul.avoid_dollar_unsigned -> LCURLY :: SIZED (1, BIN '0') :: COMMA :: expr modul expr1 @ [RCURLY]
| UNRY ((Uextend|Uextends) as op, expr1 :: []) -> IDENT (unaryopv op) :: LPAREN :: expr modul expr1 @ [RPAREN]
| UNRY (op, expr1 :: []) -> LPAREN :: IDENT (unaryopv op) :: expr modul expr1 @ [RPAREN]
| CMP ((Clts|Cltes|Cgtes|Cgts) as op, expr1 :: expr2 :: []) ->
    LPAREN :: expr modul (UNRY (Uextends, expr1 :: [])) @ CMPOP op :: expr modul (UNRY (Uextends, expr2 :: [])) @ [RPAREN]
| CMP (op, expr1 :: expr2 :: []) -> LPAREN :: expr modul expr1 @ CMPOP op :: expr modul expr2 @ [RPAREN]
| LOGIC (op, expr1 :: []) -> LPAREN :: IDENT (logopv op) :: expr modul expr1 @ [RPAREN]
| LOGIC (Lshiftrs as op, expr1 :: expr2 :: []) ->
    LPAREN :: expr modul (UNRY (Uextends, expr1 :: [])) @ (IDENT (logopv op) :: expr modul expr2) @[RPAREN]
| LOGIC (op, expr1 :: expr2 :: []) -> LPAREN :: expr modul expr1 @ (IDENT (logopv op) :: expr modul expr2) @[RPAREN]
| ARITH (op, expr1 :: expr2 :: []) -> LPAREN :: expr modul expr1 @ (IDENT (arithopv op) :: expr modul expr2) @[RPAREN]
| SEL (origin, ((VRF _ | XRF _ | ASEL _) as expr1) :: (CNST((szlo,lo'),_,_)) :: (CNST((szw,wid'),_,_)) :: []) ->
    expr modul expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: NUM lo' :: RBRACK :: []
    | _ -> LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: [])
| SEL (origin, ((VRF _ | XRF _ | ASEL _) as expr1) :: expr2 :: CNST((szw,wid'),_,_) :: []) ->
    expr modul expr1 @ (match wid' with HEX 1 | SHEX 1 -> LBRACK :: expr modul expr2 @ [RBRACK]
    | _ -> LBRACK :: expr modul expr2 @ [PLUS;COLON] @ (NUM wid' :: RBRACK :: []))
| SEL (origin, expr1 :: CNST ((32,(HEX 0 | SHEX 0)), idx, []) :: (CNST((szw,HEX wid'),_,_)) :: []) ->
    expr modul (LOGIC (Land, expr1 :: CNST ((wid', HEX ((1 lsl wid') -1)), idx, []) :: []))
| SEL (o, CND (origin, expr1 :: lft :: rght :: []) :: expr2 :: expr3 :: []) ->
    LPAREN :: expr modul expr1 @ [QUERY] @ expr modul (SEL (o, lft :: expr2 :: expr3 :: [])) @ [COLON] @ expr modul (SEL (o, rght :: expr2 :: expr3 :: [])) @ [RPAREN]
| SEL (orig, (UNRY (Uextend, VRF (id, []) :: []) :: (CNST _ as expr2) :: (CNST _ as expr3) :: [])) ->
    expr modul (SEL (orig, VRF (id, []) :: expr2 :: expr3 :: []))
| SEL (orig, (CNST ((32, SHEX n), idx, []) :: CNST ((32, HEX lo'), _, []) :: CNST ((32, HEX wid'), _, []) :: [])) ->
    SIZED (wid', SHEX (n asr lo')) :: []
    
| ASEL (VRF (lval, []) :: expr1 :: []) -> IDENT lval :: LBRACK :: expr modul expr1 @ [RBRACK]
| ASEL (ASEL _ as multi :: expr' :: []) -> expr modul multi @ LBRACK :: expr modul expr' @ [RBRACK]
| CND (origin, expr1 :: lft :: rght :: []) -> LPAREN :: expr modul expr1 @ [QUERY] @ expr modul lft @ [COLON] @ expr modul rght @ [RPAREN]
| CAT (origin, expr1 :: expr2 :: []) -> LCURLY :: expr modul expr1 @ [COMMA] @ expr modul expr2 @ [RCURLY]
| FRF (origin, fref, arglst) -> let delim = ref LPAREN in
    let lst = IDENT fref :: List.flatten (List.map (function
        | ARG (arg :: []) -> let lst = !delim :: expr modul arg in delim := COMMA; lst
        | _ -> [QUERY]) arglst) in
    lst @ (if !delim = LPAREN then [LPAREN;RPAREN] else [RPAREN])
| REPL (origin, tid, arg :: CNST ((sz,n'),_,_) :: []) -> LCURLY :: NUM n' :: LCURLY :: expr modul arg @ [RCURLY;RCURLY]
| IRNG (origin, expr2 :: expr1 :: []) -> LBRACK :: expr modul expr1 @ [COLON] @ expr modul expr2 @ [RBRACK]
| XRF (origin, id, tid, dotted, dirop) as xrf ->
    xrflst := xrf :: !xrflst;
    IDENT (dotted^(match dirop with Dinam _ when modul.remove_interfaces -> "_" | _ -> ".")^id) :: []
| TPLSRGS (origin, id, tid, []) -> IDENT "$test$plusargs" :: LPAREN :: DQUOTE :: IDENT id :: DQUOTE :: RPAREN :: []
| VPLSRGS (origin, tid, CNST ((len, fmt), _, []) :: VRF (arg, []) :: []) -> IDENT "$value$plusargs" :: LPAREN :: NUM fmt :: COMMA :: IDENT arg :: RPAREN :: []
| SYS (origin, "$rand", []) -> SP :: IDENT "$random" :: []
| SYS (origin, "$powsu", fst::snd::[]) -> LPAREN :: expr modul fst @ POW :: expr modul snd @ RPAREN :: []
| SYS (origin, "$streaml", hd::tl) -> LCURLY :: LSHIFT :: LCURLY :: expr modul hd @ RCURLY :: RCURLY :: []
| SYS (origin, "$initarray", arglst) -> let delim = ref SP in
    let initarr ix itm = let lst' = !delim :: num ix :: COLON :: expr modul itm in delim := COMMA; lst' in
    QUOTE :: LCURLY :: List.flatten (List.mapi initarr arglst) @ [RCURLY]
| SYS (origin, fn, arglst) -> IDENT fn :: LPAREN :: eiter modul SP arglst @ [RPAREN]

| SEL (origin, expr1 :: lo :: wid :: []) as sel -> selopt := Some sel; failwith "expr: selopt"

| oth -> exprothlst := oth :: !exprothlst; failwith "exprothlst"
and eiter modul tok lst =
    let delim = ref tok in
    List.flatten (List.map (fun itm -> let lst' = !delim :: expr modul itm in delim := COMMA; lst') lst)

let implicit s =
  let sub = "__pinNumber" in
  let l = String.length s and m = String.length sub in
  l > m && String.sub s 0 m = sub

let rec portconn modul = function
| VRF (id, []) -> DOT :: IDENT id :: []
| PORT (origin, id_o, dir, []) -> DOT :: IDENT id_o :: LPAREN :: RPAREN :: []
| PORT (origin, id_i, dir, expr1 :: []) when implicit id_i -> LPAREN :: expr modul expr1 @ [RPAREN]
| PORT (origin, id_i, dir, expr1 :: []) -> DOT :: IDENT id_i :: LPAREN :: expr modul expr1 @ [RPAREN]
| RNG [CNST ((_,lft), _, []); CNST ((_,rght), _, [])] -> LCOMMENT :: NUM lft :: COLON :: NUM rght :: RCOMMENT :: []
| oth -> portothlst := oth :: !portothlst; failwith "portothlst"

let rec ioconn = function
| CNST (cnst, _, []) -> cnst
| oth -> iothlst := oth :: !iothlst; failwith "iothlst"

let stmtdly = function
| false -> SP :: ASSIGNMENT :: SP :: []
| true -> SP :: ASSIGNDLY :: SP :: []

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
| SP :: DOT :: tl -> NL :: DOT :: reformat1 tl
| COMMA :: DOT :: tl -> COMMA :: NL :: DOT :: reformat1 tl
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

let reviter modul lst =
    let delim = ref COLON in
    List.rev (List.flatten (List.map (fun itm -> let lst' = !delim :: expr modul itm in delim := COMMA; lst') lst))

let rec cntbasic = function
| (STRDTYP,_,typmap,rw_lst) -> fold1 (+) (List.flatten (List.map cntmembers rw_lst)) :: []
| (UNIDTYP,_,typmap,rw_lst) -> fold1 (max) (List.flatten (List.map cntmembers rw_lst)) :: []
| (BASDTYP, typ, TYPRNG(hi, lo), []) when (function "logic"|"integer"|"int"|"bit"|"wire" -> true | _ -> false) !typ -> hi - lo + 1 :: []
| (BASDTYP, {contents=("logic"|"bit")}, TYPNONE, []) -> 1 :: []
| (BASDTYP, {contents="real"}, TYPNONE, []) -> 64 :: []
| (BASDTYP, {contents="string"}, TYPNONE, []) -> 1 :: []
| (IFCRFDTYP _, _, TYPNONE, []) -> 0 :: []
| (PACKADTYP, {contents=_}, RECTYP subtyp, TYPRNG(n,n')::_) -> List.map (fun itm -> itm * (n - n' + 1)) (findmembers subtyp)
| (UNPACKADTYP, {contents=_}, RECTYP subtyp, TYPRNG (n,n')::_) -> (n - n' + 1) :: findmembers subtyp
| (MEMBDTYP, id, SUBTYP subtyp, []) -> 1 :: []
| oth -> typopt := Some oth; failwith "typopt;;1425"

and cntmembers = function
| TYPMEMBER typ' -> findmembers typ'
| oth -> memothlst := oth :: !memothlst; failwith "memothlst"

and findmembers' typ' =
        let (t,s,m1,ml) = typ' in
        (cntbasic typ', !s="const", match m1 with TYPRNG _ -> true | _ -> false)

and findmembers typ' = let (widlst,cnst,rng) = findmembers' typ' in widlst

let comment widlst =
        let delim = ref LCOMMENT in
        List.flatten (List.map (fun itm -> let lst = !delim :: num itm :: [] in delim := SEMI; lst) widlst) @ [RCOMMENT]

let expand delim = fun w -> let lst = !delim :: num w :: [] in delim := STAR; lst

let widshow id rng = function
| [] -> IDENT id :: []
| 1 :: [] when not rng -> IDENT id :: []
| n :: [] -> LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: []
| n :: m :: [] -> LBRACK :: num (m-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: SP :: LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: []
| n :: m :: l :: [] -> LBRACK :: num (l-1) :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: SP :: LBRACK :: num (m-1) :: COLON :: num 0 :: RBRACK :: LBRACK :: num (n-1) :: COLON :: num 0 :: RBRACK :: SP :: []
| lst -> let delim = ref LBRACK in List.flatten (List.map (expand delim) lst) @ MINUS :: num 1 :: COLON :: num 0 :: RBRACK :: SP :: IDENT id :: []

let varlst modul delim typ' id =
    let (widlst,cnst,rng) = findmembers' typ' in
    let decl = !delim :: (if cnst then WIRE else LOGIC) :: SP :: widshow id rng widlst in decl @
    comment widlst @ (if List.mem_assoc id !(modul.cnst) then
                         begin
                         let (isc,init) = List.assoc id !(modul.cnst) in
                         if cnst && isc then
                             begin
                             SP :: ASSIGNMENT :: SP :: SIZED init :: []
                             end
                         else
                             begin
                             SEMI :: NL :: INITIAL :: SP :: IDENT id :: SP :: ASSIGNMENT :: SP :: SIZED init :: []
                             end
                         end else [])

let rec iter2 modul dly lst =
    List.flatten (List.map (fun itm -> cstmt modul dly itm @ [SEMI;NL]) lst)

and csitm modul dly = function
    | CSITM (origin, []) -> SEMI :: []
    | CSITM (origin, IRNG (_, [CNST ((w, HEX lo), idx, []); CNST ((w', HEX hi), idx', [])]) :: (BGN _ as st) :: []) ->
        eiter modul SP (Array.to_list (Array.init (hi-lo+1) (fun x -> CNST ((w, HEX (x+lo)), idx, [])))) @ COLON :: cstmt modul dly st @ NL :: []
    | CSITM (origin, cexp :: (BGN _ as st) :: []) -> expr modul cexp @ COLON :: cstmt modul dly st @ NL :: []
    | CSITM (origin, (CNST _ as cexp) :: []) -> expr modul cexp @ COMMA :: []
    | CSITM (origin, st :: []) -> DEFAULT :: COLON :: cstmt modul dly st @ SEMI :: []
    | CSITM (origin, cexplst) -> let lbl, stmts = List.partition (function CNST _ | VRF _ | LOGIC _ -> true | _ -> false) cexplst in
                                 (if lbl <> [] then eiter modul SP lbl else DEFAULT :: []) @ COLON :: BEGIN None :: iter2 modul dly stmts @ [END;NL]
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
| BGN(lbl, rw_lst) ->
    let decl,body = List.partition (function VAR _ -> true | _ -> false) rw_lst in
    BEGIN lbl :: iter2 modul dly (decl@body) @ [END;NL]
| IF(origin, cnd :: then_stmt :: []) -> ifcond (expr modul cnd) @ (SP :: cstmt modul dly then_stmt)
| IF(origin, cnd :: (BGN _ as then_stmt) :: else_stmt :: []) ->
    ifcond (expr modul cnd) @ (SP :: cstmt modul dly then_stmt) @ [ELSE] @ cstmt modul dly else_stmt
| IF(origin, cnd :: then_stmt :: else_stmt :: []) ->
    ifcond (expr modul cnd) @ (SP :: cstmt modul dly then_stmt) @ [SEMI;ELSE] @ cstmt modul dly else_stmt
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
    IDENT lval :: LBRACK :: expr modul expr1 @ RBRACK :: LBRACK :: NUM (cadd [lo';wid';SHEX (-1)]) :: COLON :: NUM lo' :: RBRACK :: stmtdly true @ expr modul src
| ASGN(dly, origin, src :: dst :: []) ->
    expr modul dst @ stmtdly dly @ expr modul src
| CS (origin, sel :: lst) -> CASE :: LPAREN :: expr modul sel @ (RPAREN :: NL :: List.flatten (List.map (csitm modul dly) lst)) @ [NL;ENDCASE]
| CA(origin, rght::lft::[]) -> ASSIGN :: SP :: expr modul lft @ stmtdly dly @ expr modul rght
| VAR (origin, idlst, typ', _) -> List.flatten (List.map (fun id -> varlst modul (ref NL) typ' id) idlst)
| FORSTMT (o,kind,cnd,ix,strt,stop,inc,stmts) ->
    FOR :: LPAREN :: (if kind <> "" then IDENT kind :: SP :: [] else []) @
    IDENT ix :: ASSIGNMENT :: SIZED strt :: SEMI ::
    SIZED stop :: CMPOP cnd :: IDENT ix :: SEMI ::
    IDENT ix :: ASSIGNMENT :: IDENT ix :: PLUS :: SIZED inc :: RPAREN ::
    BEGIN None :: iter2 modul dly stmts @ [END]
| DSPLY (origin, typ, SFMT (fmt, arglst) :: []) -> IDENT typ :: LPAREN :: DQUOTE :: IDENT fmt :: DQUOTE :: eiter modul COMMA arglst @ [RPAREN]
| DSPLY (origin, typ, SFMT (fmt, arglst) :: expr1 :: []) ->
    IDENT typ :: LPAREN :: expr modul expr1 @ (COMMA :: DQUOTE :: IDENT fmt :: DQUOTE :: COMMA :: eiter modul SP arglst) @ [RPAREN]
| SYS (origin, ("$fopen" as fn), frst::secnd::thrd::[]) ->
    expr modul frst @ ASSIGNMENT :: IDENT fn :: LPAREN :: expr modul secnd @ COMMA :: expr modul thrd @ [RPAREN]
| SYS (origin, fn, arglst) -> IDENT fn :: LPAREN :: eiter modul SP arglst @ [RPAREN]
| CNST((s,n), _, []) -> SIZED (s,n) :: []
| TASKRF (origin, nam, arglst) -> IDENT nam :: (if arglst <> [] then eiter modul LPAREN arglst @ [RPAREN] else [])
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
| TASK(origin, nam, rw_lst) :: tl -> TASK(origin, nam, optitm3 rw_lst) :: optitm3 tl
| TASKRF(origin, nam, rw_lst) :: tl -> TASKRF(origin, nam, optitm3 rw_lst) :: optitm3 tl
| ASGN _ as oth :: tl -> oth :: optitm3 tl
| IF(origin, cnd :: then_stmt :: []) :: tl -> IF (origin, cnd :: BGN(None, optitm3 [then_stmt]) :: []) :: optitm3 tl
| IF(origin, cnd :: then_stmt :: else_stmt :: []) :: tl -> IF (origin, cnd :: BGN(None, optitm3 [then_stmt]) :: BGN(None, optitm3 [else_stmt]) :: []) :: optitm3 tl
| (CNST _ | VAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _ | UNRY _) as oth :: tl -> oth :: optitm3 tl
| oth :: tl when !catch_escapes -> optothlst := oth :: !optothlst; failwith ("optothlst3: "^dumpitm oth)
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
| TASK(origin, nam, rw_lst) -> TASK(origin, nam, List.map optitm4 rw_lst)
| TASKRF(origin, nam, rw_lst) -> TASKRF(origin, nam, List.map optitm4 rw_lst)
| (ASGN _  | CNST _ | VAR _ | VRF _ | LOGIC _ | SEL _ | CMP _ | DSPLY _ | SYS _) as oth -> oth
| oth -> optothlst := oth :: !optothlst; failwith "optothlst4"

let optitm lst =
    let lst' = optitm3 lst in
    optopt := Some (lst,lst');
    let lst'' = List.map optitm4 lst' in
    lst''

let rec is_cnst itms id =
    if List.mem_assoc id !(itms.v) then
        begin
        let (origin, ((a,b,c,d) as typ'), kind', n) = List.assoc id !(itms.v) in
        print_endline (dumptab typ');
        !b = "const"
        end
    else
        false

let rec catitm (pth:string option) itms = function
| IO(origin, str1lst, typ1, dir, str3, clst) ->
    List.iter (fun str1 ->
        let typ' = ref typ1 in
        List.iter (fun itm ->
            itms.ca := (origin, VRF(str1,[]), itm) :: !(itms.ca);
            let (a,b,c,d) = typ1 in typ' := (a,ref "wire",c,d)) clst;
        itms.io := (str1, (origin, !typ', dir, str3, List.map ioconn clst)) :: !(itms.io);
        ) str1lst
| VAR(origin, str1lst, typ1, "ifaceref") -> List.iter (fun str1 -> itms.ir := (origin, str1, typ1) :: !(itms.ir)) str1lst
| VAR(origin, str1lst, typ1, str2) -> List.iter (fun str1 ->
    if not (List.mem_assoc str1 !(itms.v)) then
        itms.v := (str1, (origin, typ1, str2, (UNKDTYP,ref "",TYPNONE,[]))) :: !(itms.v)) str1lst
| IVAR(origin, str1, int1, rwlst, int2) -> itms.iv := (str1, (origin, int1, rwlst, int2)) :: !(itms.iv)
| TMPVAR(origin, str1, typ1, stmtlst) ->
    List.iter (catitm pth itms) stmtlst;
    if not (List.mem_assoc str1 !(itms.v)) then
        itms.v := (str1, (origin, typ1, str1, (UNKDTYP,ref "",TYPNONE,[]))) :: !(itms.v)
| CA(origin, (rght::lft::[] as args)) ->
    List.iter (catitm pth itms) args;
    itms.ca := (origin, lft, rght) :: !(itms.ca)
| INST(origin, str1lst, (str2, port_lst)) ->
    List.iter (fun str1 ->
        let pth' = match lcombine(pth,Some str1) with Some s -> s | None -> failwith "lcombine" in
        itms.inst := (pth', (origin, str2, port_lst)) :: !(itms.inst)
        ) str1lst
| ALWYS(origin, SNTRE(SNITM ("POS", [VRF (ck, [])]) :: SNITM ("POS", [VRF (rst, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.alwys := (origin, POSPOS(ck,rst), optitm rw_lst) :: !(itms.alwys)    
| ALWYS(origin, SNTRE(SNITM ("POS", [VRF (ck, [])]) :: []) :: rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    let rw_lst' = optitm rw_lst in
    if rw_lst' <> [] then itms.alwys := (origin, POSEDGE(ck), rw_lst') :: !(itms.alwys)
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
             itms.cnst := (id,(is_cnst itms id,cnst)) :: !(itms.cnst);
             print_endline ("initial found : "^id);
        | _ -> itms.init := (origin, INITIAL, rw_lst) :: !(itms.init));
| INIT (origin, "final", rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.init := (origin, FINAL, rw_lst) :: !(itms.init)
| BGN(pth', rw_lst) ->
    List.iter (catitm (lcombine (pth,pth')) itms) rw_lst
| FNC(origin, nam, typ', rw_lst) ->
    let itms' = empty_itms () in
    List.iter (catitm pth itms') rw_lst;
    let fn = (origin, typ', rw_lst, itms') in
    itms.func := (nam, fn) :: !(itms.func);
    Hashtbl.add functable nam fn;
| IF(origin, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    itms.gen := (origin,rw_lst) :: !(itms.gen)
| IMP(origin, nam, rw_lst) ->
    itms.imp := (nam, (origin, List.map (function
    | IMRF(_, str1, dir, []) -> (str1, dir)
    | MODPORTFTR (_,str1) -> (str1, Dunknown)
    | oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst;;1442") rw_lst)) :: !(itms.imp)
| IMRF(origin, str1, str2, []) -> ()
| TASK (origin, nam, rw_lst) ->
    let itms' = empty_itms () in
    List.iter (catitm pth itms') rw_lst;
    let tsk = (origin, rw_lst, itms') in
    itms.task := (nam, tsk) :: !(itms.task);
    Hashtbl.add tasktable nam tsk;
| TASKRF (origin, nam, rw_lst) ->
    List.iter (catitm pth itms) rw_lst;
    if not (List.mem (TASK,nam) !(itms.needed)) then
        itms.needed := (TASK,nam) :: !(itms.needed)
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
| SFMT(_, rw_lst)
| SYS(_,_, rw_lst)
| PORT(_, _, _, rw_lst)
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
    if not (List.mem (FUNCTION,nam) !(itms.needed)) then
        begin
        print_endline ("Needed function: "^nam);
        itms.needed := (FUNCTION,nam) :: !(itms.needed);
        end
| XRF(origin, str1, str2, str3, dirop) -> ()
| MODUL(origin, str1, str2, rw_lst) ->
    let itms = empty_itms () in
    List.iter (catitm None itms) rw_lst;
    let itms' = rev_itms itms in
    if Hashtbl.mem hierarchy str1 then Hashtbl.add modules str1 (origin, itms')
    else if Hashtbl.mem hierarchy str2 then Hashtbl.add modules str2 (origin, itms')
    else print_endline ("Module "^str1^"/"^str2^" not in cell hierarchy");
| PKG(origin, str1, rw_lst) ->
    let itms = empty_itms () in
    List.iter (catitm (Some str1) itms) rw_lst;
    Hashtbl.add packages str1 (origin, itms)
| IFC(origin, str1, str2, rw_lst) ->
    let itms = empty_itms () in
    List.iter (catitm (Some str1) itms) rw_lst;
    Hashtbl.add interfaces str1 (origin, itms)
| FIL(enc, fil) ->
    Hashtbl.add files enc fil
| CELLS(rw_lst) -> ()
| TPLSRGS (_, id, tid, []) -> ()
| TYPETABLE _ -> ()
| TYP _ -> ()
| oth -> itmothlst := oth :: !itmothlst; failwith "itmothlst;;1508"

let iolst modul delim dir io = function
| (IFCRFDTYP dir, kind, TYPNONE, []) -> !delim :: NL :: IDENT !kind :: DOT :: IDENT dir :: SP :: IDENT io :: []
| (BASDTYP, kind, typmap, []) as typ' ->
    let (widlst,cnst,rng) = findmembers' typ' in
    !delim :: (DIR dir :: SP :: IDENT !kind :: SP :: widshow io rng widlst @ comment widlst)
| oth -> typopt := Some oth; failwith "typopt;;1762"

let fndlm = function
| FIRSTG -> LPAREN::RPAREN::SEMI::[]
| IOSTG -> RPAREN::SEMI::[]
| VARSTG
| JMPSTG
| BDYSTG -> SEMI :: []
	     
let rec fnstmt modul dly stg = function
| IO (origin, iolst', typ', dir, _, lst) ->
    let lst = List.flatten (List.map (fun io -> iolst modul (ref (if !stg = FIRSTG then LPAREN else COMMA)) dir io typ') iolst') in
    stg := IOSTG;
    lst
| VAR (origin, idlst, typ', _) -> 
    let dlm = fndlm !stg in
    stg := VARSTG;
    dlm @ List.flatten (List.map (fun id -> varlst modul (ref NL) typ' id) idlst)
| JMPL(origin, rw_lst) ->
    let dlm = fndlm !stg in
    stg := JMPSTG;
    dlm @ BEGIN None :: (List.flatten (List.map (fnstmt modul dly stg) rw_lst)) @ [SEMI;END]
| JMPG (_,[]) -> []
| itm ->
    let dlm = fndlm !stg in
    stg := BDYSTG;
    dlm @ cstmt modul dly itm

let rec taskstmt modul dly nam = function
| BGN(_,rw_lst) -> List.flatten (List.map (taskstmt modul dly nam) rw_lst)
| itm -> cstmt modul dly itm @ SEMI :: NL :: []

let outnam f = f^".v"
let outnamopt f = let l = String.length f in f^(if l < 4 || String.sub f (l-4) 4 <> "_opt" then "_opt.v" else ".v")
let outtok f = f^"_tokens.txt"
let outtcl f = "./"^f^"_fm.tcl"

let needed modul (kind,nam) = match kind with
| FUNCTION ->
    print_endline ("Searching function: "^nam);
    let found = List.mem_assoc nam !(modul.func) in
    let (origin, typ', lst, itms') = if found then
        List.assoc nam !(modul.func)
    else
        Hashtbl.find functable nam in
    let stg = ref FIRSTG in let lst = fsrc origin :: NL :: FUNCTION :: SP :: (varlst modul (ref NL) typ' nam) @
    List.flatten (List.map (fnstmt modul false stg) (List.tl lst)) in
    lst @ (fndlm !stg @ [NL;ENDFUNCTION])
| TASK ->
    print_endline ("Searching task: "^nam);
    let found = List.mem_assoc nam !(modul.task) in
    let (origin, lst, itms') = if found then
        List.assoc nam !(modul.task)
    else
        Hashtbl.find tasktable nam in
    let lst = List.flatten (List.map (taskstmt modul false nam) lst) in
    fsrc origin :: TASK :: SP :: IDENT nam :: SEMI :: NL :: BEGIN None :: lst @ END :: ENDTASK :: NL :: []
| oth -> failwith "needed"

let dump intf f (origin, modul) =
  let appendlst = ref [] in
  let append lst = appendlst := lst :: !appendlst in
  if true then print_endline ("f \""^f^"\";; /* "^outnam f^" : "^outtcl f ^" */");
  let head = ref [fsrc origin; if intf then INTERFACE else MODULE; SP; IDENT f] in
  let delim = ref LPAREN in
  List.iter (fun (io, (origin, typ', dir, kind', lst)) -> 
    head := !head @ iolst modul delim dir io typ';
    delim := COMMA;
    ) (!(modul.io));
  head := !head @ (if !delim <> COMMA then !delim :: [] else []) @ [RPAREN;SEMI;NL];
  List.iter (fun (id, (origin, typ', kind', n)) -> append (fsrc origin :: varlst modul (ref NL) typ' id @ SEMI :: NL :: []);
                 ) (List.rev !(modul.v));
  List.iter (fun itm -> append (needed modul itm)) (List.rev !(modul.needed));
  List.iter (fun (a, (origin, lst)) -> let delim = ref LPAREN in
    let lst = MODPORT :: SP :: IDENT a ::
    List.flatten (List.map (fun (nam',dir') -> let lst = !delim :: DIR dir' :: SP :: IDENT nam' :: [] in delim := COMMA; lst) lst) @
    [RPAREN; SEMI] in
    append ((fsrc origin) :: lst)) !(modul.imp);  
  List.iter (fun (origin, dst, src) ->
                 append (fsrc origin :: ASSIGN :: SP :: expr modul dst @ (SP :: ASSIGNMENT :: SP:: expr modul src @ SEMI :: NL :: []));
                 ) (List.rev !(modul.ca));
  List.iter (function
    | (origin, COMB, lst) ->
      append (fsrc origin :: ALWAYS :: AT :: STAR :: flatten1 modul false lst);
    | (origin, POSPOS (ck, rst), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: COMMA :: POSEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | (origin, POSNEG (ck, rst), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | (origin, NEGNEG (ck, rst), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: COMMA :: NEGEDGE :: SP :: IDENT rst :: RPAREN :: flatten1 modul true lst);
    | (origin, POSEDGE (ck), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: POSEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 modul true lst);
    | (origin, NEGEDGE (ck), lst) ->
      append (fsrc origin :: ALWAYS :: AT :: LPAREN :: NEGEDGE :: SP :: IDENT ck :: RPAREN :: flatten1 modul true lst);
    | (origin, _, lst) -> failwith "edge specification not implemented";
    ) (List.rev (List.map (fun (origin,edg,lst) -> (origin, edg, lst)) !(modul.alwys)));
  List.iter (fun (inst, (origin, kind, lst)) ->
                 let delim = ref SP in
                 let lst = List.flatten (List.map (fun term -> let lst = !delim :: portconn modul term in delim := COMMA; lst) lst) in
                 append (fsrc origin :: IDENT kind :: SP :: IDENT inst :: LPAREN :: lst @ [NL;RPAREN;SEMI]);
                 ) !(modul.inst);
  List.iter (fun (origin, tok, lst) -> append (fsrc origin :: tok :: flatten1 modul false lst);
                 ) !(modul.init);
  !head @ List.flatten (List.sort compare !appendlst) @ [NL;if intf then ENDINTERFACE else ENDMODULE;NL;NL]

let rec iterate depth f (modorig, modul) =
    let indent = String.make depth ' ' in
    let newitms = copy_itms modul in
    newitms.ir := [];
    newitms.inst := [];
    print_string (indent^"Scanning instances of "^f^": ["^String.concat ";" (List.map (fun (inst, (_,kind,_)) -> inst^"("^kind^")") !(modul.inst))^"]");
    List.iter (fun (inst, (origin, kind, iolst)) ->
        if Hashtbl.mem interfaces kind then
           begin
           let (_, intf) = Hashtbl.find interfaces kind in
           List.iter (fun (nam, (origin, typ', kind, n)) ->
                 let pth = inst^"_"^nam in
                 newitms.v := (pth, (modorig, typ', kind, n)) :: !(newitms.v);
                ) !(intf.v);
           end
        else if Hashtbl.mem modules kind then
           begin
           print_endline (indent^"Iterating: "^kind);
           let (kindorig, itms) = Hashtbl.find modules kind in
           let newiolst = ref [] in
           let newinnerlst = ref [] in
	   let previolst = !(itms.io) in
           List.iter2 (fun ((_, (origin, typ', idir', typ'', ilst)) as inr) -> function
		       | VRF (id, []) ->
                           newiolst := PORT(origin, id, idir', [VRF(id, [])]) :: !newiolst;
                           newinnerlst := inr :: !newinnerlst;
		       | PORT (origin, id_i, Dvif bus, VRF (id, []) :: []) as pat ->
                           if List.mem_assoc id !(modul.inst) then
                               begin
                               let (_,inam,_) = List.assoc id !(modul.inst) in
                               print_endline (indent^id^" is a local bus mapped to "^inam);
                               bus := inam;
                               end;
                           print_endline (indent^id^" connects to "^id_i);
                           let inam = !bus in
                           print_endline (indent^id^" maps to "^inam);
                           if Hashtbl.mem interfaces inam then (match typ' with (IFCRFDTYP iport, simple, TYPNONE, []) ->
                              begin
                              print_endline (indent^iport^" matched");
                              let (_, intf) = Hashtbl.find interfaces inam in
                              print_endline (indent^"Searching for "^ iport);
                              if List.mem_assoc iport !(intf.imp) then
                              let (origin, lst) = List.assoc iport !(intf.imp) in
                              List.iter (fun (nam, dir) ->
                                    print_endline (indent^inam^":"^iport^":"^nam^":"^id_i);
                                    let (_, typ', kind, _) = List.assoc nam !(intf.v) in
                                    newiolst := PORT(origin, id_i^"_"^nam, dir, [VRF(id^"_"^nam, [])]) :: !newiolst;
                                    newinnerlst := (id_i^"_"^nam, (origin, typ', dir, typ'', ilst)) :: !newinnerlst) lst
                              else print_endline (indent^"Direction "^ iport ^" not found");
                              end
                           | _ ->
                              print_endline (indent^dumptab typ'^" did not match");
                              newiolst := pat :: !newiolst; newinnerlst := inr :: !newinnerlst)
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
               let newhash = (kindorig, newinneritms) in
	       iterate (depth+2) kind newhash
               end;
           newitms.inst := (inst, (origin, kind_opt, newiolst)) :: !(newitms.inst);
           end
        ) !(modul.inst);
    newitms.inst := List.rev (!(newitms.inst));
    Hashtbl.replace modules_opt (f^"_opt") (modorig, newitms);
    print_endline (indent^f^" done")

let dumpform f f' separate = 
    let fd = open_out (outtcl f') in
    let srcpath = try Sys.getenv "XMLSRCPATH" with err -> "." in
    Printf.fprintf fd "#!/opt/synopsys/fm_vO-2018.06-SP3/bin/fm_shell -f\n";
    Printf.fprintf fd "set hdlin_warn_on_mismatch_message \"FMR_ELAB-115 FMR_ELAB-117 FMR_ELAB-146 FMR_ELAB-147 FMR_VLOG-928\"\n";
    Printf.fprintf fd "read_sverilog -container r -libname WORK -12 { \\\n";
    let plst = ref [] in Hashtbl.iter (fun _ (s,_) -> plst := fst (find_source s) :: !plst) packages;
    let iflst = List.map snd (if Hashtbl.mem hierarchy f then Hashtbl.find hierarchy f else []) in
    let hlst = List.sort_uniq compare (List.map (fun k -> let (s, _) = if Hashtbl.mem modules k then Hashtbl.find modules k else (k, empty_itms ()) in fst (find_source s)) (f::iflst)) in
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
    
let rec debug f (origin, modul) =
  let fd = open_out (f^".debug") in
  dumpitms fd modul;
  close_out fd

let readxml xmlf =
    let xmlerr = ref None in
    let xml = try Xml.parse_file xmlf with Xml.Error err -> xmlerr := Some err; Xml.PCData "Xml.Error" in
    match !xmlerr with Some (_, errpos) -> (Xml.line errpos, Xml.range errpos, xml) | None -> (0, (0,0), xml)

let translate errlst xmlf =
    let (line,range,xml) = readxml xmlf in
    let empty = empty_itms () in
    let empty_attr = empty_attr errlst in
    let rwxml = rw' empty_attr xml in
    catitm None empty rwxml;
    let top = snd(List.hd !top) in
    print_endline ("toplevel is "^top);
    let separate = try int_of_string (Sys.getenv "VXML_SEPARATE") > 0 with _ -> true in
    let debugtree = try int_of_string (Sys.getenv "VXML_DEBUGTREE") > 0 with _ -> true in
    let opttree = try int_of_string (Sys.getenv "VXML_OPTTREE") > 0 with _ -> true in
    let tophash = Hashtbl.find modules top in
    if opttree then iterate 0 top tophash;
    let top_opt = top^"_opt" in
    dumpform top top_opt separate;
    let mods = ref [] in
    if debugtree then
        begin
        Hashtbl.iter debug interfaces;
        Hashtbl.iter debug modules;
        if opttree then Hashtbl.iter debug modules_opt;
        end;
    Hashtbl.iter (fun k x -> let d = reformat0 (dump true k x) in
        mods := (k, d, reformat2 (reformat1 d)) :: !mods) interfaces;
    Hashtbl.iter (fun k x -> let d = reformat0 (dump false k x) in
        mods := (k, d, reformat2 (reformat1 d)) :: !mods) modules;
    if opttree then Hashtbl.iter (fun k (o, m) -> let d = reformat0 (dump false k (o, {m with remove_interfaces=true})) in
        mods := (k, d, reformat2 (reformat1 d)) :: !mods) modules_opt;
    let mods = List.sort compare !mods in
    let indent = ref 0 in
    if separate then
        begin
        List.iter (fun (k,d,lst) ->
        Hashtbl.add modtokens k (d,lst);
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
        List.iter (fun (_,_,lst) ->
            List.iter (tokenout fd indent) lst;
        ) mods;
        close_out fd;
        end;
    (line,range,rwxml,xml,mods)
