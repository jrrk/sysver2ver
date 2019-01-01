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

val exprothlst : rw list ref
val stmtothlst : rw list ref
val portothlst : rw list ref
val iothlst : rw list ref
val csothlst : rw list list ref
val bgnothlst : rw list ref
val itmothlst : rw list ref
val catothlst : rw list ref
val cellothlst : rw list ref
val posneglst : rw list list ref
val typothlst : typetable_t list ref
val memothlst : typmap list ref
val subothlst : rw list ref
val mapothlst : (string * string) list list ref
val tskothlst : rw list ref
val optothlst : rw list ref
val xrflst : rw list ref
val smplopt : rw option ref
val selopt : rw option ref
val optopt : (rw list * rw list) option ref
val forlst : (rw * rw * rw list) list ref
val ternlst : (rw * rw * rw * rw) list ref
val ternothlst : (rw * rw) list ref
val widthlst : rw list ref
val smpothlst : rw list ref
val optitmlst : (rw list * rw list) list ref

val modules : (string, string * itms * rw list) Hashtbl.t
val modules_opt : (string, string * itms * rw list) Hashtbl.t
val packages : (string, string * itms) Hashtbl.t
val interfaces : (string, string * itms * rw list) Hashtbl.t
val hierarchy : (string, (string * string) list) Hashtbl.t
val functable : (string, string * int * rw list * itms) Hashtbl.t
val typetable : (int, typetable_t) Hashtbl.t

val top : (string * string) list ref

val hex_of_bigint : int -> Big_int.big_int -> string
val hex_to_bigint : string -> Big_int.big_int
val decode : string -> cexp
val cadd : cexp list -> cexp
val cexp : string -> int * cexp
val expr : rw -> token list
val ewidth : rw -> int
val cntmembers : typmap -> int list
val findmembers : int -> int list
val findmembers' : int -> int list * bool * bool
val optitm : rw list -> rw list
val simplify_exp : string -> rw list ref -> rw -> rw
val simplify_asgn : bool -> string -> rw -> rw -> rw
val jump_opt : string -> rw list -> rw
val fortailmatch : string -> rw list -> bool
val translate : Xml.xml list ref -> string -> int * (int * int) * rw * Xml.xml * (string * token list) list
val dump : string -> string * itms * rw list -> token list
val debug : string -> string * itms * rw list -> unit
