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
| CELL of string * string * string * string *rw list
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

type itms = { 
  top: bool;
  io: (string*int*dirop*string*string list) list ref;
  v: (string*(int*string*int)) list ref;
  iv: (string*(int*rw list*int)) list ref;
  ir: (string*int) list ref;
  ca: (rw*rw) list ref;
  typ: string list ref;
  alwys: (rw*rw list) list ref;
  init: (rw*rw list) list ref;
  func: (string*int*rw list) list ref;
  task: (string*rw list) list ref;
  gen : rw list list ref;
  imp : (string*string) list list ref;
  inst: (string*(string*rw list)) list ref;
}

val exprothlst : rw list ref
val stmtothlst : rw list ref
val portothlst : rw list ref
val iothlst : rw list ref
val csothlst : rw list ref
val bgnothlst : rw list ref
val itmothlst : rw list ref
val catothlst : rw list ref
val cellothlst : rw list ref
val posneglst : rw list list ref
val typothlst : (string * string * typmap * typmap list) list ref
val memothlst : typmap list ref
val subothlst : rw list ref
val mapothlst : (string * string) list list ref

val modules : (string, string * int * itms) Hashtbl.t
val modules_opt : (string, string * int * itms) Hashtbl.t
val packages : (string, string * int * itms) Hashtbl.t
val hierarchy : (string, (string * string) list) Hashtbl.t
val typetable : (int, string*string*typmap*typmap list) Hashtbl.t
val interfaces : (string, string * int * itms * rw list) Hashtbl.t
val top : (string * string) list ref

val translate : Xml.xml list ref -> string -> int * (int * int) * rw * Xml.xml
val dump : out_channel -> string -> string * int * itms -> unit
