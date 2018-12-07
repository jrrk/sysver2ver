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
  gen : string list list ref;
  inst: (string*string*string list) list ref;
}

val modules : (string, itms) Hashtbl.t
val packages : (string, itms) Hashtbl.t

val translate : string -> int * (int * int)
