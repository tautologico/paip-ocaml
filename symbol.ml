(*
 *
 * symbol.ml
 * Defines symbols as "interned strings", similar to Lisp symbols. 
 * 
 * Andrei de Araujo Formiga, 2011-04-15
 * 
 *)

(*** type definitions ***)
type symbol = int

(*** data ****)

(** Current index for symbols *)
let index = ref 0

let start_size = 30

(** The symbol table *)
let sym_table : (symbol, string) Hashtbl.t = Hashtbl.create start_size

(*** functions to operate with symbols ***)

let create str = 
  let i = !index in
  Hashtbl.add sym_table i str;
  incr index;
  i

let str sym = 
  if sym >= !index then 
    failwith "Symbol not created"
  else
    Hashtbl.find sym_table sym

let compare s1 s2 = compare s1 s2 

  

