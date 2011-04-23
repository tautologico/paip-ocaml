(*
 *
 * symbol.ml
 * Defines symbols as "interned strings", similar to Lisp symbols. 
 * 
 * Andrei de Araujo Formiga, 2011-04-15
 * 
 *)

(*** type definitions ***)
type symbol = (string * int)

(*** data ****)

let create str = 
  (str, Hashtbl.hash str)

let str (s, h) = 
  s

let compare (s1,h1) (s2,h2) = compare h1 h2


  

