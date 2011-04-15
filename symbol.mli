(*
 *
 * symbol.mli
 * Defines symbols as "interned strings", similar to Lisp symbols. 
 * 
 * Andrei de Araujo Formiga, 2011-04-15
 * 
 *)

type symbol

val create : string -> symbol

val str : symbol -> string

val compare : symbol -> symbol -> int

