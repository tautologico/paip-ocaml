(* 
 *
 * gps1.ml
 * Simple GPS implementation
 *
 * From Chapter 4, Sections 4.3 and 4.4 of the book
 * "Paradigms of Artificial Intelligence Programming" by Peter Norvig
 * Translated from the original code in Common Lisp
 * 
 * Andrei de Araújo Formiga, 2011-04-15
 * 
 *)

(* Compile with ocamlc -I .. ../symbol.mli ../symbol.ml gps1.ml *)

(** A set of symbols *) 
module SymbolSet = Set.Make(struct type t = Symbol.symbol
                                   let compare = Symbol.compare end)

(** The empty set of symbols *)
let e = SymbolSet.empty

(** The current state *)
let state = ref SymbolSet.empty


(** Type for operations *)
type op = { action   : Symbol.symbol; 
            preconds : SymbolSet.t;
            add_list : SymbolSet.t;
            del_list : SymbolSet.t   }

let appropriate goal op = SymbolSet.mem goal op.add_list

let rec achieve ops goal = 
  let appropriate_goals = List.filter (appropriate goal) ops in
  SymbolSet.mem goal !state || List.exists (apply_op ops) appropriate_goals
and apply_op ops op =  
  if SymbolSet.for_all (achieve ops) op.preconds then
  (
    print_endline ("Executing: " ^ Symbol.str op.action);
    state := SymbolSet.diff !state op.del_list;
    state := SymbolSet.union !state op.add_list;
    true
  )
  else 
    false

let gps st goals ops = 
  let _ = state := st in
  List.for_all (achieve ops) goals
