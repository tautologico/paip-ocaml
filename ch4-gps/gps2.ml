(* 
 *
 * gps2.ml
 * Implementation of the GPS version 2
 * 
 * From Chapter 4, Section 4.11 of the book
 * "Paradigms of Artificial Intelligence Programming" by Peter Norvig
 * 
 * Andrei de Araújo Formiga, 2011-04-16
 * 
 *)

(* 

We need to form lists [executing; stuff] with actions, so maybe we'll 
need to declare actions as symbols after all.   

*)

(* Compile with ocamlc -I .. ../symbol.mli ../symbol.ml gps1.ml *)

(** A set of symbols *) 
module SymbolSet = Set.Make(struct type t = Symbol.symbol
                                   let compare = Symbol.compare end)

(** The empty set of symbols *)
let e = SymbolSet.empty

(** The type for state elements (a "state point") *)
type statept = Symbol of Symbol.symbol | Exec of string


(*** GPS code from Section 4.11 ***)

(** Type for operations *)
type op = { action   : string; 
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
    print_endline ("Executing: " ^ op.action);
    state := SymbolSet.diff !state op.del_list;
    state := SymbolSet.union !state op.add_list;
    true
  )
  else 
    false

let gps st goals ops = 
  let _ = state := st in
  List.for_all (achieve ops) goals


(*** Testing code from Section 4.4 ***)

(* symbol definitions *)
let son_at_home        = Symbol.create "son at home"
let son_at_school      = Symbol.create "son at school"
let car_works          = Symbol.create "car works"
let car_needs_battery  = Symbol.create "car needs battery"
let shop_knows_problem = Symbol.create "shop knows problem"
let shop_has_money     = Symbol.create "shop has money"
let know_phone_number  = Symbol.create "know phone number"
let have_phone_book    = Symbol.create "have phone book"
let have_money         = Symbol.create "have money"
let in_communication_with_shop = Symbol.create "in communication with shop"

(* Helper function for creating symbol sets *) 
let symbols l = List.fold_right SymbolSet.add l e
