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


(*** GPS code from Section 4.3 ***)

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

(*
 * The GPS implementation in the book relies on the sequential order of iteration
 * of list elements to work well. As this implementation uses sets, and the 
 * order of iteration of set elements is not fixed, running the first example 
 * with this implementation will result in a different order of actions. And this order 
 * is not right: to give the shop the money, you should be in communication with 
 * the shop and the shop has to know the problem. Adding these preconditions to the 
 * last op makes it run as in the book. 
 *)
let school_ops = 
  [ { action = "Drive son to school";
      preconds = symbols [son_at_home; car_works];
      add_list = symbols [son_at_school];
      del_list = symbols [son_at_home]; };
    { action = "Shop installs battery";
      preconds = symbols [car_needs_battery; shop_knows_problem; shop_has_money];
      add_list = symbols [car_works];
      del_list = e };
    { action = "Tell shop the problem";
      preconds = symbols [in_communication_with_shop];
      add_list = symbols [shop_knows_problem];
      del_list = e };
    { action = "Telephone shop";
      preconds = symbols [know_phone_number];
      add_list = symbols [in_communication_with_shop];
      del_list = e };
    { action = "Look up number";
      preconds = symbols [have_phone_book];
      add_list = symbols [know_phone_number];
      del_list = e };
    (* The list of preconditions is different from the book (see above) *)
    { action = "Give shop money";
      preconds = symbols [in_communication_with_shop; shop_knows_problem; have_money];
      add_list = symbols [shop_has_money];
      del_list = symbols [have_money] } ]

(* state for the first example *)
let exstate1 = symbols [son_at_home; car_needs_battery; have_money; have_phone_book]
