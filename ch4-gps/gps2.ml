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

(** The type for a state condition *)
type statecond = Symbol of Symbol.symbol | Exec of string | Start

(** A set of state conditions *)
module CondSet = Set.Make(struct type t = statecond
                                 let compare = compare end)

(*** Some auxiliary functions ***)
let remove i l = List.fold_right (fun x xs -> if x = i then xs else x :: xs) l []

let union l1 l2 = List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) l1 l2

let diff l1 l2 = List.fold_right (fun x xs -> remove x xs) l2 l1

let rec find_op fn l = match l with
    [] -> []
  | i :: ls -> (match fn i with [] -> find_op fn ls | r -> r)

let subsetp l1 l2 = List.for_all (fun x -> List.mem x l2) l1

let remove_if fn l = List.filter (fun x -> not (fn x)) l

(*** Debug code from Section 4.10 ***)
let dbg_ids = ref []

let debug ids = 
  dbg_ids := union ids !dbg_ids

let undebug ids = 
  match ids with
      [] -> dbg_ids := []
    | _ -> dbg_ids := diff !dbg_ids ids

let dbg id str =
  if List.mem id !dbg_ids then 
    print_endline str
  else ()

let dbg_indent id indent str = 
  if List.mem id !dbg_ids then
    print_endline ((String.make (3 * indent) ' ') ^ str)
  else ()


(*** GPS code from Section 4.11 ***)

(** Type for operations *)
type op = { action   : string; 
            preconds : Symbol.symbol list;
            add_list : statecond list;
            del_list : statecond list  }


let appropriate_p goal op = List.mem (Symbol goal) op.add_list

let rec achieve_all state goals goal_stack ops = 
  let new_state = List.fold_left 
    (fun s g -> match s with [] -> [] | _ -> achieve s g goal_stack ops) state goals in
  match new_state with
      [] -> []
    | _ -> if subsetp (List.map (fun s -> Symbol s) goals) new_state then new_state else []  
and achieve state goal goal_stack ops = 
  dbg_indent "gps" (List.length goal_stack) ("Goal: " ^ Symbol.str goal);
  if List.mem (Symbol goal) state then state
  else if List.mem goal goal_stack then []
  else let appropriate_goals = List.filter (appropriate_p goal) ops in
       find_op (fun op -> apply_op state goal op goal_stack ops) appropriate_goals
and apply_op state goal op goal_stack ops = 
  dbg_indent "gps" (List.length goal_stack) ("Consider: " ^ op.action);
  match achieve_all state op.preconds (goal :: goal_stack) ops with
      [] -> []
    | state2 -> 
      dbg_indent "gps" (List.length goal_stack) ("Action: " ^ op.action); 
      (remove_if (fun x -> List.mem x op.del_list) state2) @ op.add_list
      

(********** Change frontier *****)
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
