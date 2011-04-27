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

(* Compile with ocamlc -I .. ../symbol.mli ../symbol.ml gps1.ml *)

(** The type for a state condition *)
type statecond = Symbol of Symbol.symbol | Exec of string | Start

(*** Some auxiliary functions ***)
let remove i l = List.fold_right (fun x xs -> if x = i then xs else x :: xs) l []

let union l1 l2 = List.fold_right (fun x xs -> if List.mem x xs then xs else x :: xs) l1 l2

let diff l1 l2 = List.fold_right (fun x xs -> remove x xs) l2 l1

let rec find_op fn l = match l with
    [] -> []
  | i :: ls -> (match fn i with [] -> find_op fn ls | r -> r)

let subsetp l1 l2 = List.for_all (fun x -> List.mem x l2) l1

let remove_if fn l = List.filter (fun x -> not (fn x)) l

(** Injection function from symbols to state conditions *)
let symbols_cond l = List.map (fun s -> Symbol s) l

(** String representation of state conditions *)
let cond_to_str sc = match sc with
    Symbol s -> Symbol.str s
  | Exec s -> "Executing " ^ s
  | Start -> "Start State"

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
            preconds : statecond list;
            add_list : statecond list;
            del_list : statecond list  }

(** Build an op including the action as an executing state in add_list *)
let executing_p x = 
  match x with
    Exec _ -> true
  | _ -> false

(** Convert an operator to include the executing action in its add-list *)
let convert_op op = 
  if List.exists executing_p op.add_list then op
  else { op with add_list = (Exec op.action) :: op.add_list }

(** Constructor for operations *)
let op action ~preconds ~add_list ~del_list = 
  convert_op { action=action; preconds=symbols_cond preconds; 
               add_list=symbols_cond add_list; del_list=symbols_cond del_list }

let op2 action ~preconds ~add_list ~del_list = 
  convert_op { action=action; preconds=preconds; add_list=add_list; del_list=del_list }

(** An op is appropriate for a goal if it is in its add-list *)
let appropriate_p goal op = List.mem goal op.add_list

(** Achieve each goal, and make sure they still hold in the end *)
let rec achieve_all state goals goal_stack ops = 
  let new_state = List.fold_left 
    (fun s g -> match s with [] -> [] | _ -> achieve s g goal_stack ops) state goals in
  match new_state with
      [] -> []
    | _ -> if subsetp goals new_state then new_state else []  
(** A goal is achieved if it already holds, 
    or if there is an appropriate op for it that is applicable *)
and achieve state goal goal_stack ops = 
  dbg_indent "gps" (List.length goal_stack) ("Goal: " ^ cond_to_str goal);
  if List.mem goal state then state
  else if List.mem goal goal_stack then []
  else let appropriate_goals = List.filter (appropriate_p goal) ops in
       find_op (fun op -> apply_op state goal op goal_stack ops) appropriate_goals
(** Return a new, transformed state if op is applicable *)
and apply_op state goal op goal_stack ops = 
  dbg_indent "gps" (List.length goal_stack) ("Consider: " ^ op.action);
  match achieve_all state op.preconds (goal :: goal_stack) ops with
      [] -> []
    | state2 -> 
      dbg_indent "gps" (List.length goal_stack) ("Action: " ^ op.action); 
      (remove_if (fun x -> List.mem x op.del_list) state2) @ op.add_list

(** General Problem Solver: from state, achieve goals using ops *)
let gps state goals ops = 
  let final_state = achieve_all (Start :: state) (symbols_cond goals) [] ops in
  remove_if (function Exec _ -> false | _ -> true) final_state

(** General Problem Solver: from state, achieve goals using ops *)
let gpsl ~state ~goals ~ops = 
  gps state goals ops


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


let school_ops = List.map convert_op 
  [ { action = "Drive son to school";
      preconds = symbols_cond [son_at_home; car_works];
      add_list = symbols_cond [son_at_school];
      del_list = symbols_cond [son_at_home]; };
    { action = "Shop installs battery";
      preconds = symbols_cond [car_needs_battery; shop_knows_problem; shop_has_money];
      add_list = symbols_cond [car_works];
      del_list = [] };
    { action = "Tell shop the problem";
      preconds = symbols_cond [in_communication_with_shop];
      add_list = symbols_cond [shop_knows_problem];
      del_list = [] };
    { action = "Telephone shop";
      preconds = symbols_cond [know_phone_number];
      add_list = symbols_cond [in_communication_with_shop];
      del_list = [] };
    { action = "Look up number";
      preconds = symbols_cond [have_phone_book];
      add_list = symbols_cond [know_phone_number];
      del_list = [] };
    { action = "Give shop money";
      preconds = symbols_cond [have_money];
      add_list = symbols_cond [shop_has_money];
      del_list = symbols_cond [have_money] } ]

(** Example state for the "driving son to school" problem *)
let school_state = symbols_cond [son_at_home; car_needs_battery; have_money; have_phone_book]

(* 
   Example test:
   gpsl ~state:school_state ~goals:[son_at_school] ~ops:school_ops

   Try it with debug ["gps"]
*)


(*** Code for the monkey and bananas problem, Section 4.12 ***)

(* Symbols *)
let chair_at_middle_room = Symbol.create "chair at middle room"
let at_middle_room       = Symbol.create "at middle room"
let on_floor             = Symbol.create "on floor"
let at_bananas           = Symbol.create "at bananas"
let on_chair             = Symbol.create "on chair"
let chair_at_door        = Symbol.create "chair at door"
let at_door              = Symbol.create "at door"
let empty_handed         = Symbol.create "empty handed"
let has_ball             = Symbol.create "has ball"
let has_bananas          = Symbol.create "has bananas"
let hungry               = Symbol.create "hungry"
let not_hungry           = Symbol.create "not hungry"


let banana_ops = 
  [ op "climb on chair" 
       ~preconds:[chair_at_middle_room; at_middle_room; on_floor]
       ~add_list:[at_bananas; on_chair]
       ~del_list:[at_middle_room; on_floor];
    op "push chair from door to middle room"
       ~preconds:[chair_at_door; at_door]
       ~add_list:[chair_at_middle_room; at_middle_room]
       ~del_list:[chair_at_door; at_door];
    op "walk from door to middle room"
       ~preconds:[at_door; on_floor]
       ~add_list:[at_middle_room]
       ~del_list:[at_door];
    op "grasp bananas"
       ~preconds:[at_bananas; empty_handed]
       ~add_list:[has_bananas]
       ~del_list:[empty_handed];
    op "drop ball"
       ~preconds:[has_ball]
       ~add_list:[empty_handed]
       ~del_list:[has_ball];
    op "eat bananas"
       ~preconds:[has_bananas]
       ~add_list:[empty_handed; not_hungry]
       ~del_list:[has_bananas; hungry] ]


(** An example state for the monkey and bananas problem *)
let banana_state = symbols_cond [at_door; on_floor; has_ball; hungry; chair_at_door]

(* 
   Example test:
   gpsl ~state:banana_state ~goals:[not_hungry] ~ops:banana_ops

   Try it with debug ["gps"]
*)
