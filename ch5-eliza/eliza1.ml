(* 
 *
 * eliza1.ml
 * Implementation of the ELIZA chat program
 * 
 * From Chapter 5 of the book
 * "Paradigms of Artificial Intelligence Programming" by Peter Norvig
 * 
 * Andrei de Araújo Formiga, 2011-04-29
 * 
 *)

(* Compile with ocamlc -o eliza -I .. ../util.cmo eliza1.ml *)

(** Indicates pattern matching failure *)
let fail = []

(** Indicates matching success, with no variables *)
let no_bindings = [("", "")]

(** Is s a variable (a string beginning with '?')? *)
let variable_p s = (s.[0] = '?')

(** Find a (var, value) pair in a binding list *)
let get_binding var bindings = List.assoc var bindings

(** Add a (var, value) pair to a binding list *)
let extend_bindings var value bindings = 
  if bindings = fail then [(var, value)] 
  else (var, value) :: bindings

(** Effects substitution in lst given a binding list.
    Raises Not_found if the pattern contains variables without any bindings. *)
let sublis bindings lst = 
  let subst i = if variable_p i then get_binding i bindings else i in
  List.fold_right (fun i ls -> subst i :: ls) lst []

(** Is this a segment matching pattern: a string beginning with ?* *)
let segment_pattern_p s = (s.[0] = '?' && s.[1] = '*')

(* 
   Segment variables are different from normal variables; this may be a problem if the 
   rule or pattern requires that a variable and a segment variable of the same name 
   (eg. ?X and ?*X) have matching bindings. This pattern matching apparatus will not 
   guarantee it, while the original Common Lisp code would. 
*)

(** Does var match input? Uses (or updates) and returns bindings *)
let match_variable var input bindings = 
  try
    let binding = get_binding var bindings in
    if input = binding then bindings else fail
  with Not_found -> extend_bindings var input bindings

(** Match pattern pat against input. 
    If pat is not a segment pattern, match pats with the remaining input. *)
let rec match_patterns pat pats input bindings = 
  match input with
      _ when segment_pattern_p pat -> segment_match pat pats input bindings 0
    | i :: ri when variable_p pat -> 
         let b2 = match_variable pat i bindings in
         pat_match pats ri b2
    | i :: ri when pat = i -> pat_match pats ri bindings 
    | _ -> fail
(** Match pattern against input in the context of the bindings. *)
and pat_match pattern input bindings = 
  if bindings = fail then fail
  else match pattern with
      [] -> if input = [] then bindings else fail
    | p :: ps -> match_patterns p ps input bindings
(** Match the segment pattern var against input. *)
and segment_match var pats input bindings start = 
  match pats with
      [] -> match_variable var (Util.join input) bindings
    | p :: ps -> 
      try 
        let pos = Util.position p input start in  (* assume no 2 vars occur in pattern in sequence *) 
        let b' = match_variable var (Util.join (Util.take input pos)) bindings in
        let b2 = pat_match pats (Util.drop input pos) b' in
        if b2 = fail then segment_match var pats input bindings (pos+1)
        else b2
      with Not_found -> fail

(** Type for ELIZA rules. *)
type rule = { pattern: string list; responses: string list list }

(** Helper function to create rules. *)
let mkrule ~pattern ~responses = 
  { pattern   = Util.split pattern;
    responses = List.map Util.split responses }

(** Simple set of rules for ELIZA as seen in Section 5.4 *)
let eliza_rules = 
  [ 
    mkrule ~pattern:"?*X hello ?*Y"
      ~responses:["How do you do. Please state your problem."];

    mkrule ~pattern:"?*X I want ?*Y"
      ~responses:["What would it mean if you got ?*Y"; 
                  "Why do you want ?*Y";
                  "Suppose you got ?*Y soon"];

    mkrule ~pattern:"?*X if ?*Y"
      ~responses:["Do you really thing it's likely that ?*Y";
                  "Do you wish that ?*Y";
                  "What do you think about ?*Y";
                  "Really-- if ?*Y"];

    mkrule ~pattern:"?*X no ?*Y"
      ~responses:["Why not?"; "You are being a bit negative"; 
                  "Are you saying NO just to be negative?"];

    mkrule ~pattern:"?*X I was ?*Y"
      ~responses:["Were you really?"; "Perhaps I already knew you were ?*Y";
                  "Why do you tell me you were ?*Y now?"];

    mkrule ~pattern:"?*X I feel ?*Y"
      ~responses:["Do you often feel ?*Y"];

    mkrule ~pattern:"?*X I felt ?*Y"
      ~responses:["What other feelings do you have?"] 
  ]

let default_eliza_rule = 
  mkrule ~pattern:"" ~responses:["Tell me more about this";
                                 "You were saying...";
                                 ]

(** Change I to you and vice versa, and so on. *)
let switch_viewpoint bindings = 
  let word_map w = match w with
      "I" -> "you" | "you" -> "I" 
    | "me" -> "you" | "am" -> "are" | _ -> w in
  List.map (fun (v, w) -> (v, word_map w)) bindings

(** Find some rule with which to transform the input. *)
let use_eliza_rules input = 
  let rec find_rule rules = match rules with
      [] -> (fail, default_eliza_rule)
    | r :: rs -> let res = pat_match r.pattern input no_bindings in
                 if res = fail then find_rule rs else (res, r) in
  let bindings, rule = find_rule eliza_rules in
  if bindings = fail then (Util.random_elt rule.responses)
  else sublis (switch_viewpoint bindings) (Util.random_elt rule.responses)

let eliza () = 
  let rec loop () = 
    let _ = print_string "eliza> " in
    let input = read_line () in
    print_endline (Util.join (use_eliza_rules (Util.split input)));
    loop () in
  loop ()

let _ = eliza ()
