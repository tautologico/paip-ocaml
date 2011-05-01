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


(** Indicates pattern matching failure *)
let fail = []

(** Indicates matching success, with no variables *)
let no_bindings = [("", "")]

(** Is s a variable (a string beginning with '?')? *)
let variable_p s = (s.[0] = '?')

(** Find a (var, value) pair in a binding list *)
let get_binding var bindings = List.assoc var bindings

(** Get the value part of a single binding *)
let binding_val = snd (*** TODO: need it? ***)

(** Get the value part (for var) from a binding list *)
let lookup var bindings = (*** TODO: need it? ***)
  get_binding var bindings

(** Add a (var, value) pair to a binding list *)
let extend_bindings var value bindings = 
  match bindings with
      [("", "")] -> [(var, value)]   (*** Duplication! ***)
    | _ -> (var, value) :: bindings

(** Effects substitution in lst given a binding list.
    Raises Not_found if the pattern contains variables without any bindings. *)
let sublis bindings lst = 
  let subst i = if variable_p i then get_binding i bindings else i in
  List.fold_right (fun i ls -> subst i :: ls) lst []

(** Is this a segment matching pattern: a string beginning with ?* *)
let segment_pattern_p s = (s.[0] = '?' && s.[1] = '*')

(** Does var match input? Uses (or updates) and returns bindings *)
let match_variable var input bindings = 
  try
    let binding = get_binding var bindings in
    if input = binding then bindings else fail
  with Not_found -> extend_bindings var input bindings

(** Match a single pattern against input. Return remaining input and (possibly updated) bindings. *)
let rec match_patterns pat pats input bindings = 
  match input with
      [] -> fail
    | i :: ri when segment_pattern_p pat -> segment_match pat pats input bindings 0
    | i :: ri -> 
      if variable_p pat then 
        let b2 = match_variable pat i bindings in
        pat_match pats ri b2
      else if pat = i then pat_match pats ri bindings 
      else fail
(** Match pattern against input in the context of the bindings. *)
and pat_match pattern input bindings = 
  if bindings = fail then fail
  else match pattern with
      [] -> if input = [] then bindings else fail
    | p :: ps -> match_patterns p ps input bindings
(** Shumbawumba *)
and segment_match var pats input bindings start = 
  match pats with
      [] -> match_variable var (Util.join input) bindings
    | p :: ps -> 
      try 
        let pos = Util.position p input start in
        let b' = match_variable var (Util.join (Util.take input pos)) bindings in
        let b2 = pat_match pats (Util.drop input pos) b' in
        if b2 = fail then segment_match var pats input bindings (pos+1)
        else b2
      with Not_found -> fail

let eliza () = ()
