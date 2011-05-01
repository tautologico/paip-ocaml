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

(** Effects substitution given a list and an assoc list. *)
let sublis bindings lst = 
  let subst i = if variable_p i then get_binding i bindings else i in  (* assume there's a binding for all vars *)
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
let match_single_pat pat input bindings = 
  match input with
      [] -> (fail, input)
    | i :: ri when segment_pattern_p pat -> (fail, input)
    | i :: ri -> 
      if variable_p pat then (match_variable pat i bindings, ri)
      else if pat = i then (bindings, ri) 
      else (fail, input)

(** Match pattern against input in the context of the bindings. *)
let rec pat_match pattern input bindings = 
  if bindings = fail then fail
  else match pattern with
      [] -> if input = [] then bindings else fail
    | p :: ps -> let bs, inp = match_single_pat p input bindings in
                 pat_match ps inp bs

let eliza () = ()
