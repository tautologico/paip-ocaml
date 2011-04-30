(*
 * 
 * util.ml
 * Utility functions
 * 
 * Andrei de A. Formiga, 2011-04-29
 * 
 *)


(** Split a string into substrings using characters in string seps as separators. 
    Default separator is a space character. *)
let split ?(seps=" ") s = 
  let l = String.length s in
  let rec sep i = 
    if i >= l then [] else 
      if String.contains seps s.[i] then sep (i+1) else nonsep i 1
  and nonsep i j = 
    if (i+j) = l then [(i,j)] else 
      if String.contains seps s.[i+j] then (i,j) :: sep (i+j) else nonsep i (j+1) in
  List.map (fun (i, l) -> String.sub s i l) (sep 0)
