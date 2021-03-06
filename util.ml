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


(** Join a list of strings into a single string, using spaces as separators. *)
let rec join sl = match sl with
    [] -> ""
  | [s] -> s
  | s :: sl' -> s ^ " " ^ join sl'

(** Drops n items from lst. *)
let rec drop lst n = 
  match lst, n with
      (_, 0) -> lst
    | ([], _) -> raise Not_found
    | (x :: xs, _) -> drop xs (n-1)

(** Take the first n items from lst. *)
let rec take lst n = 
  match lst, n with
      (_, 0) -> []
    | ([], _) -> raise Not_found
    | (x :: xs, _) -> x :: take xs (n-1)

(** Find the position of i in lst, starting at index start. Raises Not_found if i is not in lst. *)
let position i lst start = 
  let rec pos_aux lst c = 
    match lst with
        [] -> raise Not_found
      | l :: ls when i = l -> c
      | _ :: ls -> pos_aux ls (c+1) in
  (pos_aux (drop lst start) 0) + start

(** Select a random element from a list. *)
let random_elt lst = 
  let l = List.length lst in
  let i = Random.int l in 
  List.nth lst i

(** A string formed by n repetitions of string s. *)
let repstr s n =
  let rec loop i acc = 
    if i = 0 then acc else loop (i-1) (acc ^ s) in
  loop n ""

(** Concatenate n repetitions of s, separated by sep. *)
let repsep s n sep = 
  let rec loop i acc = 
    if i = 1 then acc ^ s else loop (i-1) (s ^ sep ^ acc) in
  loop n ""

(** Concatenate all strings in ss, separated by sep. *)
let rec concatsep ss sep = 
  match ss with 
      [] -> "" 
    | [s] -> s 
    | s :: rss -> s ^ sep ^ (concatsep rss sep)
