type
  thing =
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string ;;

(* Returns true if predicate holds for every element in the list.
   Empty list returns true since nothing can fail. *)
let rec every predicate elements =
  match elements with
  | Cons (h, t) ->
      if predicate h
      then every predicate t
      else false
  | _ -> true ;;

(* Walks through elements and replaces any top-level match of old with
   replacement. Doesn't look inside nested lists, just the top level.
   "new" is a keyword in OCaml so we use "replacement" instead. *)
let rec substitute elements old replacement =
  match elements with
  | Cons (h, t) ->
      Cons
        ((if h = old then replacement else h),
         substitute t old replacement)
  | _ -> Nil ;;

(* Like structural equality, but "?" in left matches anything on the right.
   Checks car and cdr recursively when both sides are Cons nodes. *)
let rec questyEqual left right =
  left = Symbol "?" ||
  (match (left, right) with
   | (Cons (lh, lt), Cons (rh, rt)) ->
       questyEqual lh rh && questyEqual lt rt
   | _ ->
       left = right) ;;