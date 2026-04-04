(* EVERY. Test if PREDICATE returns true for every element of the Lisp
   list ELEMENTS. Returns true vacuously for an empty list.

   Lisp:
     (define every
       (λ (predicate elements)
         (if (is-cons elements)
           (if (predicate (car elements))
             (every predicate (cdr elements))
             nil)
           t))) *)
let rec every predicate elements =
  match elements with
  | Cons (h, t) ->
      if predicate h
      then every predicate t
      else false
  | _ -> true ;;

(* SUBSTITUTE. Return a copy of the Lisp list ELEMENTS in which every
   top-level occurrence of OLD is replaced by REPLACEMENT.
   (Note: "new" is a reserved word in OCaml, so we use "replacement".)

   Lisp:
     (define substitute
       (λ (elements old new)
         (if (is-cons elements)
           (cons
             (if (= (car elements) old) new (car elements))
             (substitute (cdr elements) old new))
           nil))) *)
let rec substitute elements old replacement =
  match elements with
  | Cons (h, t) ->
      Cons
        ((if h = old then replacement else h),
         substitute t old replacement)
  | _ -> Nil ;;

(* QUESTY EQUAL. Test if LEFT and RIGHT are structurally equal, where
   the symbol "?" in LEFT acts as a wildcard matching anything in RIGHT.

   Lisp:
     (define questy-equal
       (λ (left right)
         (or
           (= left (quote ?))
           (and (is-cons left)
                (is-cons right)
                (questy-equal (car left) (car right))
                (questy-equal (cdr left) (cdr right)))
           (= left right)))) *)
let rec questyEqual left right =
  left = Symbol "?" ||
  (match (left, right) with
   | (Cons (lh, lt), Cons (rh, rt)) ->
       questyEqual lh rh && questyEqual lt rt
   | _ ->
       left = right) ;;