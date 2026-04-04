open Lazy ;;

(* The type of a lazy list. Both head and tail are lazies so neither is
   evaluated until explicitly forced. *)
type 'a lazyList =
  | LazyEmpty
  | LazyNode of 'a Lazy.t * 'a lazyList Lazy.t ;;

(* Raised when lazyHead, lazyTail, or lazyTake get an empty lazy list. *)
exception LazyListError ;;

(* Build a lazy list node from a lazy head H and a lazy tail T. *)
let lazyCons h t = LazyNode (h, t) ;;

(* Return the evaluated head of L, or raise LazyListError if L is empty. *)
let lazyHead l =
  match l with
  | LazyEmpty        -> raise LazyListError
  | LazyNode (h, _) -> force h ;;

(* Return the evaluated tail of L, or raise LazyListError if L is empty. *)
let lazyTail l =
  match l with
  | LazyEmpty        -> raise LazyListError
  | LazyNode (_, t) -> force t ;;

(* Return the first N elements of L as an ordinary list.
   Raise LazyListError if L has fewer than N elements.

   lazyTail is called before lazyHead so that deeper elements
   are forced first. This matches the expected print order in the tests
   (e.g. "Computed integer 3 / 2 / 1" for lazyTake l 3). *)
let rec lazyTake l n =
  if n = 0
  then []
  else
    let t    = lazyTail l    in   (* force tail lazy — no head printed yet *)
    let rest = lazyTake t (n - 1) in   (* recurse deeper first             *)
    (lazyHead l) :: rest ;;            (* force head last, prepend          *)