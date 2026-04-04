(* Remove the first occurrence of THING from THINGS. If THING doesn't appear,
   return THINGS unchanged. *)
let rec allbut things thing =
  match things with
  | []     -> []
  | h :: t -> if h = thing
              then t
              else h :: allbut t thing ;;

(* Call the continuation ETC on each element of THINGS, in order. *)
let rec choose etc things =
  match things with
  | []     -> ()
  | h :: t -> etc h ; choose etc t ;;

(* Helper: ETC is the continuation, PERMUTED_THINGS is the prefix built so far
   (in reverse), UNPERMUTED_THINGS is what still needs to be placed. *)
let rec permuting etc permutedThings unpermutedThings =
  match unpermutedThings with
  | [] -> etc permutedThings
  | _  ->
      choose
        (fun thing ->
          permuting etc
            (thing :: permutedThings)
            (allbut unpermutedThings thing))
        unpermutedThings ;;

(* Generate all permutations of THINGS, calling ETC on each one. *)
let permute etc things =
  permuting etc [] things ;;