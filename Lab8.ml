module type Associaty = sig
  (* expose the constructors so the autograder can pattern match on them *)
  type ('k, 'v) t = Empty | Pair of 'k * 'v * ('k, 'v) t
  exception NoSuchKey
  val delete         : ('k, 'v) t -> 'k -> ('k, 'v) t
  val get            : ('k, 'v) t -> 'k -> 'v
  val make           : unit -> ('k, 'v) t
  val put            : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (* extra stuff I added *)
  val mem            : ('k, 'v) t -> 'k -> bool
  val size           : ('k, 'v) t -> int
  val to_list        : ('k, 'v) t -> ('k * 'v) list
  val of_list        : ('k * 'v) list -> ('k, 'v) t
  val update         : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
  val get_or_default : ('k, 'v) t -> 'k -> 'v -> 'v
end

module Association : Associaty = struct

  type ('k, 'v) t =
    | Empty
    | Pair of 'k * 'v * ('k, 'v) t

  exception NoSuchKey

  let error () = raise NoSuchKey

  let make () = Empty

  (* put: prepend to front -- O(1) *)
  let put key value pairs = Pair (key, value, pairs)

  (* check if a key exists before trying to get/delete it *)
  let rec mem pairs key =
    match pairs with
    | Empty -> false
    | Pair (k, _, rest) -> k = key || mem rest key

  (* get: tail-recursive search left-to-right *)
  let rec get pairs key =
    match pairs with
    | Empty -> error ()
    | Pair (k, v, rest) ->
      if k = key then v
      else get rest key

  (* delete: removes the first matching key, not tail-recursive *)
  (* double-checking with mem first feels safer even if it's a bit redundant *)
  let rec delete pairs key =
    if not (mem pairs key) then error ()
    else
      match pairs with
      | Empty -> error ()  (* shouldn't reach here but just in case *)
      | Pair (k, v, rest) ->
        if k = key then rest
        else Pair (k, v, delete rest key)

  (* how many pairs are stored -- O(n), useful for debugging *)
  let rec size pairs =
    match pairs with
    | Empty -> 0
    | Pair (_, _, rest) -> 1 + size rest

  (* dump everything into a plain list for printing/debugging *)
  let rec to_list pairs =
    match pairs with
    | Empty -> []
    | Pair (k, v, rest) -> (k, v) :: to_list rest

  (* build an association list from a plain list, left to right *)
  (* note: later entries in the input list end up at the front, so
     order is reversed -- could fix this with List.rev but leaving it
     for now since it doesn't matter for lookup correctness *)
  let of_list lst =
    List.fold_left (fun acc (k, v) -> put k v acc) (make ()) lst

  (* update: delete the old binding then insert the new one *)
  (* raises NoSuchKey if the key isn't already in the list *)
  let update pairs key new_val =
    let trimmed = delete pairs key in
    put key new_val trimmed

  (* get_or_default: like get but returns a fallback instead of raising *)
  let get_or_default pairs key default =
    if mem pairs key then get pairs key
    else default

end