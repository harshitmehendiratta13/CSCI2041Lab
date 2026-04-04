(* Plain recursive binomial coefficient — no memoization.
   C(n,k) = 1          if k = 0
   C(n,k) = 0          if k ≠ 0 and n = 0
   C(n,k) = C(n-1,k) + C(n-1,k-1)   otherwise *)
let rec c n k =
  if k = 0 then 1
  else if n = 0 then 0
  else c (n - 1) k + c (n - 1) (k - 1) ;;

(* Memoized version.  The hash table is created exactly once, when memyC is
   first defined, and is captured in the closure.  It is invisible to any
   code outside this definition. *)
let memyC =
  let table = Hashtbl.create ~random:false 1000 in
  let rec memyC' n k =
    match Hashtbl.find_opt table (n, k) with
    | Some v -> v
    | None   ->
        let v =
          if k = 0 then 1
          else if n = 0 then 0
          else memyC' (n - 1) k + memyC' (n - 1) (k - 1)
        in Hashtbl.add table (n, k) v ;
           v
  in memyC' ;;