(* A stream of odd integers: 1, 3, 5, 7, ...
   State is unused since the next value is always this + 2. *)
let odds =
  makeStream 1 () (fun this state -> (this + 2, ())) ;;

(* Return a stream like STREAM, but with its first COUNT elements removed.
   Recurses COUNT times using rest, so runs in O(count) time. *)
let rec trim count stream =
  match count with
  | 0 -> stream
  | _ -> trim (count - 1) (rest stream) ;;

(* Return a new stream like STREAM but with every element multiplied by FACTOR.
   The state is the remaining un-scaled stream; next advances it by one step.
   Runs in O(1) time per element. *)
let scale factor stream =
  makeStream
    (factor * first stream)
    (rest stream)
    (fun this state -> (factor * first state, rest state)) ;;

(* Return a new stream whose nth element is the sum of the nth elements of
   LEFT and RIGHT. The state is a pair of the remaining streams.
   Runs in O(1) time per element. *)
let sum left right =
  makeStream
    (first left + first right)
    (rest left, rest right)
    (fun this (l, r) -> (first l + first r, (rest l, rest r))) ;;