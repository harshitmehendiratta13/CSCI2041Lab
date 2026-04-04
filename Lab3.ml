(* Exception for empty BST case in bstMaxKey *)
exception BadEmptyBst ;;

(* BST type definition *)
type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;

(* Returns the maximum key in a BST by traversing right as far as possible *)
let rec bstMaxKey tree =
  match tree with
  | BstEmpty -> raise BadEmptyBst
  | BstNode(k, _, BstEmpty) -> k
  | BstNode(_, _, r) -> bstMaxKey r ;;

(* Deletes a node with the given key from a BST, preserving the BST property *)
let rec bstDelete tree key =
  match tree with

  (* Case 1: Deleting from an empty BST — nothing to do *)
  | BstEmpty -> BstEmpty

  | BstNode(k, l, r) ->
      if key < k then
        (* Key is in the left subtree — recurse left, rebuild node *)
        BstNode(k, bstDelete l key, r)
      else if key > k then
        (* Key is in the right subtree — recurse right, rebuild node *)
        BstNode(k, l, bstDelete r key)
      else
        (* key = k: this is the node to delete *)
        match (l, r) with

        (* Case 2: No children — just remove the node *)
        | (BstEmpty, BstEmpty) -> BstEmpty

        (* Case 3: Only right child — replace with right subtree *)
        | (BstEmpty, _) -> r

        (* Case 4: Only left child — replace with left subtree *)
        | (_, BstEmpty) -> l

        (* Case 5: Two children — replace key with left subtree's max,
           then delete that max from the left subtree *)
        | (_, _) ->
            let maxKey = bstMaxKey l in
            BstNode(maxKey, bstDelete l maxKey, r) ;;