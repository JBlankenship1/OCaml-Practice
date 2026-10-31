(* Jacob Blankenship, OCaml3*)
(* opam exec -- dune build *)
(* Command to disable AI Copilot: ctrl+shift+p *)

(* Example code so see if Opam Switch is activated*)
let a: int =10 ;;
let () = print_int a ;;
let b = a + 23 ;;
let c = 2 * b ;;
let()= print_int c ;;

(* Top level bindings:
val isSubset : list int -> list int -> bool
val isSubsetOpt : list int -> list int -> bool
val treeReduce : ('acc -> a' -> 'acc) -> 'acc -> TreeNode 'a -> 'acc 
val treeCount : TreeNode 'a -> int
val treeSum : TreeNode int -> int  
val fib: int -> int
val tailFib: int -> int *)

(* Problem 1
Implement isSubset that takes 2 lists of integers and returns true if the first list is a subset of the second list.
Implement isSubsetOpt that computes subset but in O(n log n) time or better. 
Solution 1: Simple double recursion with a helper function that itereates through each element in list B, if any element in
  list A isn't in list b, return false.
Solution 2: Converting list b inst a set, which removes duplicates so that unneded elements need not be checked, then
  itereates through list A to find if element is present in set B, done with a module called IntSet*)

(* isSubset [1; 2; 3] [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] ;; *)
(* isSubset [30; 124431; 24320] [124432; 24320; 30];; *)
(* isSubsetOpt [1; 2; 3] [1; 2; 3; 4; 5; 6; 7; 8; 9; 0] ;; *)
(* isSubsetOpt [30; 124431; 24320] [124432; 24320; 30];; *)
let rec contains (x: int) (lst: int list): bool =
    match lst with
    | [] -> false
    | head :: tail ->
        if head = x then
            true
        else
            contains x tail
let rec isSubset (a: int list) (b: int list): bool =
    match a with
    | [] -> true
    | head :: tail ->
        if contains head b then
            isSubset tail b
        else
            false

module IntSet = Set.Make(Int)
let isSubsetOpt (a : int list) (b : int list) : bool =
  let bSet = IntSet.of_list b in
  let rec check (a : int list) : bool =
    match a with
    | [] -> true
    | head :: tail ->
      if IntSet.mem head bSet then 
        check tail
      else 
        false
  in
  check a


(* Problem 2
Define an ADT TreeNode 'a that is a binary tree (is a leaf with just a value or a node that has a left, right, and value). 
Using this definition implement the function treeReduce, which takes a reduce function, an init value, a tree and performs an 
*in order* reduction on the tree.
Using treeReduce and *currying* -- define the functions treeCount and treeSum respectively which return the number of leaves/nodes 
and sum of all values respectively. 
Solution: treeReduce applies our inputted function to each of the nodes in the tree. In treeSum, the function is an additive increment
and for treeCount it is a simple incrementer that increases by one for each node. Trees are a custom type 'a treeNode, which 
can either be a leaf of a value (treeSum requires it to be an int), or a combination of a left branch, then nodes value, and a right
branch, with the left and right branch also having type treeNode. *)
(* treeReduce add 0 tree1 ;; *)
(* treeSum tree2 ;; *) 
(* treeCount tree1 ;; *)
type 'a treeNode =
  | Leaf of 'a
  | Node of 'a treeNode * 'a * 'a treeNode

let tree1 = 
  Node (Node (Leaf 7, 3, Leaf 9), 
        5, 
        Leaf 1)
let tree2 =
  Node (Node (Leaf 2, 4, Leaf 0),
        7,
        Node (Leaf 18, 3, Leaf 9))

let rec treeReduce (func : 'b -> 'a -> 'b) (init : 'b) (tree : 'a treeNode) : 'b =
  match tree with
  | Leaf v -> func init v
  | Node (l, v, r) ->
    let accL = treeReduce func init l in
    let accV = func accL v in
  treeReduce func accV r

let add acc v = acc + v

let treeSum = treeReduce add 0
let treeCount t = treeReduce (fun acc _ -> acc + 1) 0 t



(* Problem 3
Implement a Fibonacci number calculation function fib as a *non* tail-recursive function and (if your version of OCaml is less 
  than 5.0) what value of n causes an Out-of-Stack?
Implement a Fibonacci number calculation function tailFib as a tail-recursive function check that it does not hit an OOS 
condition for the n above. 
Solution 1: A non tail recursive has no carry-one variable, it is much slower and involves decrementing our n value by branching
into 2 seperate fib calls until we reach 1, before combining all branches to a final value of the nth-1 + nth-2 Fibonacci
Solution 2: Using a tail-recursive function we can iterate upwards with 2 integers that pass our variables along, this is much
faster. A ceiling seems to be made at n = 91, which causes the sign of our sequence to flip, probably because of a 63 bit sign
int flip. I am running ocaml 5.4.1, so the Out-of-Stack error is not applicable.*)

(* fib 10 ;; *)
(* tailFib 90 ;; *)
(* tailFib 91 ;; *)

let rec fib (n : int) : int =
  if (n <= 1) then 
    n
  else
    fib (n-1) + fib (n-2)


let tailFib (n : int) : int =
  let rec fibIncr (n : int) (a : int) (b : int) : int =
    if n = 0 then
      a
    else
      fibIncr (n-1) b (a+b) in
  fibIncr n 0 1
  

