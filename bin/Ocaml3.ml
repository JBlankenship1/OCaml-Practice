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
Implement isSubsetOpt that computes subset but in O(n log n) time or better. *)

(* Problem 2
Define an ADT TreeNode 'a that is a binary tree (is a leaf with just a value or a node that has a left, right, and value). 
Using this definition implement the function treeReduce, which takes a reduce function, an init value, a tree and performs an *in order* reduction on the tree.
Using treeReduce and *currying* -- define the functions treeCount and treeSum respectively which return the number of leaves/nodes and sum of all values respectively. *)

(* Problem 3
Implement a Fibonacci number calculation function fib as a *non* tail-recursive function and (if your version of OCaml is less than 5.0) what value of n causes an Out-of-Stack?
Implement a Fibonacci number calculation function tailFib as a tail-recursive function check that it does not hit an OOS condition for the n above. *)