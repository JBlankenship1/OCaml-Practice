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
and sum of all values respectively. *)



(* Problem 3
Implement a Fibonacci number calculation function fib as a *non* tail-recursive function and (if your version of OCaml is less 
  than 5.0) what value of n causes an Out-of-Stack?
Implement a Fibonacci number calculation function tailFib as a tail-recursive function check that it does not hit an OOS 
condition for the n above. *)
let rec fib (n : int) : int =
  if (n <= 1) then 
    n
  else
    fib (n-1) + fib (n-2)


let tailFib (n : int) : int =
  let rec fibIncr (n : int) (a : int) (b : int) =
    if (n = 1) then
      a
    else
      fibIncr (n-1) b a+b in
  fibIncr n 0 1
  

