(* Jacob Blankenship, OCaml2*)
(* opam exec -- dune build *)
(* opam exec -- dune exec ocaml_practice *)
(* Command to disable AI Copilot: ctrl+shift+p *)

(* Example code so see if Opam Switch is activated*)
let a: int =10 ;;
let () = print_int a ;;
let b = a + 23 ;;
let c = 2 * b ;;
let()= print_int c ;;

(* Natural number initialization *)
type nat =
| Zero
| Succ of nat

(* 
Problem 1
val add : nat -> nat -> nat *)
(* Solution: Iterate through successor list with a match, using recursion to get to most nested pair.*)
(* add (Succ Zero) (Succ (Succ Zero));;*)
let rec add (a : nat) (b : nat) : nat =
  match a with
  | Zero -> b
  | Succ c -> Succ (add c b)

(* 
Problem 2
val to_int : nat -> int 
val from_int : int -> nat *)
(* Solution: to_int recursively iterates through the input, incrementing i until the all Succ have been reached
from_int is similar, but builds a successors list whilst decrementing i recursively. *)
(* to_int (Succ (Succ (Succ Zero)));; *)
(* from_int (5);; *)


let to_int (a : nat) : int =
  let rec inc_func ((natural : nat), (i : int)) =
    match natural with
    | Zero -> i
    | Succ natural -> inc_func(natural, (i + 1)) in
  inc_func(a, 0)

let from_int (a : int) : nat =
  let rec inc_func ((i : int), (natural : nat)) =
    if i = 0 then
      natural
    else
    inc_func((i-1), Succ(natural)) in
  inc_func(a, Zero)


(* Problem 3
val evaluate: expr -> int *)
(* Solution: After defining type with our parameters, we can iterate through the three operands, not stopping our 
recursions until reaching a literal. *)
(* evaluate(Mul (Mul (Mul (Literal 5, Literal 99), Literal 98), Literal 97));; *)

type expr =
  | Literal of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mul of expr * expr

let rec evaluate (ex : expr) : int =
  match ex with
  | Literal n -> n
  | Plus (left, right) -> (evaluate left) + (evaluate right)
  | Minus (left, right) -> (evaluate left) - (evaluate right)
  | Mul (left, right) -> (evaluate left) * (evaluate right)

(* Problem 4
val filtermap: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list *)
(* Solution: Iterate through list (stopping at the end of it). If the element in the list evaluates to true
with the filter, apply map to it and append it to the left as the tail is recalled.*)
(* filtermap (fun x -> x < 0) (fun x -> x + 100) [-10; 20; -30];; *)
let rec filtermap (filter : 'a -> bool) (map : 'a -> 'b) (list : 'a list) : 'b list =
  match list with
    | [] -> []
    | head:: tail ->
      if filter head then
        map head :: filtermap filter map tail
      else
        filtermap filter map tail


(* Problem 5
val makesay string -> (string -> string) *)
(* Solution: Simply append the two strings together with the ^ operand.*)
(* (makesay "hello") "world";; *)
let makesay (a : string) (b : string) : string =
  a ^ " " ^ b
