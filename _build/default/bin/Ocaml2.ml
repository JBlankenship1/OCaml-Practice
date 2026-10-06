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
(*Solution: *)
(*  *)
let rec add (a : nat) (b : nat) : nat =
  match a with
  | Zero -> b
  | Succ n -> Succ (add n b)


(* 
Problem 2
val to_int : nat -> int 
val from_int : int -> nat *)
(* Solution: *)
(* *)

(* Problem 3
val evaluate: expr -> int *)
(* Solution: *)
(*  *)


(* Problem 4
val filtermap: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list *)
(*Solution: *)
(*  *)


(* Problem 5
val makesay string -> (string -> string) *)
(*Solution: *)
(*  *)
