(* Jacob Blankenship, OCaml1*)
(* opam exec -- dune build *)
(* opam exec -- dune exec ocaml_practice *)
(* Command to disable AI Copilot: ctrl+shift+p *)

(* Example code so see if Opam Switch is activated*)
let a: int =10 ;;
let () = print_int a ;;
let b = a + 23 ;;
let c = 2 * b ;;
let()= print_int c ;;

(* Problem 1
val get_nth : string list * int -> string option *)
let get_nth ((string_list : string list), (i : int)) : string option =
  if i < 1 || i >= List.length string_list + 1 then
    None
  else
    Some (List.nth string_list (i - 1))

(* Problem 2
val cumulative_sum : int list -> int list *)
let cumulative_sum : (int_list


(* Problem 3
val search : int list -> int -> int option *)

(* Problem 4
val sequence : int -> int -> int -> int list *)

(* Problem 5
val string_append_map : string list -> string -> string list *)
