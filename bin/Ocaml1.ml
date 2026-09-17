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

(* 
Problem 1
val get_nth : string list * int -> string option 
test run: get_nth(["a"; "b"; "c"], 2);; 
*)
let get_nth ((string_list : string list), (i : int)) : string option =
  if i < 1 || i >= List.length string_list + 1 then
    None
  else
    Some (List.nth string_list (i - 1))

(* 
Problem 2
val cumulative_sum : int list -> int list 
test run: cumulative_sum [12; 27; -13];;
*)
let cumulative_sum (old_list : int list) : int list option = 
  let rec func2 ((sum: int), (old_list: int list)) : int list =
    match old_list with
    | [] -> []
    | a :: b -> 
      let new_sum = sum + a in
      new_sum :: func2 (new_sum, b)
  in
  Some (func2 (0, old_list))


(* Problem 3
val search : int list -> int -> int option 
Implementation notes:
One function that takes in the inputs, 
then have it call a second function with int option
that iterates through the list with an increment, 
if the head = the n value, then it returns index, otherwise it increases index. 
If the tail is empty (no index found) it returns null/ *)
let search ((xs: int list), (n: int)) : int option = 
  let rec increment ((xs: int list), (n: int), (increment: int)) : int option =
    if List.length xs = 0 then
      None
    else
      if xs.hd = n then
        Some(increment)
      else
        let xs_new = xs.tl in
        increment(xs_new, n, (increment + 1)) 
    in
    Some(increment(xs, n, 0))
      

(* Problem 4
val sequence : int -> int -> int -> int list *)

(* Problem 5
val string_append_map : string list -> string -> string list *)
