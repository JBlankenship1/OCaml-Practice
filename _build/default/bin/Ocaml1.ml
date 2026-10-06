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
val get_nth : string list * int -> string option *)
(*Solution: Function itereates through the list, if the position is equal to i, return the string in
  that position. Returns None if int position is not present.*)
(* get_nth(["a"; "b"; "c"], 2);; *)

let get_nth ((string_list : string list), (i : int)) : string option =
  if i < 1 || i >= List.length string_list + 1 then
    None
  else
    Some (List.nth string_list (i - 1))

(* 
Problem 2
val cumulative_sum : int list -> int list *)
(* Solution: A recursively defined function that removes the head of the list, adds sum to it,
and then add the updated head to the list. Increases sum by heads value before recalling. Base
case starts with sum at 0.*)
(* cumulative_sum [12; 27; -13];;*)
let cumulative_sum (old_list : int list) : int list = 
  let rec func2 ((sum: int), (old_list: int list)) : int list =
    match old_list with
    | [] -> []
    | head :: tail -> 
      let new_sum = sum + head in
      new_sum :: func2 (new_sum, tail)
  in
  func2 (0, old_list)


(* Problem 3
val search : int list -> int -> int option *)
(* Solution: A recursively defined function that removes the head of the list. If the head equals n,
return increment, otherwise recall with increment + 1. If the tail is empty (no index found) 
it returns null (not founc). *)
(* search([1; 3; 5; 3; 1], 5);; *)

let search (xs: int list) (n: int) : int option =
  let rec incrementfunc ((xs: int list), (increment: int)) : int option =
    match xs with
        | [] -> None
        | head :: tail ->
            if head = n then
              Some(increment)
            else
              incrementfunc(tail, (increment + 1))
      in
    incrementfunc(xs, 0)
      

(* Problem 4
val sequence : int -> int -> int -> int list *)
(*Solution: Create a recursively defined function that first checks if our current increment
  is greater than high. If it is, return our list (base case []). If it isnt, append current_num
to the back of the list, and increase current_num by spacing.*)
(* sequence(2,0,8);;*)
let sequence(spacing: int) (low: int) (high: int) : int list =
  let rec incrementfunc((int_list: int list), (current_num: int)) : int list = 
    if current_num > high then
      int_list
    else
      let new_list = int_list @ [current_num] in
    incrementfunc(new_list, (current_num + spacing))
    in
  incrementfunc([], low)


(* Problem 5
val string_append_map : string list -> string -> string list *)
(*Solution: Create a recursively defined function that removes the head of our str_list
and addes it onto new_list with the appended concatenated string. If str_list is empty,
reverse new_list before returning.*)
(* string_append_map(["a";"b";"c"], "1");;  *)
let string_append_map(str_list: string list) (concat: string): string list =
  let rec incrementfunc((str_list: string list), (new_list: string list)) =
      match str_list with
        | [] -> List.rev(new_list)
        | head :: tail ->
          let new_list = (head ^ concat) :: new_list in
        incrementfunc(tail, new_list) in
  incrementfunc(str_list, [])
