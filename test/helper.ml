open Chess.Board
open Chess.Piece

(* Comparators *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are equivalent
    set-like lists. That means checking two things. First, they must both be {i set-like},
    meaning that they do not contain any duplicates. Second, they must contain the same
    elements, though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1 && List.length lst2 = List.length uniq2 && uniq1 = uniq2

(* Print Helpers *)
let id x = x

let position_printer (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to pretty-print each element
    of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* Board Moves *)
let rec move_times time (x, y) piece =
  match time with
  | 0 -> piece
  | _ -> move_times (time - 1) (x, y) (move_piece (x, y) piece)

let move_helper str1 str2 =
  let ch1 = String.get str1 0 in
  let ch2 = String.get str1 1 in
  let ch3 = String.get str2 0 in
  let ch4 = String.get str2 1 in
  move
    (7 - (Char.code ch2 - 49), Char.code ch1 - 97)
    (7 - (Char.code ch4 - 49), Char.code ch3 - 97)
