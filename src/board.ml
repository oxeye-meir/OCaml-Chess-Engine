open Piece
(* Board representation: [[(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)];
   [(1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7)];
   [(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7)];
   [(3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7)];
   [(4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7)];
   [(5,0);(5,1);(5,2);(5,3);(5,4);(5,5);(5,6);(5,7)];
   [(6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7)];
   [(7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7)]] *)

type t = Piece.t list list

exception InvalidPos

(* Helper functions *)

(** Iterate through Board matrix and check if it's empty. *)
let get_piece board (x, y) =
  let row = List.nth board x in
  List.nth row y

let position_empty board (x, y) =
  let piece = get_piece board (x, y) in
  is_empty piece

let rec replace_piece piece y = function
  | [] -> raise InvalidPos
  | h :: t -> if y = 0 then piece :: t else h :: replace_piece piece (y - 1) t

let rec replace_row piece x y = function
  | [] -> raise InvalidPos
  | h :: t ->
      if x = 0 then replace_piece piece y h :: t else h :: replace_row piece (x - 1) y t

(* occupied_squares needs to filter out Empty pieces *)
let occupied_squares board = board |> List.flatten |> List.map (fun piece -> position piece)

let row_to_string row = List.fold_left (fun acc piece -> acc ^ get_name piece ^ "|") "|" row

let invalid_pos (x, y) = x < 0 || x > 7 || y < 0 || y > 7

(* [[rook b;knight b;bishop b;queen b;king b;bishop b;knight b;rook b]; [pawn b;pawn b;pawn
   b;pawn b;pawn b;pawn b;pawn b;pawn b]; [empty;empty;empty;empty;empty;empty;empty;empty];
   [empty;empty;empty;empty;empty;empty;empty;empty];
   [empty;empty;empty;empty;empty;empty;empty;empty];
   [empty;empty;empty;empty;empty;empty;empty;empty]; [pawn w;pawn w;pawn w;pawn w;pawn w;pawn
   w;pawn w;pawn w]; [rook w;knight w;bishop w;queen w;king w;bishop w;knight w;rook w]] *)
let rec empty_squares color x y lst =
  if y <= 7 then empty_squares (not color) x (y + 1) (init_piece "empty" color x y :: lst)
  else lst

let rec pawns color x y lst =
  if y <= 7 then pawns color x (y + 1) (init_piece "pawn" color x y :: lst) else lst

let backrank color x =
  [
    init_piece "rook" color x 0;
    init_piece "knight" color x 1;
    init_piece "bishop" color x 2;
    init_piece "queen" color x 3;
    init_piece "king" color x 4;
    init_piece "bishop" color x 5;
    init_piece "knight" color x 6;
    init_piece "rook" color x 7;
  ]

let init_board =
  [
    backrank true 0;
    pawns true 1 0 [];
    empty_squares false 2 0 [];
    empty_squares true 3 0 [];
    empty_squares false 4 0 [];
    empty_squares true 5 0 [];
    pawns false 6 0 [];
    backrank false 7;
  ]

let next_moves board piece =
  let possible_moves = valid_moves piece in
  let empty_check = position_empty board in
  List.filter empty_check possible_moves

let move board curr_pos new_pos =
  if invalid_pos curr_pos || invalid_pos new_pos then raise InvalidPos
  else
    let curr_piece_moved = curr_pos |> get_piece board |> move_piece new_pos in
    let new_piece_moved = new_pos |> get_piece board |> move_piece curr_pos in
    replace_row new_piece_moved (fst curr_pos) (snd curr_pos) board
    |> replace_row curr_piece_moved (fst new_pos) (snd new_pos)

let rec to_string (board : t) =
  match board with
  | [] -> ""
  | h :: t -> row_to_string h ^ "\n-----------------\n" ^ to_string t