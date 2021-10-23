open Piece

type t = Piece.t list list

exception InvalidPos

(* Helper functions *)
let get_piece board (x, y) : Piece.t =
  if x < 0 || x > 7 || y < 0 || y > 7 then raise InvalidPos
  else
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

let row_to_string row = List.fold_left (fun acc piece -> acc ^ name piece ^ "|") "|" row

let invalid_pos (x, y) = x < 0 || x > 7 || y < 0 || y > 7

let rec empty_squares color x y lst =
  if y >= 0 then empty_squares (not color) x (y - 1) (init_piece "empty" color x y :: lst)
  else lst

let rec pawns color x y lst =
  if y >= 0 then pawns color x (y - 1) (init_piece "pawn" color x y :: lst) else lst

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
    pawns true 1 7 [];
    empty_squares false 2 7 [];
    empty_squares true 3 7 [];
    empty_squares false 4 7 [];
    empty_squares true 5 7 [];
    pawns false 6 7 [];
    backrank false 7;
  ]

let rec left_diagonal board (x, y) lst = function
  | true ->
      if x >= 1 && y >= 1 && is_empty (get_piece board (x - 1, y - 1)) then
        left_diagonal board (x - 1, y - 1) ((x - 1, y - 1) :: lst) true
      else lst
  | false ->
      if x <= 6 && y <= 6 && is_empty (get_piece board (x + 1, y + 1)) then
        left_diagonal board (x + 1, y + 1) ((x + 1, y + 1) :: lst) false
      else lst

let rec right_diagonal board (x, y) lst = function
  | true ->
      if x >= 1 && y <= 6 && is_empty (get_piece board (x - 1, y + 1)) then
        right_diagonal board (x - 1, y + 1) ((x - 1, y + 1) :: lst) true
      else lst
  | false ->
      if x <= 6 && y >= 1 && is_empty (get_piece board (x + 1, y - 1)) then
        right_diagonal board (x + 1, y - 1) ((x + 1, y - 1) :: lst) false
      else lst

let rec vertical_check board (x, y) lst = function
  | true ->
      if x >= 1 && is_empty (get_piece board (x - 1, y)) then
        vertical_check board (x - 1, y) ((x - 1, y) :: lst) true
      else lst
  | false ->
      if x <= 6 && is_empty (get_piece board (x + 1, y)) then
        vertical_check board (x + 1, y) ((x + 1, y) :: lst) false
      else lst

let rec horizontal_check board (x, y) lst = function
  | true ->
      if y >= 1 && is_empty (get_piece board (x, y - 1)) then
        horizontal_check board (x, y - 1) ((x, y - 1) :: lst) true
      else lst
  | false ->
      if y <= 6 && is_empty (get_piece board (x, y + 1)) then
        horizontal_check board (x, y + 1) ((x, y + 1) :: lst) false
      else lst

let next_moves board piece =
  let possible_moves =
    try valid_moves piece with
    | EmptySquare -> []
  in
  let left_dgl_moves = left_diagonal board (position piece) [] in
  let right_dgl_moves = right_diagonal board (position piece) [] in
  let horizontal_moves = horizontal_check board (position piece) [] in
  let vertical_moves = vertical_check board (position piece) [] in
  let total_lst =
    horizontal_moves true @ horizontal_moves false @ vertical_moves true @ vertical_moves false
    @ left_dgl_moves true @ left_dgl_moves false @ right_dgl_moves true @ right_dgl_moves false
  in
  List.filter (fun x -> List.mem x total_lst) possible_moves

let move board (x1, y1) (x2, y2) =
  if invalid_pos (x1, y1) || invalid_pos (x2, y2) then raise InvalidPos
  else
    let curr_piece = (x1, y1) |> get_piece board in
    let curr_legal = next_moves board curr_piece in
    if not (List.mem (x2, y2) curr_legal) then raise InvalidPos
    else
      let curr_piece_moved = move_piece (x2, y2) curr_piece in
      let new_piece_moved = (x2, y2) |> get_piece board |> move_piece (x1, y1) in
      replace_row new_piece_moved x1 y1 board |> replace_row curr_piece_moved x2 y2

let rec to_string (board : t) =
  match board with
  | [] -> ""
  | h :: t -> row_to_string h ^ "\n-----------------\n" ^ to_string t