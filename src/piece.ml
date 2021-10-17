type color = bool

exception EmptySquare

exception InvalidPiece of string

type piece_info = {
  name : string;
  color : color;
  x : int;
  y : int;
}

type t =
  | Pawn of piece_info
  | Rook of piece_info
  | Knight of piece_info
  | Bishop of piece_info
  | Queen of piece_info
  | King of piece_info
  | Empty of piece_info

(** [pattern_helper f piece] is the result of applying function [f] on [piece] by
    pattern-matching [piece] against the possible pieces. *)
let pattern_helper f piece =
  match piece with
  | Pawn t -> f t
  | Rook t -> f t
  | Knight t -> f t
  | Bishop t -> f t
  | Queen t -> f t
  | King t -> f t
  | Empty t -> f t

(* white king ♔ white queen ♕ white rook ♖ white bishop ♗ white knight ♘ white pawn ♙ black
   king ♚ black queen ♛ black rook ♜ black bishop ♝ black knight ♞ black pawn ♟︎ *)

let init_piece name color x y =
  match name with
  | "pawn" ->
      let piece_name = if color then "♟︎" else "♙" in
      Pawn { name = piece_name; color; x; y }
  | "knight" ->
      let piece_name = if color then "♞" else "♘" in
      Knight { name = piece_name; color; x; y }
  | "rook" ->
      let piece_name = if color then "♜" else "♖" in
      Rook { name = piece_name; color; x; y }
  | "bishop" ->
      let piece_name = if color then "♝" else "♗" in
      Bishop { name = piece_name; color; x; y }
  | "king" ->
      let piece_name = if color then "♚" else "♔" in
      King { name = piece_name; color; x; y }
  | "queen" ->
      let piece_name = if color then "♛" else "♕" in
      Queen { name = piece_name; color; x; y }
  | "empty" -> Empty { name = " "; color; x; y }
  | _ -> raise (InvalidPiece name)

let is_empty piece =
  match piece with
  | Empty _ -> true
  | _ -> false

let position piece = pattern_helper (fun piece_info -> (piece_info.x, piece_info.y)) piece

let get_name piece = pattern_helper (fun piece_info -> piece_info.name) piece

let valid_pos (x, y) = x <= 7 && x >= 0 && y >= 0 && y <= 7

(* [[(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)];
   [(1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7)];
   [(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7)];
   [(3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7)];
   [(4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7)];
   [(5,0);(5,1);(5,2);(5,3);(5,4);(5,5);(5,6);(5,7)];
   [(6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7)];
   [(7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7)]] *)

let valid_pawn_moves (pawn : piece_info) : (int * int) list =
  if pawn.color then [ (pawn.x + 1, pawn.y) ] else [ (pawn.x - 1, pawn.y) ]

let rec rook_updown row y current_list =
  if row <= 7 then rook_updown (row + 1) y ((row, y) :: current_list) else current_list

let rec rook_leftright col x current_list =
  if col <= 7 then rook_leftright (col + 1) x ((x, col) :: current_list) else current_list

let valid_rook_moves rook =
  let updown_list = rook_updown 0 in
  let left_right_list = rook_leftright 0 in
  [] |> updown_list rook.y |> left_right_list rook.x
  |> List.filter (fun x -> x <> (rook.x, rook.y))
  |> List.sort_uniq compare

let valid_knight_moves (knight : piece_info) : (int * int) list =
  [
    (knight.x + 1, knight.y + 2);
    (knight.x + 1, knight.y - 2);
    (knight.x - 1, knight.y + 2);
    (knight.x - 1, knight.y - 2);
    (knight.x + 2, knight.y + 1);
    (knight.x - 2, knight.y + 1);
    (knight.x + 2, knight.y - 1);
    (knight.x - 2, knight.y - 1);
  ]

let rec bishop_moves direction (x, y) current_list : (int * int) list =
  let next_direction = direction + 4 in
  match direction mod 4 with
  | 0 ->
      if direction / 4 <= 7 then
        (x + 1, y + 1) :: bishop_moves next_direction (x + 1, y + 1) current_list
      else current_list
  | 1 ->
      if direction / 4 <= 7 then
        (x + 1, y - 1) :: bishop_moves next_direction (x + 1, y - 1) current_list
      else current_list
  | 2 ->
      if direction / 4 <= 7 then
        (x - 1, y + 1) :: bishop_moves next_direction (x - 1, y + 1) current_list
      else current_list
  | _ ->
      if direction / 4 <= 7 then
        (x - 1, y - 1) :: bishop_moves next_direction (x - 1, y - 1) current_list
      else current_list

let valid_bishop_moves (bishop : piece_info) : (int * int) list =
  let top_left_list = bishop_moves 3 in
  let top_right_list = bishop_moves 2 in
  let bottom_left_list = bishop_moves 1 in
  let bottom_right_list = bishop_moves 0 in
  []
  |> top_left_list (bishop.x, bishop.y)
  |> top_right_list (bishop.x, bishop.y)
  |> bottom_left_list (bishop.x, bishop.y)
  |> bottom_right_list (bishop.x, bishop.y)
  |> List.sort_uniq compare

let valid_queen_moves (queen : piece_info) : (int * int) list =
  queen |> valid_rook_moves
  |> List.rev_append (valid_bishop_moves queen)
  |> List.sort_uniq compare

let valid_king_moves (king : piece_info) : (int * int) list =
  [
    (king.x + 1, king.y);
    (king.x - 1, king.y);
    (king.x, king.y - 1);
    (king.x, king.y + 1);
    (king.x + 1, king.y + 1);
    (king.x + 1, king.y - 1);
    (king.x - 1, king.y + 1);
    (king.x - 1, king.y - 1);
  ]

(* Use the piece move logic to get a list of possible moves*)
let valid_moves = function
  | Pawn t -> List.filter valid_pos (valid_pawn_moves t)
  | Rook t -> List.filter valid_pos (valid_rook_moves t)
  | Knight t -> List.filter valid_pos (valid_knight_moves t)
  | Bishop t -> List.filter valid_pos (valid_bishop_moves t)
  | Queen t -> List.filter valid_pos (valid_queen_moves t)
  | King t -> List.filter valid_pos (valid_king_moves t)
  | _ -> raise EmptySquare

(* knight moves can be - 1 up, 2 right - 1 up, 2 left - 1 down, 2 right - 1 down, 2 left - 1
   left, 2 up - 1 left, 2 down - 1 right, 2 up - 1 right, 2 down *)

(* bishop moves can be - 1 up, 1 right - 1 up, 1 left - 1 down, 1 left - 1 down, 1 right - any
   scalar multiple of the above *)

(* rook moves can be - 1-7 left - 1-7 right - 1-7 up - 1-7 down - castling *)

(** queen moves can be - Bishop @ Rook moves *)

(* pawn moves can be (special case of king) - 1 or 2 up/down initially - 1 up normally - 1 up,
   1 right/left for capture - promotion - en passant *)

(** king moves can be - 1 in all 8 directions - castle *)
