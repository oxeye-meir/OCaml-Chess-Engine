open Chess.Board
open Chess.Piece
open Chess.State
open Helper

(* white king ♔ white queen ♕ white rook ♖ white bishop ♗ white knight ♘ white pawn ♙ black
   king ♚ black queen ♛ black rook ♜ black bishop ♝ black knight ♞ black pawn ♟︎ *)

(* [[(0,0); (0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)];
   [(0,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7)];
   [(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7)];
   [(3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7)];
   [(4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7)];
   [(5,0);(5,1);(5,2);(5,3);(5,4);(5,5);(5,6);(5,7)];
   [(6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7)];
   [(7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7)]] *)

(* Pieces *)

let initial_board = init_board

let bl_pawn = init_piece "pawn" true 1 0

let wh_pawn = init_piece "pawn" false 6 0

let bl_king = init_piece "king" true 2 4

let bl_rook = init_piece "rook" true 0 0

let wh_rook = init_piece "rook" false 7 0

let bl_knight = init_piece "knight" true 0 1

let initial_wh_knight_L = init_piece "knight" false 7 1

let initial_wh_knight_R = init_piece "knight" false 7 6

let bl_bishop = init_piece "bishop" true 0 2

let bl_queen = init_piece "queen" true 3 3

let initial_bl_queen = init_piece "queen" true 0 3

let empty_sq = init_piece "empty" false 2 5

let promotion_queen = init_piece "queen" true 7 0 |> Helper.move_times 5 (7, 0)

let moved_pawn = move_piece (3, 0) bl_pawn

let moved_5_times = move_times 5 (3, 0) bl_pawn

(* Board Strings *)
let sep = "\n  -------------------------\n"

let empty num = string_of_int num ^ " |  |  |  |  |  |  |  |  |"

let header = "   A  B  C  D  E  F  G  H "

let initial_board_string =
  header ^ sep ^ "8 |♜ |♞ |♝ |♛ |♚ |♝ |♞ |♜ |" ^ sep ^ "7 |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |" ^ sep
  ^ empty 6 ^ sep ^ empty 5 ^ sep ^ empty 4 ^ sep ^ empty 3 ^ sep
  ^ "2 |♙ |♙ |♙ |♙ |♙ |♙ |♙ |♙ |" ^ sep ^ "1 |♖ |♘ |♗ |♕ |♔ |♗ |♘ |♖ |" ^ sep

let fst_board_string =
  header ^ sep ^ "8 |♜ |♞ |♝ |♛ |♚ |♝ |♞ |♜ |" ^ sep ^ "7 |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |" ^ sep
  ^ empty 6 ^ sep ^ empty 5 ^ sep ^ "4 |  |  |  |  |♙ |  |  |  |" ^ sep ^ empty 3 ^ sep
  ^ "2 |♙ |♙ |♙ |♙ |  |♙ |♙ |♙ |" ^ sep ^ "1 |♖ |♘ |♗ |♕ |♔ |♗ |♘ |♖ |" ^ sep

let snd_board_string =
  header ^ sep ^ "8 |♜ |♞ |♝ |♛ |♚ |♝ |♞ |♜ |" ^ sep ^ "7 |  |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |" ^ sep
  ^ empty 6 ^ sep ^ "5 |♟︎ |  |  |  |  |  |  |  |" ^ sep ^ "4 |  |  |  |  |♙ |  |  |  |" ^ sep
  ^ empty 3 ^ sep ^ "2 |♙ |♙ |♙ |♙ |  |♙ |♙ |♙ |" ^ sep ^ "1 |♖ |♘ |♗ |♕ |♔ |♗ |♘ |♖ |" ^ sep

let promotion_board_string =
  header ^ sep ^ "8 |  |♞ |♝ |♛ |♚ |♝ |♞ |♜ |" ^ sep ^ "7 |  |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |♟︎ |" ^ sep
  ^ empty 6 ^ sep ^ empty 5 ^ sep ^ "4 |  |  |  |  |♙ |  |  |♙ |" ^ sep
  ^ "3 |  |  |  |  |  |  |  |♖ |" ^ sep ^ "2 |  |  |♙ |♙ |  |♙ |♙ |  |" ^ sep
  ^ "1 |♛ |♜ |♗ |♕ |♔ |♗ |♘ |  |" ^ sep

(* Boards/Board Setups *)

let fst_board = move (6, 4) (4, 4) false initial_board

let snd_board = move (1, 0) (3, 0) true fst_board

let promotion_board =
  let move_back_forth prev_move back board =
    prev_move board |> if back then move (5, 7) (7, 7) false else move (7, 7) (5, 7) false
  in
  snd_board
  |> move (6, 7) (4, 7) false
  |> move_back_forth (move (0, 0) (2, 0) true) false
  |> move_back_forth (move (2, 0) (2, 1) true) true
  |> move_back_forth (move (2, 1) (6, 1) true) false
  |> move_back_forth (move (6, 1) (6, 0) true) true
  |> move_back_forth (move (6, 0) (7, 0) true) false
  |> move_back_forth (move (7, 0) (7, 1) true) true
  |> move_back_forth (move (3, 0) (4, 0) true) false
  |> move_back_forth (move (4, 0) (5, 0) true) true
  |> move_back_forth (move (5, 0) (6, 0) true) false
  |> move (6, 0) (7, 0) true

let scholar_check =
  initial_board
  |> move (6, 4) (4, 4) false
  |> move (1, 4) (3, 4) true
  |> move (7, 3) (3, 7) false
  |> move (1, 0) (3, 0) true
  |> move (7, 5) (4, 2) false
  |> move (1, 1) (3, 1) true
  |> move (3, 7) (1, 5) false

let double_check =
  initial_board
  |> Helper.move_helper "c2" "c4" false
  |> Helper.move_helper "a7" "a6" true
  |> Helper.move_helper "b1" "c3" false
  |> Helper.move_helper "d7" "d5" true
  |> Helper.move_helper "c3" "b5" false
  |> Helper.move_helper "g7" "g5" true
  |> Helper.move_helper "d1" "a4" false
  |> Helper.move_helper "f7" "f5" true
  |> Helper.move_helper "b5" "c7" false

let move_into_check =
  initial_board
  |> Helper.move_helper "c2" "c4" false
  |> Helper.move_helper "c7" "c5" true
  |> Helper.move_helper "d1" "a4" false

(* State values*)
let initial_state = init_state

let fst_state = change_state (6, 4) (4, 4) init_state

let snd_state = change_state (1, 0) (3, 0) fst_state

(* Other Values *)
let empty_space = "         "
