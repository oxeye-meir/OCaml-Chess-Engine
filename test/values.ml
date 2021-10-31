open Chess.Board
open Chess.Piece
open Chess.State
open Helper

(* white king ♔ white queen ♕ white rook ♖ white bishop ♗ white knight ♘ white pawn ♙ black
   king ♚ black queen ♛ black rook ♜ black bishop ♝ black knight ♞ black pawn ♟︎ *)

(* [[(0,0); (0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)];
   [(1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7)];
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

let initial_black_pieces = backrank true 0 @ pawns true 1 7 []

let initial_white_pieces = backrank false 7 @ pawns false 6 7 []

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

(* State values*)
let initial_state = init_state

let fst_state = change_state (6, 4) (4, 4) init_state

let snd_state = change_state (1, 0) (3, 0) fst_state

let scholar_state =
  initial_state
  |> change_state (6, 4) (4, 4)
  |> change_state (1, 4) (3, 4)
  |> change_state (7, 3) (3, 7)
  |> change_state (1, 0) (3, 0)
  |> change_state (7, 5) (4, 2)
  |> change_state (1, 1) (3, 1)
  |> change_state (3, 7) (1, 5)

let double_state =
  initial_state |> state_helper "c2" "c4" |> state_helper "a7" "a6" |> state_helper "b1" "c3"
  |> state_helper "d7" "d5" |> state_helper "c3" "b5" |> state_helper "g7" "g5"
  |> state_helper "d1" "a4" |> state_helper "f7" "f5" |> state_helper "b5" "c7"

let bl_checkmate =
  initial_state |> state_helper "f2" "f3" |> state_helper "e7" "e5" |> state_helper "g2" "g4"
  |> state_helper "d8" "h4"

let one_en_passant_state =
  initial_state |> state_helper "e2" "e4" |> state_helper "h7" "h5" |> state_helper "e4" "e5"
  |> state_helper "d7" "d5"

let taking_en_passant_state = one_en_passant_state |> state_helper "e5" "d6"

let ignore_en_passant_state = one_en_passant_state |> state_helper "a2" "a4"

let second_en_passant_state =
  taking_en_passant_state |> state_helper "h5" "h4" |> state_helper "g2" "g4"

let en_passant_into_check =
  initial_state |> state_helper "e2" "e4" |> state_helper "h7" "h5" |> state_helper "e4" "e5"
  |> state_helper "e7" "e6" |> state_helper "h2" "h4" |> state_helper "e8" "e7"
  |> state_helper "a2" "a4" |> state_helper "d7" "d5" |> state_helper "e5" "d6"

(* Boards/Board Setups *)

let fst_board = move (6, 4) (4, 4) None initial_board

let snd_board = move (1, 0) (3, 0) None fst_board

let promotion_board =
  let move_back_forth prev_move back board =
    prev_move board |> if back then move (5, 7) (7, 7) None else move (7, 7) (5, 7) None
  in
  snd_board
  |> move (6, 7) (4, 7) None
  |> move_back_forth (move (0, 0) (2, 0) None) false
  |> move_back_forth (move (2, 0) (2, 1) None) true
  |> move_back_forth (move (2, 1) (6, 1) None) false
  |> move_back_forth (move (6, 1) (6, 0) None) true
  |> move_back_forth (move (6, 0) (7, 0) None) false
  |> move_back_forth (move (7, 0) (7, 1) None) true
  |> move_back_forth (move (3, 0) (4, 0) None) false
  |> move_back_forth (move (4, 0) (5, 0) None) true
  |> move_back_forth (move (5, 0) (6, 0) None) false
  |> move (6, 0) (7, 0) None

let scholar_check = board scholar_state

let double_check = board double_state

let move_into_check =
  initial_board |> move_helper "c2" "c4" |> move_helper "c7" "c5" |> move_helper "d1" "a4"

let en_passant_check = board en_passant_into_check

(* Other Values *)
let empty_space = "         "
