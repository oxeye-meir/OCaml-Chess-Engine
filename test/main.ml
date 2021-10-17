open OUnit2
open Chess
open Piece
open Board

let sample_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input

let position_printer (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let id x = x

(*Piece function tests*)
let position_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (position input) ~printer:position_printer

let get_name_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (get_name input) ~printer:id

(* Board function tests*)
let to_string_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (to_string input) ~printer:id

let bl_pawn = init_piece "pawn" true 1 0

let empty_sq = init_piece "empty" false 2 5
(* white king ♔ white queen ♕ white rook ♖ white bishop ♗ white knight ♘ white pawn ♙ black
   king ♚ black queen ♛ black rook ♜ black bishop ♝ black knight ♞ black pawn ♟︎ *)

let piece_tests =
  [
    position_test "position of black pawn is (1,0)" (1, 0) bl_pawn;
    get_name_test "name of black pawn is ♟︎" "♟︎" bl_pawn;
    position_test "position of empty square is (2,5)" (2, 5) empty_sq;
    get_name_test "name of empty square is [ ]" " " empty_sq;
  ]

let initial_board_string =
  let sep = "\n-----------------\n" in
  let empty = "| | | | | | | | |" in
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ empty ^ sep ^ empty
  ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙|♙|♙|♙|♙|" ^ sep ^ "|♖|♘|♗|♕|♔|♗|♘|♖|"

let initial_board = init_board []

let board_tests =
  [ (*to_string_test "initial board configuration" initial_board_string initial_board*) ]

let suite = "test suite for Chess" >::: List.flatten [ piece_tests; board_tests ]

let _ = run_test_tt_main suite
