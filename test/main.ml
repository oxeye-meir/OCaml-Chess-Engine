open OUnit2
open Chess
open Piece
open Board

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are equivalent
    set-like lists. That means checking two things. First, they must both be {i set-like},
    meaning that they do not contain any duplicates. Second, they must contain the same
    elements, though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1 && List.length lst2 = List.length uniq2 && uniq1 = uniq2

let position_printer (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let id x = x

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

(*Piece function tests*)
let position_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (position piece) ~printer:position_printer

let name_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (Piece.name piece) ~printer:id

let valid_moves_test name expected_output piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (valid_moves piece)
    ~printer:(pp_list position_printer)

(* Board function tests*)
let to_string_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (to_string input) ~printer:id

let next_moves_test name expected_output board piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (next_moves board piece)
    ~printer:(pp_list position_printer)

let invalidpos_test name board curr_pos new_pos =
  name >:: fun _ -> assert_raises InvalidPos (fun () -> move board curr_pos new_pos)

let initial_board = init_board

let bl_pawn = init_piece "pawn" true 1 0

let wh_pawn = init_piece "pawn" false 6 0

let bl_king = init_piece "king" true 2 4

let bl_rook = init_piece "rook" true 0 0

let bl_knight = init_piece "knight" true 0 1

let initial_wh_knight_L = init_piece "knight" false 7 1

let initial_wh_knight_R = init_piece "knight" false 7 6

let bl_bishop = init_piece "bishop" true 0 2

let bl_queen = init_piece "queen" true 3 3

let initial_bl_queen = init_piece "queen" true 0 3

let empty_sq = init_piece "empty" false 2 5

(* white king ♔ white queen ♕ white rook ♖ white bishop ♗ white knight ♘ white pawn ♙ black
   king ♚ black queen ♛ black rook ♜ black bishop ♝ black knight ♞ black pawn ♟︎ *)

let piece_tests =
  [
    position_test "position of black pawn is (1,0)" (1, 0) bl_pawn;
    name_test "name of black pawn is ♟︎" "♟︎" bl_pawn;
    position_test "position of empty square is (2,5)" (2, 5) empty_sq;
    name_test "name of empty square is [ ]" " " empty_sq;
    valid_moves_test "valid move of black pawn is [(2,0); (3,0)]" [ (2, 0); (3, 0) ] bl_pawn;
    valid_moves_test "valid move of white pawn is [(5, 0); (4,0)] " [ (5, 0); (4, 0) ] wh_pawn;
    valid_moves_test "valid move of king is [(1,3);(1,4);(1,5);(2,3);(2,5);(3,3);(3,4);(3,5)]"
      [ (1, 3); (1, 4); (1, 5); (2, 3); (2, 5); (3, 3); (3, 4); (3, 5) ]
      bl_king;
    valid_moves_test
      "valid moves of rook is [ (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7,0); (0, \
       1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7)]"
      [
        (1, 0);
        (2, 0);
        (3, 0);
        (4, 0);
        (5, 0);
        (6, 0);
        (7, 0);
        (0, 1);
        (0, 2);
        (0, 3);
        (0, 4);
        (0, 5);
        (0, 6);
        (0, 7);
      ]
      bl_rook;
    valid_moves_test "valid moves of knight is [(2,0);(2,2); (1,3)]" [ (2, 0); (2, 2); (1, 3) ]
      bl_knight;
    valid_moves_test "valid moves of bishop is [(1,1);(2,0);(1,3);(2,4);(3,5);(4,6);(5,7)]"
      [ (1, 1); (2, 0); (1, 3); (2, 4); (3, 5); (4, 6); (5, 7) ]
      bl_bishop;
    valid_moves_test
      "valid moves of queen is \
       [(3,0);(3,1);(3,2);(3,4);(3,5);(3,6);(3,7);(0,3);(1,3);(2,3);(4,3);(4,2);(5,3);(6,3);(7,3);(0,0);(1,1);(2,2);(4,4);(5,5);(6,6);(7,7);(0,6);(1,5);(2,4);(5,1);(6,0)]"
      [
        (3, 0);
        (3, 1);
        (3, 2);
        (3, 4);
        (3, 5);
        (3, 6);
        (3, 7);
        (0, 3);
        (1, 3);
        (2, 3);
        (4, 3);
        (5, 3);
        (6, 3);
        (7, 3);
        (0, 0);
        (1, 1);
        (2, 2);
        (4, 4);
        (5, 5);
        (6, 6);
        (7, 7);
        (0, 6);
        (1, 5);
        (2, 4);
        (4, 2);
        (5, 1);
        (6, 0);
      ]
      bl_queen;
    valid_moves_test
      "valid moves of initial queen is [(0,0);(0,1);(0,2); (0,4); \
       (0,5);(0,6);(0,7);(1,3);(2,3);(3,3);(4,3);(5,3);(6,3);(7,3);(1,2);(2,1);(3,0);(1,4);(2,5);(3,6);(4,7)]"
      [
        (0, 0);
        (0, 1);
        (0, 2);
        (0, 4);
        (0, 5);
        (0, 6);
        (0, 7);
        (1, 3);
        (2, 3);
        (3, 3);
        (4, 3);
        (5, 3);
        (6, 3);
        (7, 3);
        (1, 2);
        (2, 1);
        (3, 0);
        (1, 4);
        (2, 5);
        (3, 6);
        (4, 7);
      ]
      initial_bl_queen;
  ]

let initial_board_string =
  let sep = "\n-----------------\n" in
  let empty = "| | | | | | | | |" in
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ empty ^ sep ^ empty
  ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙|♙|♙|♙|♙|" ^ sep ^ "|♖|♘|♗|♕|♔|♗|♘|♖|" ^ sep

let new_board_string =
  let sep = "\n-----------------\n" in
  let empty = "| | | | | | | | |" in
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "| |♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ "|♟︎| | | | | | | |"
  ^ sep ^ empty ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙|♙|♙|♙|♙|" ^ sep ^ "|♖|♘|♗|♕|♔|♗|♘|♖|" ^ sep

let new_board_string_2 =
  let sep = "\n-----------------\n" in
  let empty = "| | | | | | | | |" in
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "|♟︎|♟︎|♟︎|♟︎| |♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ "| | | | |♟︎| | | |"
  ^ sep ^ empty ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙|♙|♙|♙|♙|" ^ sep ^ "|♖|♘|♗|♕|♔|♗|♘|♖|" ^ sep

let board_tests =
  let new_board_2 = move initial_board (1, 4) (3, 4) in
  [
    next_moves_test "black pawn's next move is [ (2, 0); (3,0) ] " [ (2, 0); (3, 0) ]
      init_board bl_pawn;
    next_moves_test "initial black queen's next move is []" [] init_board initial_bl_queen;
    next_moves_test "next moves of bishop is []" [] init_board bl_bishop;
    next_moves_test "black rook's next moves is [(2, 0); (3, 0); (4, 0); (5, 0)]" []
      initial_board bl_rook;
    next_moves_test "left white knight's next moves is [(5, 0); (5, 2)]" [ (5, 0); (5, 2) ]
      initial_board initial_wh_knight_L;
    next_moves_test "right white knight's next moves is [(5, 7); (5, 5)]" [ (5, 7); (5, 5) ]
      initial_board initial_wh_knight_R;
    (let new_board = move initial_board (1, 0) (3, 0) in
     to_string_test "new board's configuation after moving pawn at (1,0)" new_board_string
       new_board);
    next_moves_test "white pawn's next moves is [ (5, 0); (4,0)] " [ (5, 0); (4, 0) ]
      initial_board wh_pawn;
    to_string_test "initial board configuration" initial_board_string initial_board;
    next_moves_test "black rook's next moves after moving a pawn is [(1,0); (2,0)]"
      [ (1, 0); (2, 0) ]
      (move initial_board (1, 0) (3, 0))
      bl_rook;
    invalidpos_test "moving to (-1,-1) should raise InvalidPos" initial_board (0, 0) (-1, -1);
    to_string_test "new board's configuation after moving black pawn at (1,4)"
      new_board_string_2 new_board_2;
  ]

(*Command Module Tests Here*)
let parse_test (name : string) (str : string) (expected_output : Command.command) : test =
  name >:: fun _ -> assert_equal expected_output (Command.parse str)

let parse_excep_test (name : string) (str : string) (e : exn) : test =
  name >:: fun _ -> assert_raises e (fun () -> Command.parse str)

let command_tests =
  [
    parse_test "Input reset shoud parse to Reset" "reset" Reset;
    parse_test "Input rEsEt shoud parse to Reset" "rEsEt" Reset;
    parse_test "Input Quit should parse to Quit" "Quit" Quit;
    parse_test "Input  quIT  should parse to Quit" "  quIT  " Quit;
    parse_test "Input a3 c6 should parse to Move (5,0) (2,2)" "a3 c6" (Move ((5, 0), (2, 2)));
    parse_test "Input a1 h7 should parse to Move (7,0) (1,7)" "a1 h7" (Move ((7, 0), (1, 7)));
    parse_excep_test "An empty input should raise Malformed" "" Command.Malformed;
    parse_excep_test "An empty input should raise Malformed" "    " Command.Malformed;
    parse_excep_test "An input of yellow should raise Malformed" "yellow" Command.Malformed;
    parse_excep_test "An input of quite should raise Malformed" "quite" Command.Malformed;
    parse_excep_test "An input of a5 c10 should raise Malformed" "a5 c10" Command.Malformed;
    parse_excep_test "An input of a7 should raise Malformed" "a7" Command.Malformed;
  ]

let suite =
  "test suite for Chess" >::: List.flatten [ piece_tests; board_tests; command_tests ]

let _ = run_test_tt_main suite
