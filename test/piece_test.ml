open OUnit2
open Chess.Piece
open Values
open Helper

let position_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (position piece) ~printer:position_printer

let name_test test_name expected_output piece =
  test_name >:: fun _ -> assert_equal expected_output (name piece) ~printer:id

let moves_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (moves piece) ~printer:string_of_int

let color_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (color piece) ~printer:string_of_bool

let is_empty_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (is_empty piece) ~printer:string_of_bool

let valid_moves_test name expected_output piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (valid_moves piece)
    ~printer:(pp_list position_printer)

let position_tests =
  [
    position_test "position of black pawn is (1,0)" (1, 0) bl_pawn;
    position_test "position of empty square is (2,5)" (2, 5) empty_sq;
    position_test "position of moved pawn is (3,0)" (3, 0) moved_pawn;
    position_test "moving a pawn in-place 5 times has a position of (3,0)" (3, 0) moved_5_times;
  ]

let name_tests =
  [
    name_test "name of black pawn is ♟︎" "♟︎" bl_pawn;
    name_test "name of empty square is [ ]" " " empty_sq;
  ]

let moves_tests =
  [
    moves_test "moves of an initial black pawn is 0" 0 bl_pawn;
    moves_test "moved pawn's move counter is 1" 1 moved_pawn;
    moves_test "moving a pawn 5 times has counter of 5" 5 moved_5_times;
  ]

let color_tests =
  [
    color_test "color of black pawn is true (black)" true bl_pawn;
    color_test "color of white pawn is false (white)" false wh_pawn;
  ]

let is_empty_tests =
  [
    is_empty_test "pawn is not empty" false bl_pawn;
    is_empty_test "empty square is empty" true empty_sq;
  ]

let valid_moves_tests =
  [
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

let suite =
  "test suite for Piece"
  >::: List.flatten
         [
           position_tests;
           name_tests;
           moves_tests;
           color_tests;
           is_empty_tests;
           valid_moves_tests;
         ]

let _ = run_test_tt_main suite
