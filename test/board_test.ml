open OUnit2
open Chess.Board
open Helper
open Values
let get_piece_test name expected_output board pos =
  name >:: fun _ -> assert_equal expected_output (get_piece board pos)

let next_moves_test name expected_output board piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (next_moves board piece)
    ~printer:(pp_list position_printer)

let invalidpos_test name board curr_pos new_pos turn =
  name >:: fun _ -> assert_raises InvalidPos (fun () -> move curr_pos new_pos turn board)

let check_test name board expected_output =
  name >:: fun _ -> check board |> assert_equal expected_output

let checkmate_test name board turn expected_output =
  name >:: fun _ -> checkmate board turn |> assert_equal expected_output

let to_string_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (to_string input) ~printer:id

let get_piece_tests =
  [
    get_piece_test "the piece at (7,0) after promotion is a Queen" promotion_queen
      promotion_board (7, 0);
  ]

let next_moves_tests =
  [
    next_moves_test "black pawn's next move is [ (2, 0); (3,0) ] " [ (2, 0); (3, 0) ]
      init_board bl_pawn;
    next_moves_test "initial black queen's next move is []" [] init_board initial_bl_queen;
    next_moves_test "initial black bishop's next move is []" [] init_board bl_bishop;
    next_moves_test "black rook's next moves is [(2, 0); (3, 0); (4, 0); (5, 0)]" []
      initial_board bl_rook;
    next_moves_test "left white knight's next moves is [(5, 0); (5, 2)]" [ (5, 0); (5, 2) ]
      initial_board initial_wh_knight_L;
    next_moves_test "right white knight's next moves is [(5, 7); (5, 5)]" [ (5, 7); (5, 5) ]
      initial_board initial_wh_knight_R;
    next_moves_test "white pawn's next moves is [ (5, 0); (4,0)] " [ (5, 0); (4, 0) ]
      initial_board wh_pawn;
    next_moves_test "white rook's next moves after moving a pawn is [(6,0); (5,0)]"
      [ (6, 0); (5, 0) ]
      (move (6, 0) (4, 0) false initial_board)
      wh_rook;
  ]

let invalidpos_tests =
  [
    invalidpos_test "moving to (-1,-1) should raise InvalidPos" initial_board (0, 0) (-1, -1) false;
    invalidpos_test "moving the wrong color initially should raise InvalidPos" initial_board
      (1, 0) (3, 0) false;
    invalidpos_test "moving the wrong color after first move should raise InvalidPos" fst_board
      (6, 7) (4, 7) true;
    invalidpos_test "Cannot move into a check" move_into_check (1, 3) (3, 3) false;
  ]

let check_tests =
  [
    check_test "Initial board should not be in a check state" initial_board false;
    check_test "Scholar's checkmate should be in a check state" scholar_check true;
    check_test "Double check should be in a check state" double_check true;
  ]

let checkmate_tests =
  [
    checkmate_test "Initial board should not be in a checkmate state" initial_board false false;
    checkmate_test "Scholar's checkmate should be in a checkmate state" scholar_check true true;
    checkmate_test "Double check is not a checkmate state" double_check true false;
  ]

let to_string_tests =
  [
    to_string_test "initial board's string configuration" initial_board_string initial_board;
    to_string_test "first board's configuation after moving white pawn at (6,4)"
      fst_board_string fst_board;
    to_string_test "second board's configuration after moving black pawn at (1,0)"
      snd_board_string snd_board;
    to_string_test "promotion board's configuration after capturing and promotion"
      promotion_board_string promotion_board;
  ]

let suite =
  "test suite for Board"
  >::: List.flatten
         [
           get_piece_tests;
           next_moves_tests;
           invalidpos_tests;
           check_tests;
           checkmate_tests;
           to_string_tests;
         ]

let _ = run_test_tt_main suite
