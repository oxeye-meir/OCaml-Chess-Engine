open OUnit2
open Chess.State
open Values
open Helper

let turn_test name expected_output state =
  name >:: fun _ -> assert_equal expected_output (turn state) ~printer:string_of_bool

let result_test name expected_output state =
  name >:: fun _ -> assert_equal expected_output (result state) ~printer:result_printer

let checkmate_test name state expected_output =
  name >:: fun _ -> checkmate state |> assert_equal expected_output ~printer:string_of_bool

let stalemate_test name state expected_output =
  name >:: fun _ -> stalemate state |> assert_equal expected_output ~printer:string_of_bool

let graveyard_test name expected_output state color =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (graveyard state color)

let score_test name state color expected_output =
  name >:: fun _ -> score state color |> assert_equal expected_output ~printer:string_of_int

let turn_tests =
  [
    turn_test "turn of initial state is false (white)" false initial_state;
    turn_test "turn of state after one move is true (black)" true fst_state;
    turn_test "turn of state after 2 moves is false (white)" false snd_state;
    turn_test "turn of before moving into promotion is false (white)" false pre_promotion;
    turn_test "turn of state after moving into promotion is still false (white)" false
      promotion_state;
  ]

let result_tests =
  [
    result_test "result of initial state is Playing None" (Playing None) initial_state;
    result_test "result of moving a pawn two squares is Playing None" (Playing None) fst_state;
    result_test "result of scholar's checkmate is WhiteWin" WhiteWin scholar_state;
    result_test "result of one valid en passant is Playing Some (3, 3) (3, 4)"
      (Playing (Some ((3, 3), (3, 4))))
      one_en_passant_state;
    result_test "result of ignoring en passant is Playing None" (Playing None)
      ignore_en_passant_state;
    result_test "result of taking en passant is Playing None" (Playing None)
      taking_en_passant_state;
    result_test "result of second en passant is Playing Some (4, 6) (4, 7)"
      (Playing (Some ((4, 6), (4, 7))))
      second_en_passant_state;
    result_test "result of being able to capture out of mate is Playing None" (Playing None)
      capture_out_of_mate;
    result_test "result of black checkmate is BlackWin" BlackWin black_checkmate;
    result_test "result of white checkmate is WhiteWin" WhiteWin white_checkmate;
    result_test "result of stalemate is Stalemate" Stalemate stalemate_state;
    result_test "result of a state before promotion is Playing None" (Playing None)
      pre_promotion;
    result_test "result of promoting is Promotion (0,5)" (Promotion (0, 5)) promotion_state;
    result_test "result after promoting is Playing None" (Playing None) after_promote;
  ]

let checkmate_tests =
  [
    checkmate_test "Initial board is not a checkmate state" initial_state false;
    checkmate_test "Scholar's checkmate is in a checkmate state" scholar_state true;
    checkmate_test "Double check is not a checkmate state" double_state false;
    checkmate_test "Black checkmate is in a checkmate state" bl_checkmate true;
    checkmate_test "Being able to capture out of mate is not a checkmate state"
      capture_out_of_mate false;
    checkmate_test "Stalemate is not a checkmate state" stalemate_state false;
    checkmate_test "Pre-promotion is not a checkmate state" pre_promotion false;
    checkmate_test "Promoting into a queen is not a checkmate state" promotion_state false;
  ]

let stalemate_tests =
  [
    stalemate_test "Initial board is not a stalemate state" init_state false;
    stalemate_test "Scholar's checkmate is not a stalemate state" scholar_state false;
    stalemate_test "Double check is not a stalemate state" double_state false;
    stalemate_test "Black checkmate is not a stalemate state" bl_checkmate false;
    stalemate_test "Stalemate state is a stalemate" stalemate_state true;
    stalemate_test "not stalemate state is not a stalemate" not_stalemate_state false;
    stalemate_test "Pre-promotion is not a stalemate state" pre_promotion false;
    stalemate_test "Promoting into a queen is not a stalemate state" promotion_state false;
  ]

let graveyard_tests =
  [
    graveyard_test "graveyard of white initially is []" [] init_state false;
    graveyard_test "graveyard of black initially is []" [] init_state true;
    graveyard_test "graveyard of white in black checkmate is [King]" [ checkmated_wh_king ]
      black_checkmate false;
    graveyard_test "graveyard of black in black checkmate is []" [] black_checkmate true;
    graveyard_test "graveyard of white in white checkmate is []" [] white_checkmate false;
    graveyard_test "graveyard of black in white checkmate is [King, Pawn]"
      [ checkmated_bl_king; kingside_bl_pawn ]
      white_checkmate true;
    graveyard_test "graveyard of white in stalemate is white_graveyard"
      white_stalemate_graveyard stalemate_state false;
    graveyard_test "graveyard of black in stalemate is black_graveyard"
      black_stalemate_graveyard stalemate_state true;
    graveyard_test "Black graveyard before promoting is bl_promotion_graveyard"
      bl_promotion_graveyard promotion_state true;
    graveyard_test "Black graveyard after promoting is still bl_promotion_graveyard"
      bl_promotion_graveyard after_promote true;
  ]

let score_tests =
  [
    score_test "Initial white score is 0" init_state false 0;
    score_test "Initial black score is 0" init_state true 0;
    score_test "Black score of out of mate state is 2" capture_out_of_mate true 2;
    score_test "White score of out of mate state is 1" capture_out_of_mate false 1;
    score_test "Black score after checkmate is 1000" black_checkmate true 1000;
    score_test "White score after checkmate is 1001" white_checkmate false 1001;
    score_test "Black score after stalemate is 39" stalemate_state true 39;
    score_test "White score after stalemate is 5" stalemate_state false 5;
  ]

let suite =
  "test suite for State"
  >::: List.flatten
         [
           turn_tests;
           result_tests;
           checkmate_tests;
           stalemate_tests;
           graveyard_tests;
           score_tests;
         ]

let _ = run_test_tt_main suite
