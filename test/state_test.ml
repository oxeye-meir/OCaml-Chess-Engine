open OUnit2
open Chess.State
open Values
open Helper

let turn_test name expected_output state =
  name >:: fun _ -> assert_equal expected_output (turn state) ~printer:string_of_bool

let result_test name expected_output state =
  name >:: fun _ -> assert_equal expected_output (result state)

let turn_tests =
  [
    turn_test "turn of initial state is false (white)" false initial_state;
    turn_test "turn of state after one move is true (black)" true fst_state;
    turn_test "turn of state after 2 moves is false (white)" false snd_state;
  ]

let suite = "test suite for State" >::: List.flatten [ turn_tests ]

let _ = run_test_tt_main suite
