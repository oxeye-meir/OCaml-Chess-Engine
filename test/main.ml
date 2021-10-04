open OUnit2

let sample_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output input
