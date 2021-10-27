open OUnit2
open Chess.Command
open Values

let parse_test (name : string) (str : string) (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

let parse_excep_test (name : string) (str : string) (e : exn) : test =
  name >:: fun _ -> assert_raises e (fun () -> parse str)

let parse_tests =
  [
    parse_test "Input reset shoud parse to Reset" "reset" Reset;
    parse_test "Input rEsEt shoud parse to Reset" "rEsEt" Reset;
    parse_test "Input \"         rEsEt         \" shoud parse to Reset"
      (empty_space ^ "rEsEt" ^ empty_space)
      Reset;
    parse_test "Input Quit should parse to Quit" "Quit" Quit;
    parse_test "Input  quIT  should parse to Quit" "  quIT  " Quit;
    parse_test "Input a3 c6 should parse to Move (5,0) (2,2)" "a3 c6" (Move ((5, 0), (2, 2)));
    parse_test "Input a1 h7 should parse to Move (7,0) (1,7)" "a1 h7" (Move ((7, 0), (1, 7)));
    parse_test "input \"a3         c6\" should parse to Move (5,0) (2,2)"
      ("a3" ^ empty_space ^ "c6")
      (Move ((5, 0), (2, 2)));
    parse_test "input \"         a3         c6         \" should parse to Move (5,0) (2,2)"
      (empty_space ^ "a3" ^ empty_space ^ "c6" ^ empty_space)
      (Move ((5, 0), (2, 2)));
  ]

let parse_excep_tests =
  [
    parse_excep_test "An empty input should raise Malformed" "" Malformed;
    parse_excep_test "An empty input should raise Malformed" "    " Malformed;
    parse_excep_test "An input of yellow should raise Malformed" "yellow" Malformed;
    parse_excep_test "An input of quite should raise Malformed" "quite" Malformed;
    parse_excep_test "An input of a5 c10 should raise Malformed" "a5 c10" Malformed;
    parse_excep_test "An input of a7 should raise Malformed" "a7" Malformed;
  ]

let suite = "test suite for Command" >::: List.flatten [ parse_tests; parse_excep_tests ]

let _ = run_test_tt_main suite
