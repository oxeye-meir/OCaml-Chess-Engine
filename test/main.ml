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

let rec move_times time (x, y) piece =
  match time with
  | 0 -> piece
  | _ -> move_times (time - 1) (x, y) (move_piece (x, y) piece)

(*Piece function tests*)
let position_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (position piece) ~printer:position_printer

let name_test name expected_output piece =
  name >:: fun _ -> assert_equal expected_output (Piece.name piece) ~printer:id

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

(* Board function tests*)
let turn_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (turn input) ~printer:string_of_bool

let to_string_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (to_string input) ~printer:id

let get_piece_test name expected_output board pos =
  name >:: fun _ -> assert_equal expected_output (get_piece board pos)

let next_moves_test name expected_output board piece =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output (next_moves board piece)
    ~printer:(pp_list position_printer)

let invalidpos_test name board curr_pos new_pos =
  name >:: fun _ -> assert_raises InvalidPos (fun () -> move curr_pos new_pos board)

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

(* white king ♔ white queen ♕ white rook ♖ white bishop ♗ white knight ♘ white pawn ♙ black
   king ♚ black queen ♛ black rook ♜ black bishop ♝ black knight ♞ black pawn ♟︎ *)

let piece_tests =
  let moved_pawn = move_piece (3, 0) bl_pawn in
  let moved_5_times = move_times 5 (3, 0) bl_pawn in
  [
    position_test "position of black pawn is (1,0)" (1, 0) bl_pawn;
    name_test "name of black pawn is ♟︎" "♟︎" bl_pawn;
    color_test "color of black pawn is true (black)" true bl_pawn;
    is_empty_test "pawn is not empty" false bl_pawn;
    moves_test "moves of an initial black pawn is 0" 0 bl_pawn;
    color_test "color of white pawn is false (white)" false wh_pawn;
    position_test "position of empty square is (2,5)" (2, 5) empty_sq;
    name_test "name of empty square is [ ]" " " empty_sq;
    is_empty_test "empty square is empty" true empty_sq;
    position_test "position of moved pawn is (3,0)" (3, 0) moved_pawn;
    moves_test "moved pawn's move counter is 1" 1 moved_pawn;
    position_test "moving a pawn in-place 5 times has a position of (3,0)" (3, 0) moved_5_times;
    moves_test "moving a pawn 5 times has counter of 5" 5 moved_5_times;
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

let sep = "\n-----------------\n"

let empty = "| | | | | | | | |"

let initial_board_string =
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ empty ^ sep ^ empty
  ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙|♙|♙|♙|♙|" ^ sep ^ "|♖|♘|♗|♕|♔|♗|♘|♖|" ^ sep

let fst_board = move (6, 4) (4, 4) initial_board

let snd_board = move (1, 0) (3, 0) fst_board

(* [[(0,0); (0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)];
   [(0,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7)];
   [(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7)];
   [(3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7)];
   [(4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7)];
   [(5,0);(5,1);(5,2);(5,3);(5,4);(5,5);(5,6);(5,7)];
   [(6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7)];
   [(7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7)]] *)
let promotion_board =
  let move_back_forth prev_move back board =
    prev_move board |> if back then move (5, 7) (7, 7) else move (7, 7) (5, 7)
  in
  snd_board
  |> move (6, 7) (4, 7)
  |> move_back_forth (move (0, 0) (2, 0)) false
  |> move_back_forth (move (2, 0) (2, 1)) true
  |> move_back_forth (move (2, 1) (6, 1)) false
  |> move_back_forth (move (6, 1) (6, 0)) true
  |> move_back_forth (move (6, 0) (7, 0)) false
  |> move_back_forth (move (7, 0) (7, 1)) true
  |> move_back_forth (move (3, 0) (4, 0)) false
  |> move_back_forth (move (4, 0) (5, 0)) true
  |> move_back_forth (move (5, 0) (6, 0)) false
  |> move (6, 0) (7, 0)

let promotion_queen = init_piece "queen" true 7 0 |> move_times 5 (7, 0)

let fst_board_string =
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ empty ^ sep
  ^ "| | | | |♙| | | |" ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙| |♙|♙|♙|" ^ sep ^ "|♖|♘|♗|♕|♔|♗|♘|♖|"
  ^ sep

let snd_board_string =
  "|♜|♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "| |♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ "|♟︎| | | | | | | |"
  ^ sep ^ "| | | | |♙| | | |" ^ sep ^ empty ^ sep ^ "|♙|♙|♙|♙| |♙|♙|♙|" ^ sep
  ^ "|♖|♘|♗|♕|♔|♗|♘|♖|" ^ sep

let promotion_board_string =
  "| |♞|♝|♛|♚|♝|♞|♜|" ^ sep ^ "| |♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|♟︎|" ^ sep ^ empty ^ sep ^ empty ^ sep
  ^ "| | | | |♙| | |♙|" ^ sep ^ "| | | | | | | |♖|" ^ sep ^ "| | |♙|♙| |♙|♙| |" ^ sep
  ^ "|♛|♜|♗|♕|♔|♗|♘| |" ^ sep

let board_tests =
  [
    turn_test "turn of the initial board is false (white)" false initial_board;
    turn_test "turn of the board after 1 move is true (black)" true fst_board;
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
      (move (6, 0) (4, 0) initial_board)
      wh_rook;
    get_piece_test "the piece at (7,0) after promotion is a Queen" promotion_queen
      (get_board promotion_board) (7, 0);
    invalidpos_test "moving to (-1,-1) should raise InvalidPos" initial_board (0, 0) (-1, -1);
    invalidpos_test "moving the wrong color initially should raise InvalidPos" initial_board
      (1, 0) (3, 0);
    invalidpos_test "moving the wrong color after first move should raise InvalidPos" fst_board
      (6, 7) (4, 7);
    to_string_test "initial board's string configuration" initial_board_string initial_board;
    to_string_test "first board's configuation after moving white pawn at (6,4)"
      fst_board_string fst_board;
    to_string_test "second board's configuration after moving black pawn at (1,0)"
      snd_board_string snd_board;
    to_string_test "promotion board's configuration after capturing and promotion"
      promotion_board_string promotion_board;
  ]

(*Command Module Tests Here*)
let parse_test (name : string) (str : string) (expected_output : Command.command) : test =
  name >:: fun _ -> assert_equal expected_output (Command.parse str)

let parse_excep_test (name : string) (str : string) (e : exn) : test =
  name >:: fun _ -> assert_raises e (fun () -> Command.parse str)

let command_tests =
  let empty_space = "         " in
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
