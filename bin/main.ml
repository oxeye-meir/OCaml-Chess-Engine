open Chess
open Piece
open Board

type position = int * int

let print_quit () = ANSITerminal.print_string [ ANSITerminal.yellow ] "QUITTING .... \n"

let print_rules () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Rules\n\
     1. To move your piece, please write \n\
    \     “current_position new_position”, i.e. e2 e4”\n\n\
     2. To quit, type 'Quit.'\n\n"

(*print_invalid_move is not used anywhere yet*)
let print_invalid_move () =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid move. Please try again \n";
  print_string "> "

let init_board_print () =
  ANSITerminal.print_string [ ANSITerminal.white ] (init_board |> to_string)

let print_board board = board |> to_string |> print_string

let get_command (input : string) : position * position =
  let command =
    try Chess.Command.parse input with
    | Chess.Command.Empty -> Move ((-1, -1), (-1, -1))
    | Chess.Command.Malformed -> Move ((-1, -1), (-1, -1))
  in
  match command with
  | Color color -> ((-1, -1), (-1, -1))
  | Move (loc, dst) -> (loc, dst)
  | Quit ->
      print_quit ();
      exit 0

(* let position_printer (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")" *)

let rec get_current_board board =
  print_board board;
  print_string "Your move> ";
  let input = read_line () |> String.trim in
  let command = get_command input in
  let start_coord = fst command in
  (* position_printer start_coord |> print_string; *)
  let end_coord = snd command in
  (* position_printer start_coord |> print_string; *)
  let next_board =
    try Chess.Board.move board start_coord end_coord with
    | InvalidPos ->
        print_invalid_move ();
        board
  in
  get_current_board next_board

let initial_state = Chess.Board.init_board

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nWelcome to the Chess Game engine.\n";
  print_rules ();

  get_current_board initial_state

(* Execute the game engine. *)
let () = main ()
