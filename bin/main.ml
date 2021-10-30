open Chess
open Piece
open Board
open State

type position = int * int

type error =
  | None
  | InvalidPos
  | WrongColor

let print_quit () = ANSITerminal.print_string [ ANSITerminal.yellow ] "QUITTING .... \n"

let print_rules () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Rules\n\
     1. To move your piece, please write \n\
    \     “current_position new_position”, i.e. e2 e4”\n\n\
     2. To quit, type 'Quit.'\n\n"

let print_invalid_move () =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid move. Please try again! \n"

let print_wrong_color () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "That piece is not your color. Please try again! \n"

let print_reset () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "You have restarted your game! \n"

let print_check () = ANSITerminal.print_string [ ANSITerminal.yellow ] "Check! \n"

let print_check_mate turn () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    (if turn then "Checkmate! White wins! \n" else "Checkmate! Black wins! \n");
  exit 0

let print_board state = state |> State.board |> to_string |> print_string

let get_command (input : string) : position * position =
  let command =
    try Chess.Command.parse input with
    | Chess.Command.Malformed -> Move ((-1, -1), (-1, -1))
  in
  match command with
  | Move (loc, dst) -> (loc, dst)
  | Reset -> ((-99, -99), (-99, -99))
  | Quit ->
      print_quit ();
      exit 0

let initial_state = Chess.State.init_state

let print_error error =
  if error = InvalidPos then print_invalid_move ()
  else if error = WrongColor then print_wrong_color ()

let rec get_current_board state reset error =
  print_board state;
  print_error error;
  if reset then print_reset ();
  let turn_color = turn state in
  let board = State.board state in
  if State.checkmate state then print_check_mate turn_color ();
  if check board then print_check ();
  print_string (if turn_color then "Black move> " else "White move> ");
  let command = read_line () |> get_command in
  let start_coord = fst command in
  let reset_value = fst start_coord = -99 in
  let next_state =
    if reset_value then initial_state
    else
      try State.change_state start_coord (snd command) state with
      | InvalidPos -> get_current_board state reset_value InvalidPos
      | WrongColor -> get_current_board state reset_value WrongColor
  in
  get_current_board next_state reset_value None

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let start =
    ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nWelcome to the Chess Game engine.\n";
    print_rules ();
    let board_operate = get_current_board initial_state false None |> print_board in
    board_operate
  in
  start

(* Execute the game engine. *)
let () = main ()
