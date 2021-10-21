open Chess
open Piece
open Board

type position = int * int

(** Chess.Piece, Chess.Board: pieces =
    [Pawn B/W, Rook B/W, Knight B/W, Bishop B/W, Queen B/W, King B/W, Empty] *)

let print_quit () = ANSITerminal.print_string [ ANSITerminal.red ] "QUITTING .... \n"

let print_rules () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Rules\n\
     1. To move your piece, please write \n\
    \     “current_position new_position”, i.e. “e2 e4”\n\n\
     2. To quit, type 'Quit.'\n\n"

(*print_invalid_move is not used anywhere yet*)
let print_invalid_move () =
  ANSITerminal.print_string [ ANSITerminal.magenta ] "Invalid move. Please try again \n";
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

let rec get_board board =
  let input = read_line () in
  let command = get_command input in
  let new_board = Board.move board (fst command) (snd command) in
  print_board new_board

let play_game f = print_endline "Game started"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nWelcome to the Chess Game engine.\n";
  init_board_print ();
  print_rules ();
  print_string "> "

(* Execute the game engine. *)
let () = main ()