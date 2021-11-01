open Chess
open Piece
open Board
open State
open Command

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
    \     “current_position new_position”, i.e. \"e2 e4”\n\n\
     2. To quit the current game, type 'Quit'. To reset the board, type 'Reset'. \n\
    \   To undo the last move, type 'Undo'. \n\n\
     3. For explanations of Chess rules and notations, type 'Help'.\n\n"

let print_invalid_move () =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid move. Please try again! \n"

let print_wrong_color () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "That piece is not your color. Please try again! \n"

let print_reset () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "You have restarted your game! \n"

let print_check () = ANSITerminal.print_string [ ANSITerminal.yellow ] "Check! \n"

let print_check_mate result () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    (match result with
    | WhiteWin -> "Checkmate! White wins! \n"
    | BlackWin -> "Checkmate! Black wins! \n"
    | _ -> raise (Failure "Invalid Checkmate"));
  exit 0

let print_board state = state |> State.board |> to_string |> ANSITerminal.print_string []

(* let get_command (input : string) : position * position = let command = try
   Chess.Command.parse input with | Chess.Command.Malformed -> Move ((-1, -1), (-1, -1)) in
   match command with | Move (loc, dst) -> (loc, dst) | Reset -> ((-99, -99), (-99, -99)) |
   Undo -> ((-50, -50), (-50, -50)) | Quit -> print_quit (); exit 0 *)

let initial_state = Chess.State.init_state

let print_error error =
  if error = InvalidPos then print_invalid_move ()
  else if error = WrongColor then print_wrong_color ()

let print_help () =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "Notation: each square on the board is referred to by a letter and a number. The letter \n\
    \  represents the column, while the number represents the row. Example: the third square \n\
    \  of the second row is notated as c7. \n\n\
     The black pieces are located on rows 7 and 8, while the white pieces are located on \n\
     rows 1 and 2.\n\n\
     Rules for Piece Movement ";
  ANSITerminal.print_string [ ANSITerminal.yellow; ANSITerminal.Bold ] "(Credit to ichess.net)";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    ":\n\
     Kings move one square in any direction, so long as that square is not attacked by an \n\
    \  enemy piece. Additionally, kings are able to make a special move, known as castling.\n\n\
     Queens move diagonally, horizontally, or vertically any number of squares. They are \n\
    \  unable to jump over pieces.\n\n\
     Rooks move horizontally or vertically any number of squares. They are unable to jump \n\
    \  over pieces. Rooks move when the king castles.\n\n\
     Bishops move diagonally any number of squares. They are unable to jump over pieces.\n\n\
     Knights move in an ‘L’ shape’: two squares in a horizontal or vertical direction, then \n\
    \  move one square horizontally or vertically. They are the only piece able to jump over \n\
    \  other pieces.\n\n\
     Pawns move vertically forward one square, with the option to move two squares if they \n\
    \  have not yet moved. Pawns are the only piece to capture different to how they move. The \n\
    \  pawns capture one square diagonally in a forward direction.\n\
    \  Pawns are unable to move backward on captures or moves. Upon reaching the other side of \n\
    \ the board a pawn promotes into any other piece, except for a king. Additionally, pawns \n\
    \  can make a special move named En Passant.\n\n"

let undo_command state =
  try State.undo state with
  | NoUndo ->
      ANSITerminal.print_string [ ANSITerminal.red ] "There are no more turns to undo! \n";
      state

let rec get_current_board state error =
  print_board state;
  print_error error;
  let board = State.board state in
  if State.checkmate state then print_check_mate (result state) ();
  if check board then print_check ();
  ANSITerminal.print_string [] (if turn state then "Black move> " else "White move> ");
  let input = read_line () in
  let command =
    try parse input with
    | Malformed -> Move ((-1, -1), (-1, -1))
  in
  let next_state =
    match command with
    | Quit ->
        print_quit ();
        exit 0
    | Reset ->
        print_reset ();
        initial_state
    | Undo -> undo_command state
    | Help ->
        print_help ();
        state
    | Move (start_coord, end_coord) -> (
        try change_state start_coord end_coord state with
        | InvalidPos -> get_current_board state InvalidPos
        | WrongColor -> get_current_board state WrongColor)
  in
  get_current_board next_state None

(* let command = read_line () |> get_command in let start_coord = fst command in let undo = fst
   start_coord = -50 in let reset_value = fst start_coord = -99 in let next_state = if undo
   then ( try State.undo state with | NoUndo -> ANSITerminal.print_string [ ANSITerminal.red ]
   "There are no more turns to undo! \n"; state) else if reset_value then initial_state else
   try State.change_state start_coord (snd command) state with | InvalidPos ->
   get_current_board state reset_value InvalidPos | WrongColor -> get_current_board state
   reset_value WrongColor in get_current_board next_state reset_value None *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let start =
    ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nWelcome to the Chess Game engine.\n";
    print_rules ();
    let board_operate = get_current_board initial_state None |> print_board in
    board_operate
  in
  start

(* Execute the game engine. *)
let () = main ()
