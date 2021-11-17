open Chess
open Piece
open Board
open State
open Command

exception InvalidText

type position = int * int

type error =
  | None
  | InvalidPos
  | WrongColor

let print_quit () = ANSITerminal.print_string [ ANSITerminal.yellow ] "QUITTING .... \n"

let print_resign color opponent =
  ANSITerminal.printf [ ANSITerminal.yellow ] "%s has quit. %s wins!\n" color opponent

let print_rules () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Rules\n\
     1. To move your piece, please write \n\
    \   “current_position new_position”, i.e. \"e2 e4”\n\n\
     2. To quit the current game, type 'Quit'. Please note that \n\
    \   quitting anytime after the first move results in a win \n\
    \   for the other player.\n\n\
     3. To reset the board, type 'Reset'. \n\
    \   To undo the last move, type 'Undo'. \n\n\
     4. For explanations of Chess rules and notations, type 'Help'.\n\n"

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

let print_invalid_move () =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid move. Please try again! \n"

let print_wrong_color () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "That piece is not your color. Please try again! \n"

let print_error error =
  if error = InvalidPos then print_invalid_move ()
  else if error = WrongColor then print_wrong_color ()

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

let print_file_error () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nYour file could not be found. Please try again!\n"

let print_invalid_text () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Your file contains some invalid text. Please verify that your file is valid and try again!\n";
  exit 0

let print_board state = state |> State.board |> to_string |> ANSITerminal.print_string []

let initial_state = Chess.State.init_state

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
        (if state = initial_state then print_quit ()
        else
          let color, opponent =
            if turn state then ("Black", "White") else ("White", "Black")
          in
          print_resign color opponent);
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

let data_dir_prefix = "data" ^ Filename.dir_sep

let pos_of_str str =
  let char_in_range = function
    | 'a' .. 'h' -> true
    | _ -> false
  in
  let int_in_range = function
    | '1' .. '8' -> true
    | _ -> false
  in
  let ch1 = String.get str 0 in
  let ch2 = String.get str 1 in
  if char_in_range ch1 && int_in_range ch2 && String.length str = 2 then
    (7 - (Char.code ch2 - 49), Char.code ch1 - 97)
  else raise InvalidText

let parse_line state line =
  let positions = String.split_on_char ' ' line |> List.filter (fun x -> x <> "") in
  if List.length positions <> 2 then raise InvalidText
  else
    let pos1 = pos_of_str (List.nth positions 0) in
    let pos2 = pos_of_str (List.nth positions 1) in
    change_state pos1 pos2 state

let rec go_thru_state state line = function
  | [] -> state
  | h :: t ->
      let print_line_error line =
        ANSITerminal.printf [ ANSITerminal.red ] "Error on line %i: " line
      in
      let new_state =
        try parse_line state h with
        | InvalidText ->
            print_line_error line;
            print_invalid_text ()
        | InvalidPos ->
            print_line_error line;
            print_invalid_move ();
            exit 0
        | WrongColor ->
            print_line_error line;
            print_wrong_color ();
            exit 0
      in
      go_thru_state new_state (line + 1) t

let rec read_each_line channel acc =
  try read_each_line channel (input_line channel :: acc) with
  | End_of_file ->
      close_in channel;
      acc

let read_file file =
  let channel = open_in file in
  List.rev (read_each_line channel []) |> go_thru_state initial_state 1

let rec start_from_file () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> (
      match String.lowercase_ascii file_name with
      | "regular" -> get_current_board initial_state None |> print_board
      | "quit" ->
          print_quit ();
          exit 0
      | _ ->
          get_current_board (read_file (data_dir_prefix ^ file_name ^ ".txt")) None
          |> print_board)

(** [main first_print] prompts for the game to play, then starts it. If [first_print] is true,
    then it prints the welcome message and rules. Otherwise it prompts for a file or begins a
    regular game.*)
let rec main first_print =
  if first_print then (
    try
      ANSITerminal.print_string [ ANSITerminal.cyan ] "\n\nWelcome to the Chess Game engine.\n";
      print_rules ();
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n\
         If you would like to start a game from a specific file, please enter the file \n\
         name. Otherwise, type Regular to begin a normal game, or Quit to exit. \n";
      start_from_file ()
    with
    | _ ->
        print_file_error ();
        main false)
  else
    try
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n Please enter a file name, or type Regular to start a normal game. \n";
      start_from_file ()
    with
    | _ ->
        print_file_error ();
        main false

(* Execute the game engine. *)
let () = main true
