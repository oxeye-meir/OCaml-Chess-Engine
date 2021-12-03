open Chess
open Piece
open Board
open State
open Command
open Fileutil
open Unix

type position = int * int

type error =
  | None
  | InvalidPos
  | WrongColor
  | InvalidText

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

let print_invalid_text () =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid text. Please try again! \n"

let print_error error =
  match error with
  | InvalidPos -> print_invalid_move ()
  | WrongColor -> print_wrong_color ()
  | InvalidText -> print_invalid_text ()
  | _ -> ()

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

let print_stalemate () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "Stalemate! \n";
  exit 0

let print_file_error () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\nYour file could not be found. Please try again!\n"

let print_invalid_text () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Your file contains some invalid text. Please verify that your file is valid and try again!\n";
  exit 0

let print_invalid_time () =
  ANSITerminal.print_string [ ANSITerminal.red ] "Invalid time format! Please try again.\n"

let print_draw_offer color =
  ANSITerminal.printf [ ANSITerminal.cyan ] "%s offered a draw. Do you accept? \n>" color

let print_draw_accept color =
  ANSITerminal.printf [ ANSITerminal.cyan ]
    "%s accepted the draw. The game has ended in a draw. \n" color;
  exit 0

let print_draw_deny color =
  ANSITerminal.printf [ ANSITerminal.cyan ] "%s denied the draw. The game will continue. \n"
    color

let print_board state = state |> State.board |> to_string |> ANSITerminal.print_string []

let print_scores state =
  let black_score = true |> State.score state |> string_of_int in
  let white_score = false |> State.score state |> string_of_int in
  ANSITerminal.printf [ ANSITerminal.yellow ] "White: %s \n" white_score;
  ANSITerminal.printf [ ANSITerminal.magenta ] "Black: %s \n" black_score

let rec prompt_time () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Please enter the number of time control, in minutes (between 5-60),\n\
     or untimed/casual for no time. \n\
     > ";
  match read_line () with
  | exception End_of_file ->
      print_invalid_time ();
      prompt_time ()
  | s -> begin
      try
        let time_control = float_of_string s in
        if time_control < 5. || time_control > 60. then (
          print_invalid_time ();
          prompt_time ())
        else
          let seconds = time_control *. 60. in
          Some (seconds, seconds)
      with
      | _ ->
          let formatted_str = String.lowercase_ascii s in
          if formatted_str = "quit" then (
            print_quit ();
            exit 0)
          else if formatted_str = "untimed" || formatted_str = "casual" then None
          else (
            print_invalid_time ();
            prompt_time ())
    end

let initial_state = Chess.State.init_state

let undo_command state =
  try State.undo state with
  | NoUndo ->
      ANSITerminal.print_string [ ANSITerminal.red ] "There are no more turns to undo! \n";
      state

let quit_helper state =
  if state = initial_state then print_quit ()
  else
    let color, opponent = if turn state then ("Black", "White") else ("White", "Black") in
    print_resign color opponent

let print_time_left time1 time2 =
  ANSITerminal.printf [ ANSITerminal.cyan ] "White Time: %s \nBlack Time: %s\n" time1 time2

let print_time_over color =
  let turn, opponent = if color then ("Black", "White") else ("White", "Black") in
  ANSITerminal.printf [ ANSITerminal.cyan ] "%s's time has run out. %s won!\n" turn opponent;
  exit 0

let mm_ss_string time =
  let minutes = time /. 60. in
  let minutes_int = int_of_float minutes in
  let seconds = time -. float_of_int (minutes_int * 60) in
  let minutes_str = string_of_int minutes_int in
  let seconds_str = seconds |> int_of_float |> string_of_int in
  minutes_str ^ ":" ^ if seconds < 10. then "0" ^ seconds_str else seconds_str

let pre_input_printing state error times score =
  let helper_printing state error score =
    print_error error;
    let curr_result = State.result state in
    if curr_result = Stalemate then print_stalemate ();
    if State.checkmate state then print_check_mate curr_result ();
    if score then print_scores state;
    let board = State.board state in
    if check board then print_check ();
    ANSITerminal.print_string [] (if turn state then "Black move> " else "White move> ")
  in
  match times with
  | Some (bl_times, wh_times) ->
      if bl_times <= 0.0 then print_time_over true
      else if wh_times <= 0.0 then print_time_over false
      else ();
      print_board state;
      print_time_left (wh_times |> mm_ss_string) (bl_times |> mm_ss_string);
      helper_printing state error score
  | None ->
      print_board state;
      helper_printing state error score

let rec get_current_board state error times score =
  pre_input_printing state error times score;
  let start_time = gettimeofday () in
  let input = read_line () in
  let command =
    try parse input with
    | Malformed -> Move ((-1, -1), (-1, -1))
  in
  let difference = gettimeofday () -. start_time in
  let new_times =
    match times with
    | Some (bl_times, wh_times) ->
        if turn state then Some (bl_times -. difference, wh_times)
        else Some (bl_times, wh_times -. difference)
    | None -> None
  in
  let next_state = command_helper state new_times error command in
  get_current_board next_state None new_times false

and command_helper state times error = function
  | Quit ->
      quit_helper state;
      exit 0
  | Score -> get_current_board state None times true
  | Draw -> draw_helper state times error
  | Reset ->
      print_reset ();
      initial_state
  | Undo -> undo_command state
  | Help ->
      print_help ();
      state
  | Move (start_coord, end_coord) -> (
      try change_state start_coord end_coord state with
      | Board.InvalidPos -> get_current_board state InvalidPos times false
      | State.WrongColor -> get_current_board state WrongColor times false)

and draw_helper state times error =
  print_draw_offer (if turn state then "Black" else "White");
  match read_line () with
  | exception End_of_file -> get_current_board state InvalidText times false
  | str -> begin
      let formatted_str = String.lowercase_ascii str in
      match formatted_str with
      | "yes"
      | "draw" ->
          print_draw_accept (if turn state then "White" else "Black")
      | "no" ->
          print_draw_deny (if turn state then "White" else "Black");
          state
      | _ -> get_current_board state error times false
    end

let rec start_from_file () =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> (
      match String.lowercase_ascii file_name with
      | "regular" -> get_current_board initial_state None (prompt_time ()) false |> print_board
      | "quit" ->
          print_quit ();
          exit 0
      | _ ->
          let print_line_error line =
            ANSITerminal.printf [ ANSITerminal.red ] "Error on line %i: " line
          in
          let state =
            try Fileutil.config file_name with
            | InvalidText line ->
                print_line_error line;
                print_invalid_text ()
            | InvalidPos line ->
                print_line_error line;
                print_invalid_move ();
                exit 0
            | WrongColor line ->
                print_line_error line;
                print_wrong_color ();
                exit 0
          in
          get_current_board state None (prompt_time ()) false |> print_board)

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
        "\nPlease enter a file name, or type Regular to start a normal game. \n";
      start_from_file ()
    with
    | NotFound ->
        print_file_error ();
        main false

(* Execute the game engine. *)
let () = main true
