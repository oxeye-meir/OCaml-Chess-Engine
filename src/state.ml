open Board
open Piece

exception WrongColor

exception NoUndo

type enemy_pawn_pos = int * int

type player_pawn_pos = int * int

type en_passant = (enemy_pawn_pos * player_pawn_pos) option

type result =
  | Playing of en_passant
  | WhiteWin
  | BlackWin
  | Stalemate

type t = {
  board : Board.t;
  turn : bool;
  result : result;
  prev_state : t option;
}

let init_state = { board = init_board; turn = false; result = Playing None; prev_state = None }

let turn s = s.turn

let board s = s.board

let result s = s.result

let valid_pos (a, b) = a < 8 && a >= 0 && b < 8 && b >= 0

let rec move_out_of_check board piece = function
  | [] -> false
  | pos :: t ->
      let try_current_move =
        try move (position piece) pos None board with
        | InvalidPos -> board
      in
      if check try_current_move || try_current_move = board then
        move_out_of_check board piece t
      else true

let rec try_all_check_moves board = function
  | [] -> true
  | piece :: t ->
      if move_out_of_check board piece (next_moves board piece) then false
      else try_all_check_moves board t

let checkmate state =
  let board = board state in
  let turn = turn state in
  if board = init_board then false
  else
    let same_pieces = board |> if turn then get_black_pieces else get_white_pieces in
    try_all_check_moves board same_pieces

let rec try_move board piece = function
  | [] -> false
  | pos :: t ->
      let result =
        try move (position piece) pos None board with
        | InvalidPos -> board
      in
      result = board

let stalemate_helper board turn =
  let pieces =
    (if turn then get_black_pieces board else get_white_pieces board) |> Array.of_list
  in
  let non_kings = ref [] in
  let king_pos = ref 0 in
  for i = 0 to Array.length pieces - 1 do
    if is_king pieces.(i) then king_pos := i else non_kings := pieces.(i) :: !non_kings
  done;
  let lst = List.map (next_moves board) !non_kings |> List.flatten in
  let king = pieces.(!king_pos) in
  lst = [] && try_all_check_moves board [ king ]

let stalemate state =
  let board = state.board in
  let turn = state.turn in
  stalemate_helper board turn && not (check board)

let en_passant_pawn_check board turn = function
  | [ pos1 ] ->
      let piece_at_pos1 = get_piece board pos1 in
      if is_pawn piece_at_pos1 && color piece_at_pos1 <> turn then Some pos1 else None
  | [ pos1; pos2 ] ->
      let piece_at_pos1 = get_piece board pos1 in
      let piece_at_pos2 = get_piece board pos2 in
      if is_pawn piece_at_pos1 && color piece_at_pos1 <> turn then Some pos1
      else if is_pawn piece_at_pos2 && color piece_at_pos2 <> turn then Some pos2
      else None
  | _ -> raise WrongColor

let en_passant (startx, starty) (endx, endy) board turn =
  if starty = endy then
    let possible_pawn_positions =
      List.filter (fun pos -> valid_pos pos) [ (endx, endy + 1); (endx, endy - 1) ]
    in
    match turn with
    | true ->
        if endx - startx = 2 then
          match en_passant_pawn_check board turn possible_pawn_positions with
          | Some enemy -> Some enemy
          | None -> None
        else None
    | false ->
        if startx - endx = 2 then
          match en_passant_pawn_check board turn possible_pawn_positions with
          | Some enemy -> Some enemy
          | None -> None
        else None
  else None

let undo state =
  match state.prev_state with
  | None -> raise NoUndo
  | Some t -> t

let change_state pos1 pos2 state =
  let previous_state = Some state in
  let currently_en_passant =
    match state.result with
    | Playing en_passant_position -> en_passant_position
    | _ -> None
  in
  let curr_board = board state in
  let curr_piece = get_piece curr_board pos1 in
  if turn state <> color curr_piece then raise WrongColor
  else
    let new_board = Board.move pos1 pos2 currently_en_passant curr_board in
    let new_state =
      { state with board = new_board; turn = not state.turn; prev_state = previous_state }
    in
    let is_checkmate = checkmate new_state in
    if (not is_checkmate) && stalemate new_state then { new_state with result = Stalemate }
    else if is_checkmate then
      { new_state with result = (if turn state then BlackWin else WhiteWin) }
    else
      let en_passant_enemy = en_passant pos1 pos2 state.board state.turn in
      match en_passant_enemy with
      | Some enemy_pos -> { new_state with result = Playing (Some (pos2, enemy_pos)) }
      | None -> { new_state with result = Playing None }

let rec pawns color x y lst =
  if y >= 0 then pawns color x (y - 1) (init_piece "pawn" color x y :: lst) else lst

(* Other functionalitiess TBD *)