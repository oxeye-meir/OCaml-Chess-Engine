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

let rec find_kings result = function
  | [] -> raise InvalidPos
  | h :: t ->
      if is_king h then
        if List.length result = 1 then h :: result else h :: find_kings (h :: result) t
      else find_kings result t

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

let rec try_all_moves board = function
  | [] -> true
  | piece :: t ->
      if move_out_of_check board piece (next_moves board piece) then false
      else try_all_moves board t

let checkmate state =
  let board = board state in
  let turn = turn state in
  if board = init_board then false
  else
    let same_pieces = board |> if turn then get_black_pieces else get_white_pieces in
    try_all_moves board same_pieces

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
  (* let en_passant_state = en_passant pos1 pos2 state.board state.turn in print_endline
     (string_of_bool en_passant_state); *)
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
    if checkmate new_state then
      { new_state with result = (if turn state then BlackWin else WhiteWin) }
    else
      let en_passant_enemy = en_passant pos1 pos2 state.board state.turn in
      match en_passant_enemy with
      | Some enemy_pos -> { new_state with result = Playing (Some (pos2, enemy_pos)) }
      | None -> { new_state with result = Playing None }

let rec pawns color x y lst =
  if y >= 0 then pawns color x (y - 1) (init_piece "pawn" color x y :: lst) else lst

(* Other functionalitiess TBD *)