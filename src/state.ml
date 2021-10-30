open Board
open Piece

exception WrongColor

type result =
  | Playing
  | WhiteWin
  | BlackWin
  | Stalemate

type t = {
  board : Board.t;
  turn : bool;
  result : result;
}

let init_state = { board = init_board; turn = false; result = Playing }

let turn s = s.turn

let board s = s.board

let result s = s.result

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
        try move (position piece) pos board with
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

let change_state pos1 pos2 state =
  if checkmate state then
    if turn state then { state with result = WhiteWin } else { state with result = BlackWin }
  else
    let curr_board = board state in
    let curr_piece = get_piece curr_board pos1 in
    if turn state <> color curr_piece then raise WrongColor
    else
      let new_board = Board.move pos1 pos2 curr_board in
      { state with board = new_board; turn = not state.turn }

(* Other functionalitiess TBD *)