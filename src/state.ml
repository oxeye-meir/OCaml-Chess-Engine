open Board

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

let change_state (x1, y1) (x2, y2) state =
  let curr_board = board state in
  let new_board = Board.move (x1, y1) (x2, y2) (turn state) curr_board in
  { state with board = new_board; turn = not state.turn }

(* Other functionalitiess TBD *)