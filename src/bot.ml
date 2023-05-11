open Board

type position = int*int

let is_move_valid state (piece_to_move, end_coord) =
  let start_coord = Piece.position piece_to_move in
  try State.change_state start_coord end_coord state |> ignore; true with
    | Board.InvalidPos -> false
    | State.WrongColor -> false
  
let filter_valid_moves state moves =
  List.filter (is_move_valid state) moves


type move = Piece.t * position
type score = float
type move_tree =
  | Node of (move * score * move_tree) list

let next_moves (state: State.t) : move list =
  let board = (State.board state) in
  let pieces =
    (if State.turn state then black_pieces board else white_pieces board)
  in
  let moves = List.map (fun p -> List.map (fun x -> p,x) (next_moves board p)) pieces |> List.flatten in
  let moves = filter_valid_moves state moves in
  moves

let score_of_board (board : Board.t) : score =
Board.fold (
  fun s p -> s +. float_of_int(if Piece.color p then Piece.value p else -Piece.value p)
  ) 0. board

let make_move (state: State.t) (piece_to_move, end_coord:move) : State.t option =
  let start_coord = Piece.position piece_to_move in
  try Some (State.change_state start_coord end_coord state) with
  | Board.InvalidPos -> None
  | State.WrongColor -> None

let rec generate_tree (state: State.t) (depth:int) : move_tree =
  if depth == 0
  then Node []
  else
    let moves = next_moves state in
    let sub_trees = List.map (fun m ->
      match make_move state m with
      | Some next_state ->
        m, (score_of_board (State.board state)), (generate_tree state) (depth-1)
      | None -> failwith "incorrect bot move"
      ) moves in
    Node sub_trees

(* let max (state: State.t) moves =
  List.fold_left ()  *)

let bot_move (state: State.t) : Piece.t * position =
  let moves = next_moves state in
  let n = Random.int (List.length moves) in
  let selected_piece, selected_dest_pos = List.nth moves n in

  selected_piece, selected_dest_pos
