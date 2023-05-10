open Board

type position = int*int

let is_move_valid state (piece_to_move, end_coord) =
  let start_coord = Piece.position piece_to_move in
  try State.change_state start_coord end_coord state |> ignore; true with
    | Board.InvalidPos -> false
    | State.WrongColor -> false
  
let filter_valid_moves state moves =
  List.filter (is_move_valid state) moves

let bot_move (state: State.t) : Piece.t * position =
  let board = (State.board state) in
  let pieces =
    (if State.turn state then black_pieces board else white_pieces board)
  in
  let moves = List.map (fun p -> List.map (fun x -> p,x) (next_moves board p)) pieces |> List.flatten in
  let moves = filter_valid_moves state moves in
  let n = Random.int (List.length moves) in
  let selected_piece, selected_dest_pos = List.nth moves n in

  selected_piece, selected_dest_pos
