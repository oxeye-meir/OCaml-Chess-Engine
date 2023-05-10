open Board

type position = int*int

let bot_move (state: State.t) : position * position =
  let board = (State.board state) in
  let pieces =
    (if State.turn state then black_pieces board else white_pieces board)
  in
  let lst = List.map (fun p -> List.map (fun x -> p,x) (next_moves board p)) pieces |> List.flatten in
  let n = Random.int (List.length lst) in
  let selected_piece, selected_dest_pos = List.nth lst n in

  Piece.position selected_piece, selected_dest_pos
