open Board

type position = int*int

let is_move_valid state (piece_to_move, end_coord) =
  let start_coord = Piece.position piece_to_move in
  try State.change_state start_coord end_coord state |> ignore; true with
    | Board.InvalidPos -> false
    | State.WrongColor -> false
  
let filter_valid_moves state moves =
  List.filter (is_move_valid state) moves

let score_of_board (board : Board.t) : int =
  Board.fold (fun score p ->
    let piece_value =
      if Piece.color p then Piece.value p else - (Piece.value p)
    in
    score + piece_value
    ) 0 board



let bot_move (state: State.t) : Piece.t * position =
  let board = (State.board state) in
  let pieces =
    (if State.turn state then black_pieces board else white_pieces board)
  in
  let moves = List.map (fun p -> List.map (fun x -> p,x) (next_moves board p)) pieces |> List.flatten in
  let moves = filter_valid_moves state moves in

  let first_move =
    match moves with
    | [] -> failwith "panic"
    | m::_ -> (if State.turn state then -999 else 999), m
  in

  let best_move = List.fold_left (fun (acc_score, acc_move) m ->
    let piece_to_move, end_coord = m in
    let start_coord = Piece.position piece_to_move in
    let next_state = State.change_state start_coord end_coord state in
    let s = score_of_board (State.board next_state) in
    let cmp = if State.turn state then (<) else (>) in
    if cmp acc_score s
    then (acc_score, acc_move)
    else (s,m)
    ) first_move moves in

  snd best_move
