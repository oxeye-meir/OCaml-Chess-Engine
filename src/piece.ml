type color = bool

type piece_info = string * color * int * int

type t =
  | Pawn of piece_info
  | Rook of piece_info
  | Knight of piece_info
  | Bishop of piece_info
  | Queen of piece_info
  | King of piece_info

let position piece =
  match piece with
  | Pawn (_, _, x, y) -> (x, y)
  | Rook (_, _, x, y) -> (x, y)
  | Bishop (_, _, x, y) -> (x, y)
  | Queen (_, _, x, y) -> (x, y)
  | King (_, _, x, y) -> (x, y)
  | Knight (_, _, x, y) -> (x, y)

let get_name t =
  match t with
  | Pawn (color, _, _, _) -> color
  | Knight (color, _, _, _) -> color
  | Rook (color, _, _, _) -> color
  | Bishop (color, _, _, _) -> color
  | Queen (color, _, _, _) -> color
  | King (color, _, _, _) -> color

let next_moves (piece : t) =
  let _ = ignore piece in
  [ (1, 1) ]

let valid_pos (x, y) = not (x > 7 || x < 0 || y > 7 || y < 0)
