type t =
  | Pawn of string * int * int
  | Rook of string * int * int
  | Knight of string * int * int
  | Bishop of string * int * int
  | Queen of string * int * int
  | King of string * int * int

let position piece  =
  match piece with
  |Pawn (_, x, y) -> (x, y)
  | Rook (_, x, y) -> (x, y)
  | Bishop (_, x, y) -> (x, y)
  | Queen (_, x, y) -> (x, y)
  | King (_, x, y) -> (x, y)
  | Knight (_, x, y) -> (x, y)
  | _ -> raise (Failure "uh oh")

let get_name t =
  match t with
  | Pawn (color, _, _) -> color
  | Knight (color, _, _) -> color
  | Rook (color, _, _) -> color
  | Bishop (color, _, _) ->  color
  | Queen (color, _, _) -> color
  | King (color, _, _) ->  color
  | _ -> raise (Failure "uh oh")

let next_moves (piece : t) =
  let _ = ignore piece in
  [ (1, 1) ]
