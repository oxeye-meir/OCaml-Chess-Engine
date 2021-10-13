open Chess

(** Chess.Piece, Chess.Board: pieces =
    [Pawn B/W, Rook B/W, Knight B/W, Bishop B/W, Queen B/W, King B/W, Empty] *)

let print board =
  match board with
  | [] -> raise (Failure "uh oh")
  | h :: t -> List.fold_right (fun piece -> "|" ^ get_name piece) h
