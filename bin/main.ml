open Chess
open Piece
open Board

(** Chess.Piece, Chess.Board: pieces =
    [Pawn B/W, Rook B/W, Knight B/W, Bishop B/W, Queen B/W, King B/W, Empty] *)

let print_board board = board |> to_string |> print_string
