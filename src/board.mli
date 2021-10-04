type t
(** The abstract type of value representing the board. *)

val init : t -> Piece.t list -> t
(** [init b p] is a board configuration representing the initial state
    of board b, with all of the pieces in p placed in their correct
    initial positions. *)

val move : t -> Piece.t -> t
(** [move b p] is a board configuration after piece p is moved in board
    b. *)
