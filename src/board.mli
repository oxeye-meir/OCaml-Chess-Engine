type t
(** The abstract type of value representing the board. *)

val init_board : t
(** [init_board] is a board configuration representing the initial state with all of the pieces
    placed in their correct initial positions. *)

val next_moves : t -> Piece.t -> (int * int) list
(** [next_moves b p] is a list of coordinates that represent legal positions that a piece p can
    move to on the current board configuration b. *)

val move : t -> Piece.t -> t
(** [move b p] is a board configuration after piece p is moved in board b. *)

val to_string : t -> string
(** [to_string board] is a string representation of the chess board. *)
