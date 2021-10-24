type t
(** The abstract type of value representing the board. *)
exception InvalidPos

val init_board : t
(** [init_board] is a board configuration representing the initial state with all of the pieces
    placed in their correct initial positions. *)

val turn : t -> bool
(** [turn b] is the current turn of the board b. *)

val next_moves : t -> Piece.t -> (int * int) list
(** [next_moves b p] is a list of coordinates that represent legal positions that a piece p can
    move to on the current board configuration b. *)

val move : t -> int * int -> int * int -> t
(** [move b curr_pos new_pos] is a board configuration after the piece on curr_pos is moved to
    new_pos. Raises: InvalidPos if either curr_pos or new_pos is illegal.*)

val to_string : t -> string
(** [to_string board] is a string representation of the chess board. *)
