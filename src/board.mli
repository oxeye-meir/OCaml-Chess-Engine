exception InvalidPos
(** Raised when attempting to put the board into an illegal state. *)

type t
(** The abstract type of value representing the board. *)

val init_board : t
(** [init_board] is a board configuration representing the initial state with all of the pieces
    placed in their correct initial positions. *)

val get_piece : t -> int * int -> Piece.t
(** [get_piece board pos] is the piece located on [board] at position [pos]. Raises: InvalidPos
    if [pos] is not a valid position (either x or y is less than 0 or more than 7.) *)

val get_black_pieces : t -> Piece.t list
(** [get_black_pieces b] gets all the black pieces on board [b].*)

val get_white_pieces : t -> Piece.t list
(** [get_white_pieces b] gets all the white pieces on board [b].*)

val check : t -> bool
(** [check b] is whether or not the current board is in a 'check' state. A 'check' state
    happens when either side's [King] piece is being threatened by the opposing side's piece. *)

val next_moves : t -> Piece.t -> (int * int) list
(** [next_moves b p] is a list of coordinates that represent legal positions that a piece p can
    move to on the current board configuration b. *)

val move : int * int -> int * int -> ((int * int) * (int * int)) option -> t -> t
(** [move curr_pos new_pos turn b] is the board configuration after the piece on [curr_pos] in
    board [b] is moved to [new_pos]. Raises: InvalidPos if the attempted move is illegal.
    Examples of illegal moves include moving to a position not possible on the board, moving
    the other player's piece on your turn, moving a piece to somewhere it cannot move.*)

val to_string : t -> string
(** [to_string board] is a string representation of the chess board. *)
