exception InvalidPos
(** Raised when attempting to put the board into an illegal state. *)

type board
(** The alias given to a matrix of [Piece.t] (i.e. [Piece.t list list]). *)

type t
(** The abstract type of value representing the board. *)

val init_board : t
(** [init_board] is a board configuration representing the initial state with all of the pieces
    placed in their correct initial positions. *)

val turn : t -> bool
(** [turn s] is the current turn of the state [s]. *)

val get_board : t -> board
(** [board s] is the current board of the state [s]. *)

val get_piece : board -> int * int -> Piece.t
(** [get_piece board pos] is the piece located on [board] at position [pos]. Raises: InvalidPos
    if [pos] is not a valid position (either x or y is less than 0 or more than 7.) *)

val check : t -> bool
(** [check b] is whether or not the current board is in a 'check' state. A 'check' state
    happens when either side's [King] piece is being threatened by the opposing side's piece. *)

val checkmate : t -> bool
(** [checkmate b] is whether or not the current board is in a 'checkmate' state. A 'checkmate'
    state happens when either side's [King] piece is being threatened by the opposing side's
    piece(s) and cannot move anywhere. *)

val next_moves : t -> Piece.t -> (int * int) list
(** [next_moves b p] is a list of coordinates that represent legal positions that a piece p can
    move to on the current board configuration b. *)

val move : int * int -> int * int -> t -> t
(** [move curr_pos new_pos b] is the board configuration after the piece on [curr_pos] in board
    [b] is moved to [new_pos]. Raises: InvalidPos if either [curr_pos] or [new_pos] is an
    illegal move. Examples of illegal moves include moving to a position not possible on the
    board, moving the other player's piece on your turn, moving a piece to somewhere it cannot
    move.*)

val to_string : t -> string
(** [to_string board] is a string representation of the chess board. *)
