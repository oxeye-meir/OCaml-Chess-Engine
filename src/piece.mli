exception EmptySquare
(** Raised when an empty square is passed into a moves function. *)

type t
(** The abstract type of values representing pieces. *)

type color = bool
(** The color of the piece. If true, then the piece is black, else it is white.*)

val init_piece : string -> bool -> int -> int -> t
(** [init name color x y] initializes a piece with [name], [color], and position ([x], [y]). *)

val is_empty : t -> bool
(** [is_empty p] is whether or not piece p is of the Empty variant. *)

val position : t -> int * int
(** [position p] is a tuple representing the position of piece p on the board. *)

val value : t -> int
(** [value p] is the piece's scoring value. *)

val name : t -> string
(** [name t] is the unicode character that represents piece [t]. *)

val is_king : t -> bool
(** [is_king t] is whether or not the piece t is a King. *)

val is_pawn : t -> bool
(** [is_pawn t] is whether or not the piece t is a Pawn. *)

val color : t -> bool
(** [color t] is the color of the piece [t].*)

val moves : t -> int
(** [moves t] is the move counter of the piece [t].*)

val valid_moves : t -> (int * int) list
(** [valid_moves p] is a tuple list representing the possible next moves of piece p on the
    board. *)

val move_piece : int * int -> t -> t
(** [move_piece pos piece] is the piece with its position updated to [pos].*)

(* val to_string : t -> string * [to_string piece] is a string *)
