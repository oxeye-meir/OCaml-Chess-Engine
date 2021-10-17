exception EmptySquare
(** Raised when an empty square is passed into a moves function. *)

type t
(** The abstract type of values representing pieces. *)

type color = bool
(** The color of the piece. If true, then the piece is black, else it is white.*)

val init_piece : string -> bool -> int -> int -> t
(** [init name color x y] initializes a piece with [name], [color], and position ([x], [y]). *)

val position : t -> int * int
(** [position p] is a tuple representing the position of piece p on the board. *)

val get_name : t -> string
(** [get_name t] is the unicode character that represents piece [t].*)

val valid_moves : t -> (int * int) list
(** [valid_moves p] is a tuple list representing the possible next moves of piece p on the
    board. *)
