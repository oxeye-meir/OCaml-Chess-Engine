exception EmptySquare
(** Raised when an empty square is passed into a moves function. *)

type t
(** The abstract type of values representing pieces. *)

type color = bool
(** The color of the piece. If true, then the piece is black, else it is white.*)

val init : string -> color -> int * int -> t
(** [init n c q] is a piece representing the initial position q of piece p with color c and
    name n on the board. Requires:
    [n is the lowercase English name of the piece. Each English name will map to a Unicode Chess piece character based on the color c value.] *)

val position : t -> int * int
(** [position p] is a tuple representing the position of piece p on the board. *)

val get_name : t -> string
(** [get_name t] is the unicode character that represents piece [t].*)

val valid_moves : t -> (int * int) list
(** [valid_moves p] is a tuple list representing the possible next moves of piece p on the
    board. *)
