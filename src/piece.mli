type t
(** The abstract type of values representing pieces. *)

type color = bool
(** The type representing the color of the piece: false represents
    black, true represents white. *)

val position : t -> int * int
(** [position p] is a tuple representing the position of piece p on the
    board. *)

val next_moves : t -> (int * int) list
(** [next_moves p] is a tuple list representing the possible next moves
    of piece p on the board. *)
