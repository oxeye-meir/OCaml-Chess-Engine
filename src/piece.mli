type t
(** The abstract type of values representing pieces. *)

val position : t -> int * int
(** [position p] is a tuple representing the position of piece p on the
    board. *)

val get_name : t -> string
(** [get_name t] is the unicode character that represents piece [t].*)

val next_moves : t -> (int * int) list
(** [next_moves p] is a tuple list representing the possible next moves
    of piece p on the board. *)
