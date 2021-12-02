(** The type representing the result of a chess game. *)
type result =
  | Playing of ((int * int) * (int * int)) option
  | WhiteWin
  | BlackWin
  | Stalemate

exception WrongColor
(** The exception to be raised when a player tries moving the wrong color pieces. *)

exception NoUndo
(** The exception to be raised when a player tries undoing to a state that does not exist. *)

type t
(** The abstract type representing a state of the game. *)

val init_state : t
(** [init_state] is the initial state of the game. *)

val turn : t -> bool
(** [turn s] is the current turn of the state [s].*)

val board : t -> Board.t
(** [board s] is the current board of the state [s].*)

val result : t -> result
(** [result s] is the result of the state [s]. *)

val graveyard : t -> bool -> Piece.t list
(** [graveyard s c] is the current graveyard, or all the captured pieces, of color [c] in state
    [s]. *)

val score : t -> bool -> int
(** [score s c] is the current score of the color [c] in state [s]. *)

val checkmate : t -> bool
(** [checkmate s turn] is whether or not the current state [s] is in a 'checkmate' state. A
    'checkmate' state happens when either side's [King] piece is being threatened by the
    opposing side's piece(s) and cannot move anywhere. *)

val stalemate : t -> bool
(** [stalemate s] is whether or not the current state [s] is in a 'stalemate' state. *)

val undo : t -> t
(** [undo s] is the previous state of the current state [s]. *)

val change_state : int * int -> int * int -> t -> t
(** [change_state curr_pos new_pos state] is the new state after moving the board of current
    state [s]. Raises: InvalidPos if the attempted change is invalid. *)
