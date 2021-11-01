exception Malformed
(** Raised when the inputed command is malformed. *)

type position = int * int
(** The type [position] represents a position on the board (x,y) where x and y are both integer
    in the range 0-7 inclusive. *)

(** The type [command] is a player's command which is then parsed into either the [Reset]
    command that resets the game, the [Quit] command, or the [Move] command, which represents
    the start and end position that a player wants to move. *)
type command =
  | Move of position * position
  | Reset
  | Quit
  | Undo
  | Help

val parse : string -> command
(** [parse str] parses a player's input [str] into a [command]. A command is valid if it is not
    [Malformed]. See the examples below.

    Examples:

    - [parse "a3 c6"] is [Move (5,0),(2,2)]
    - [parse "rEsEt"] is [Reset].
    - [parse "Quit"] is [Quit].
    - [parse "Undo"] is [Undo].
    - [parse "hELp"] is [Help].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space characters (only ASCII
    character code 32; not tabs or newlines, etc.).

    Raises: [Malformed] if [str] is malformed. An input string is malformed if it is empty
    after being trimmed, it is not some form of "quit" or "reset", or it is not 2 valid Chess
    coordinates.*)
