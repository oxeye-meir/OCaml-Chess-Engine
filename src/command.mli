exception Empty
(** Raised when the inputed command is empty. *)

exception Malformed
(** Raised when the inputed command is malformed. *)

type color = Black | White
(** The type [color] represents the color of the piece that a player chooses
    at the start of the game. *)

type position = int * int
(** The type [position] represents a position on the board (x,y) where x and y
    are both integer in the range 0-7 inclusive. *)

type command =
  | Color of color
  | Move of position * position
  | Quit
(** The type [command] is a player's command which is then parsed into either a 
    color of the piece the player chose, the quit command or the position to which a 
    player wantes to move a piece. There will be more commands later on - these are 
    the only commands for now. *)

val parse : string -> command
(** [parse str] parses a player's input [str] into a [command], as follows.
    If [str] is one word, it is assumed that the user is inputting either the 
    color of the piece or the quit command. Their input is checked to be a valid
    color (white or black) or quit, if their input is valid, the corresponding 
    command is returned.
    If [str] is two words, it is assumed to be a move from one position to another. 
    The positions are checked to be valid and if they are, the move command is returned.

    Examples:
    - [parse "a3 c6"] is [Move (0,3),(2,6)]
    - [parse "Black"] is [Color Black].
    - [parse "Quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if [str] is malformed. An input string is
    malformed if it is not either two valid positions where each position
    consists of a character from a-h inclusive and an int from 0-7 inclusive
    and positions are seperated by at least one space, nor string "white", 
    "black", or "quit" (where letters can be capital, lowercase, and spaces 
    can lead and trail the string). *)
