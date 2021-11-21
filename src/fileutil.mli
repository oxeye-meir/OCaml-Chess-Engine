exception NotFound

exception InvalidText of int

exception InvalidPos of int

exception WrongColor of int

val config : string -> State.t
(** [read_file file] is the state that is produced by parsing each line of the file [file].
    Raises: [NotFound] if [file] cannot be found; [InvalidText] if a line in [file] cannot be
    parsed; [InvalidPos] if a line in [file] makes an invalid move; [WrongColor] if a line in
    [file] moves on the wrong color's turn. *)
