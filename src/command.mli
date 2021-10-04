exception Empty

(* Will document later *)
exception Malformed

type phrase = string list

type command =
  | Go of phrase
  | Quit

val parse : string -> command
