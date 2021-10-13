exception Empty

exception Malformed

type phrase = string list

type command =
  | Go of phrase
  | Quit

let extract_char str index =
  try String.get str index with
  | _ -> raise Malformed

(* [string_to_pos s] is the 0-based tuple of the position on the
   chessboard that [s] represents. Examples: string_to_pos "e4" is (4,
   3). string_to_pos "h8" is (7, 7). string_to_pos "k9" raises
   Malformed. Raises: Malformed if pos is not a valid position. *)
let string_to_pos (s : string) : int * int =
  let ch1 = extract_char s 0 in
  let ch2 = extract_char s 1 in
  ( 'a' |> int_of_char |> ( - ) (int_of_char ch1),
    '0' |> int_of_char |> ( - ) (int_of_char ch2 - 1) )

let parse s =
  let phrase =
    s |> String.trim |> String.lowercase_ascii
    |> String.split_on_char ' '
    |> List.filter (fun str -> str <> "")
  in
  Go phrase
