exception Malformed

type position = int * int

type command =
  | Move of position * position
  | Reset
  | Quit

let format str = str |> String.trim |> String.lowercase_ascii

let char_in_range = function
  | 'a' .. 'h' -> true
  | _ -> false

let int_in_range = function
  | '1' .. '8' -> true
  | _ -> false

let pos_of_str str =
  let ch1 = String.get str 0 in
  let ch2 = String.get str 1 in
  if char_in_range ch1 && int_in_range ch2 && String.length str = 2 then
    (7 - (Char.code ch2 - 49), Char.code ch1 - 97)
  else raise Malformed

(* Parse needs to be able to throw away inputs like: slkdfngbsd,khgblsdkhfgb *)
(* let parse str = let s = format str in let words = remove_empty (String.split_on_char ' ' s)
   in let num_of_words = List.length words in if num_of_words = 0 then raise Empty else if
   num_of_words = 1 then let word = List.nth words 0 in if word = "quit" then Quit else if word
   = "white" then Color White else if word = "black" then Color Black else raise Malformed else
   if num_of_words = 2 then Move (pos_of_str (List.nth words 0), pos_of_str (List.nth words 1))
   else raise Malformed *)

let rec get_words_no_spaces (str_lst : string list) = List.filter (( <> ) "") str_lst

let parse str =
  let formatted_str = format str in
  let words_list = String.split_on_char ' ' str in
  if formatted_str = "quit" then Quit
  else if formatted_str = "reset" then Reset
  else if String.length formatted_str = 0 || String.length formatted_str = 1 then
    raise Malformed
  else if List.length words_list > 1 then
    let commands_no_spaces = get_words_no_spaces words_list in
    match commands_no_spaces with
    | []
    | [ _ ] ->
        raise Malformed
    | [ a; b ] ->
        if String.length a <> 2 || String.length b <> 2 then raise Malformed
        else
          let pos1 =
            try pos_of_str a with
            | Malformed -> (-1, -1)
          in
          let pos2 =
            try pos_of_str b with
            | Malformed -> (-1, -1)
          in
          Move (pos1, pos2)
    | _ -> raise Malformed
  else raise Malformed