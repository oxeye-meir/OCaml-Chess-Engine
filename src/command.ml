exception Empty

exception Malformed

type color = Black | White

type position = int * int

type command =
  | Color of color
  | Move of position * position
  | Quit 

let format str = 
  str |> String.trim |> String.lowercase_ascii

let char_in_range = function
  | 'a' .. 'h' -> true
  |  _ -> false

let int_in_range = function
  | '0' .. '7' -> true
  |  _ -> false

let pos_of_str str = 
  let ch1 = String.get str 0 in
  let ch2 = String.get str 1 in
  if char_in_range ch1 && int_in_range ch2 && String.length str = 2 then (Char.code ch1 - 97, Char.code ch2 - 48)
  else raise Malformed

let rec remove_empty = function
  | [] -> []
  | h::t -> if h = "" then remove_empty t else h::remove_empty t

let parse str = 
  let s = format str in
    let words = remove_empty (String.split_on_char ' ' s) in
      let num_of_words = List.length words in
       if num_of_words = 0 then raise Empty
       else if num_of_words = 1 then let word = List.nth words 0 in
          if word = "quit" then Quit
          else if word = "white" then Color White
          else if word = "black" then Color Black
          else raise Malformed
       else if num_of_words = 2 then Move ((pos_of_str (List.nth words 0) ), (pos_of_str (List.nth words 1 )))
       else raise Malformed

