(** Note to self, here is comprehensive list of all the types of inputs that are allowed 
    and will be parsed into a command. All other inputs will raise Empty or Malformed and 
    therefore the user should be prompted to input a new command. 
    
    Parsed into Color:
    "black" "BLACK" "   bLacK    " "white" "WHITE" "  wHITe   " ... etc. 
    Explanation: The colors "black" and "white" can have their letters be capitalized or
    not. It is also valid to have both leading and trailing whitespace.
    
    Parsed into Move:
    "a1 a2" "a6 h7" " h7     d5 " ... etc.
    Explanation: The input for a move is valid if it contains two words that consist of 
    two characters each. The first character is in a-h inclusive and the second is in 0-7
    inclusive. It's a requirment that the two positions are different. It is also valid 
    to have both leading and trailing whitespace.

    *)

(** MAKE SURE POSITIONS ARE DIFFERENT AND UPDATE THE SPEC*)
(** THEN ADD TEST CASES TO TEST PARSE*)






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

let rec remove_empty lst =
  match lst with
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
       else if num_of_words = 2 (**&& word differ*) then Move ((pos_of_str (List.nth words 0) ), (pos_of_str (List.nth words 1 )))
       else raise Malformed
