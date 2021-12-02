open State

exception NotFound

exception InvalidText of int

exception InvalidPos of int

exception WrongColor of int

let data_dir_prefix = "data" ^ Filename.dir_sep

let pos_of_str str line =
  let char_in_range = function
    | 'a' .. 'h' -> true
    | _ -> false
  in
  let int_in_range = function
    | '1' .. '8' -> true
    | _ -> false
  in
  let ch1 = String.get str 0 in
  let ch2 = String.get str 1 in
  if char_in_range ch1 && int_in_range ch2 && String.length str = 2 then
    (7 - (Char.code ch2 - 49), Char.code ch1 - 97)
  else raise (InvalidText line)

let parse_line state str line =
  let positions = str |> String.split_on_char ' ' |> List.filter (( <> ) "") in
  if List.length positions <> 2 then raise (InvalidText line)
  else
    let pos1 = pos_of_str (List.nth positions 0) line in
    let pos2 = pos_of_str (List.nth positions 1) line in
    change_state pos1 pos2 state

let rec go_thru_state state line = function
  | [] -> state
  | h :: t ->
      let new_state =
        try parse_line state h line with
        | Board.InvalidPos -> raise (InvalidPos line)
        | State.WrongColor -> raise (WrongColor line)
      in
      go_thru_state new_state (line + 1) t

let rec read_each_line channel acc =
  try read_each_line channel ((input_line channel |> String.lowercase_ascii) :: acc) with
  | End_of_file ->
      close_in channel;
      acc

let config file =
  let channel =
    try open_in (data_dir_prefix ^ file ^ ".txt") with
    | _ -> raise NotFound
  in
  List.rev (read_each_line channel []) |> go_thru_state init_state 1