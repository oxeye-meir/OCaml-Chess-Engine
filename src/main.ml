open Board

let print board = match board with
| [] -> raise (Failure("uh oh"))
| h :: t -> List.fold_right(fun piece -> "|" ^ get_name piece) h