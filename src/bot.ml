open Board

type position = int*int

let is_move_valid state (piece_to_move, end_coord) =
  let start_coord = Piece.position piece_to_move in
  try State.change_state start_coord end_coord state |> ignore; true with
    | Board.InvalidPos -> false
    | State.WrongColor -> false
  
let filter_valid_moves state moves =
  List.filter (is_move_valid state) moves


type move = Piece.t * position
type score = float
type move_tree =
  | Node of (move * score * move_tree) list

let str_of_pos p =
  Format.sprintf "%c%d" (Char.chr (Char.code 'a' + snd p)) (8 - fst p)

let str_of_move m =
  let piece_to_move, end_coord = m in
  let start_coord = Piece.position piece_to_move in
  Format.sprintf "%s %s" (str_of_pos start_coord) (str_of_pos end_coord)

let rec string_of_move_tree n =
  match n with
  | Node sub_trees ->
    let s = String.concat "," (List.map (fun (m,s,t) ->
      Format.sprintf "{\"move\": \"%s\",\"score\":%d, \"sub_tree\":%s}" (str_of_move m) (Float.to_int s) (string_of_move_tree t)) sub_trees
      ) in
    "[" ^ s ^ "]"

let next_moves (state: State.t) : move list =
  let board = (State.board state) in
  let pieces =
    (if State.turn state then black_pieces board else white_pieces board)
  in
  let moves = List.map (fun p -> List.map (fun x -> p,x) (next_moves board p)) pieces |> List.flatten in
  let moves = filter_valid_moves state moves in
  moves

let score_of_board (board : Board.t) : score =
Board.fold (
  fun s p -> s +. float_of_int(if Piece.color p then -Piece.value p else Piece.value p)
  ) 0. board

let make_move (state: State.t) (piece_to_move, end_coord:move) : State.t option =
  let start_coord = Piece.position piece_to_move in
  try Some (State.change_state start_coord end_coord state) with
  | Board.InvalidPos -> None
  | State.WrongColor -> None

let rec generate_tree (state: State.t) (depth:int) : move_tree =
  if depth == 0
  then Node []
  else
    let moves = next_moves state in
    let sub_trees = List.map (fun m ->
      match make_move state m with
      | Some next_state ->
        m, (score_of_board (State.board next_state)), (generate_tree next_state) (depth-1)
      | None -> failwith "incorrect bot move"
      ) moves in
    Node sub_trees

let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

let rec minimax (t : move_tree) (color:bool) : (move * score) =
  match t with
  | Node [] ->  failwith "invalid move tree"
  | Node (((m, s, t)::_) as sub_trees) ->
    let cmp = if color then (<) else (>) in
    (* print_endline "minimax"; *)
    List.fold_left (fun (acc_m, acc_s) (m, s, sub_t) ->
      let sub_s = match sub_t with
        | Node [] -> s
        | _ -> snd (minimax sub_t (not color))
      in
      (* Format.printf "move %s, sub_s=%d, acc_s=%d" (str_of_move m) (Float.to_int sub_s) (Float.to_int acc_s); *)
      (* print_endline ""; *)
      if cmp sub_s acc_s
      then m, sub_s
      else (acc_m, acc_s)
      ) (m, if color then 999. else -999.) sub_trees

let bot_move (state: State.t) : Piece.t * position =

  let tree = generate_tree state 3 in
  let oc = open_out "move_tree.json" in
  Printf.fprintf oc "%s" (string_of_move_tree tree);
  close_out oc;
  (* print_endline (string_of_move_tree tree); *)

  fst (minimax tree (State.turn state))

  (* let moves = next_moves state in
  let n = Random.int (List.length moves) in
  let selected_piece, selected_dest_pos = List.nth moves n in

  selected_piece, selected_dest_pos *)
