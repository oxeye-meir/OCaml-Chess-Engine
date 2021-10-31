open Piece

type t = Piece.t list list

exception InvalidPos

(* Helper functions *)
let invalid_pos (x, y) = x < 0 || x > 7 || y < 0 || y > 7

let rec replace_piece piece y = function
  | [] -> raise InvalidPos
  | h :: t -> if y = 0 then piece :: t else h :: replace_piece piece (y - 1) t

let rec replace_row piece x y = function
  | [] -> raise InvalidPos
  | h :: t ->
      if x = 0 then replace_piece piece y h :: t else h :: replace_row piece (x - 1) y t

let row_to_string row num =
  List.fold_left (fun acc piece -> acc ^ name piece ^ " |") (string_of_int num ^ " |") row

let rec empty_squares color x y lst =
  if y >= 0 then empty_squares (not color) x (y - 1) (init_piece "empty" color x y :: lst)
  else lst

let rec pawns color x y lst =
  if y >= 0 then pawns color x (y - 1) (init_piece "pawn" color x y :: lst) else lst

let same_row (r1, _) (r2, _) = r1 = r2

let same_column (_, c1) (_, c2) = c1 = c2

let enemies p1 p2 = Piece.color p1 <> Piece.color p2

let backrank color x =
  [
    init_piece "rook" color x 0;
    init_piece "knight" color x 1;
    init_piece "bishop" color x 2;
    init_piece "queen" color x 3;
    init_piece "king" color x 4;
    init_piece "bishop" color x 5;
    init_piece "knight" color x 6;
    init_piece "rook" color x 7;
  ]

let get_piece board (x, y) =
  if invalid_pos (x, y) then raise InvalidPos
  else
    let row = List.nth board x in
    List.nth row y

let init_board =
  [
    backrank true 0;
    pawns true 1 7 [];
    empty_squares false 2 7 [];
    empty_squares true 3 7 [];
    empty_squares false 4 7 [];
    empty_squares true 5 7 [];
    pawns false 6 7 [];
    backrank false 7;
  ]

let get_pieces_with_condition condition board =
  let pieces = List.flatten board in
  List.filter condition pieces

let get_black_pieces board =
  get_pieces_with_condition (fun piece -> (not (is_empty piece)) && color piece) board

let get_white_pieces board =
  get_pieces_with_condition (fun piece -> not (is_empty piece || color piece)) board

let empty_position board (x, y) = is_empty (get_piece board (x, y))

let enemy_position board color (x, y) =
  let enemy_color = (x, y) |> get_piece board |> Piece.color in
  (not (empty_position board (x, y))) && color <> enemy_color

let rec valid_check board piece_color (x, y) transformation lst =
  let x2, y2 = transformation (x, y) in
  if x2 < 0 || x2 > 7 || y2 < 0 || y2 > 7 then lst
  else
    let pos_added = (x2, y2) :: lst in
    if empty_position board (x2, y2) then
      valid_check board piece_color (x2, y2) transformation pos_added
    else if enemy_position board piece_color (x2, y2) then pos_added
    else lst

let position_printer (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to pretty-print each element
    of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let diagonal_check (startx, starty) (endx, endy) =
  (endx = 1 + startx && endy = 1 + starty)
  || (endx = 1 + startx && endy = starty - 1)
  || (endx = startx - 1 && endy = starty + 1)
  || (endx = startx - 1 && endy = starty - 1)

let pawn_moves board piece (x, y) =
  let startx, starty = position piece in
  if diagonal_check (startx, starty) (x, y) then enemy_position board (Piece.color piece) (x, y)
  else (x, y) |> get_piece board |> is_empty

let move_filter board piece =
  if is_pawn piece then List.filter (fun pos -> pawn_moves board piece pos)
  else if Piece.name piece <> "♞" && Piece.name piece <> "♘" && not (is_king piece) then
    let position = Piece.position piece in
    let color = Piece.color piece in
    let total_lst =
      valid_check board color position (fun (x, y) -> (x - 1, y)) []
      |> valid_check board color position (fun (x, y) -> (x + 1, y))
      |> valid_check board color position (fun (x, y) -> (x, y - 1))
      |> valid_check board color position (fun (x, y) -> (x, y + 1))
      |> valid_check board color position (fun (x, y) -> (x - 1, y - 1))
      |> valid_check board color position (fun (x, y) -> (x - 1, y + 1))
      |> valid_check board color position (fun (x, y) -> (x + 1, y - 1))
      |> valid_check board color position (fun (x, y) -> (x + 1, y + 1))
    in
    List.filter (fun x -> List.mem x total_lst)
  else
    List.filter (fun x -> empty_position board x || enemy_position board (Piece.color piece) x)

let next_moves board piece =
  let possible_moves =
    try valid_moves piece with
    | EmptySquare -> []
  in
  (move_filter board piece) possible_moves

let rec enemy_king_check board curr_color = function
  | [] -> false
  | pos :: t ->
      let piece_at_pos = get_piece board pos in
      if is_king piece_at_pos then
        let piece_at_pos_color = color piece_at_pos in
        if piece_at_pos_color <> curr_color then true else enemy_king_check board curr_color t
      else enemy_king_check board curr_color t

let rec enemy_under_check board = function
  | [] -> false
  | h :: t ->
      let piece_possible_moves = next_moves board h in
      let piece_color = color h in
      if enemy_king_check board piece_color piece_possible_moves then true
      else enemy_under_check board t

let check board = board |> List.flatten |> enemy_under_check board

let move (x1, y1) (x2, y2) en_passant board =
  match en_passant with
  | Some (prev_pawn_pos, enemy_pawn_pos) ->
      print_endline "enpassant";
      let prev_en_passant_pawn_row = fst prev_pawn_pos in
      let prev_en_passant_pawn_col = snd prev_pawn_pos in
      if (x1, y1) = enemy_pawn_pos then
        let pawn_initial = get_piece board (x1, y1) in
        let pawn_moved = move_piece (x2, y2) pawn_initial in
        let new_board =
          replace_row
            (init_piece "empty" false prev_en_passant_pawn_row prev_en_passant_pawn_col)
            prev_en_passant_pawn_row prev_en_passant_pawn_col board
          |> replace_row (init_piece "empty" false x1 y1) x1 y1
          |> replace_row pawn_moved x2 y2
        in
        new_board
      else
        let curr_piece = (x1, y1) |> get_piece board in
        if next_moves board curr_piece |> List.mem (x2, y2) |> not then raise InvalidPos
        else
          let new_piece = get_piece board (x2, y2) in
          let curr_piece_moved = move_piece (x2, y2) curr_piece in
          let new_board =
            if is_empty new_piece then
              let new_piece_moved = move_piece (x1, y1) new_piece in
              replace_row new_piece_moved x1 y1 board |> replace_row curr_piece_moved x2 y2
            else
              replace_row (init_piece "empty" false x1 y1) x1 y1 board
              |> replace_row curr_piece_moved x2 y2
          in
          let next_color_pieces =
            get_pieces_with_condition
              (fun p -> color p = not (Piece.color curr_piece))
              new_board
          in
          if enemy_under_check new_board next_color_pieces then raise InvalidPos else new_board
  | None ->
      let curr_piece = (x1, y1) |> get_piece board in
      if next_moves board curr_piece |> List.mem (x2, y2) |> not then raise InvalidPos
      else
        let new_piece = get_piece board (x2, y2) in
        let curr_piece_moved = move_piece (x2, y2) curr_piece in
        let new_board =
          if is_empty new_piece then
            let new_piece_moved = move_piece (x1, y1) new_piece in
            replace_row new_piece_moved x1 y1 board |> replace_row curr_piece_moved x2 y2
          else
            replace_row (init_piece "empty" false x1 y1) x1 y1 board
            |> replace_row curr_piece_moved x2 y2
        in
        let next_color_pieces =
          get_pieces_with_condition (fun p -> color p = not (Piece.color curr_piece)) new_board
        in
        if enemy_under_check new_board next_color_pieces then raise InvalidPos else new_board

let sep = "\n  -------------------------\n"

let rec to_string_helper board num =
  match board with
  | [] -> ""
  | h :: t -> row_to_string h num ^ sep ^ to_string_helper t (num - 1)

let rec to_string (board : t) = "   A  B  C  D  E  F  G  H " ^ sep ^ to_string_helper board 8