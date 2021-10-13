open Piece
(* Board representation: [[(0,0);(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7)];
   [(1,0);(1,1);(1,2);(1,3);(1,4);(1,5);(1,6);(1,7)];
   [(2,0);(2,1);(2,2);(2,3);(2,4);(2,5);(2,6);(2,7)];
   [(3,0);(3,1);(3,2);(3,3);(3,4);(3,5);(3,6);(3,7)];
   [(4,0);(4,1);(4,2);(4,3);(4,4);(4,5);(4,6);(4,7)];
   [(5,0);(5,1);(5,2);(5,3);(5,4);(5,5);(5,6);(5,7)];
   [(6,0);(6,1);(6,2);(6,3);(6,4);(6,5);(6,6);(6,7)];
   [(7,0);(7,1);(7,2);(7,3);(7,4);(7,5);(7,6);(7,7)]] *)

type t = Piece.t list list

(* Helper functions *)
let rec gen_columns_of_row col num_of_cols list =
  if col < num_of_cols then gen_columns_of_row (col + 1) num_of_cols ((0, col) :: list)
  else list

let columns = gen_columns_of_row 0 8 []

let gen_rows columns row = List.map (fun (_, col) -> (row, col)) columns

let rows = gen_rows columns 0

let occupied_squares board = board |> List.flatten |> List.map (fun piece -> position piece)

(* [[rook b;knight b;bishop b;queen b;king b;bishop b;knight b;rook b]; [pawn b;pawn b;pawn
   b;pawn b;pawn b;pawn b;pawn b;pawn b]; [empty;empty;empty;empty;empty;empty;empty;empty];
   [empty;empty;empty;empty;empty;empty;empty;empty];
   [empty;empty;empty;empty;empty;empty;empty;empty];
   [empty;empty;empty;empty;empty;empty;empty;empty]; [pawn w;pawn w;pawn w;pawn w;pawn w;pawn
   w;pawn w;pawn w]; [rook w;knight w;bishop w;queen w;king w;bishop w;knight w;rook w]] *)

(** pieces = [Pawn B/W, Rook B/W, Knight B/W, Bishop B/W, Queen B/W, King B/W, Empty] *)

let init (index : Piece.t list) : t =
  let index = List.nth index in
  [
    [ index 2; index 4; index 6; index 8; index 10; index 6; index 4; index 2 ];
    [ index 0; index 0; index 0; index 0; index 0; index 0; index 0; index 0 ];
    [ index 12; index 12; index 12; index 12; index 12; index 12; index 12; index 12 ];
    [ index 12; index 12; index 12; index 12; index 12; index 12; index 12; index 12 ];
    [ index 12; index 12; index 12; index 12; index 12; index 12; index 12; index 12 ];
    [ index 12; index 12; index 12; index 12; index 12; index 12; index 12; index 12 ];
    [ index 1; index 1; index 1; index 1; index 1; index 1; index 1; index 1 ];
    [ index 3; index 5; index 7; index 9; index 11; index 7; index 5; index 3 ];
  ]

(** val move : t -> Piece.t -> t [move b p] is a board configuration after piece p is moved in
    board b. *)
