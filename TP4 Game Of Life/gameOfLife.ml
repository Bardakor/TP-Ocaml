(*The Game*)

(*Whatever is needed*)

#use "list_tools.ml";;
#load "graphics.cma";;
open Graphics;;
open_graph"";;

(*open_window*)

let open_window size = open_graph (" " ^ string_of_int size ^ "x" ^ string_of_int (size + 20));;

(*color settings*)

let grey = rgb 127 127 127

(*draw_cell*)

let draw_cell (x,y) size color =
  set_color grey;
  draw_rect x y size size;
  set_color color;
  fill_rect (x + 1) (y + 1) (size - 2) (size - 2);;

(*cell color*)

let cell_color = function
  0 -> white
  |_ -> black;;

(*draw_board*)

let draw_board board cell_size =
  let rec tmp board (x,y) = match board with
    [] -> ()
    |l::board when l=[] -> tmp board (x+cell_size,0)
    |l::board -> match l with
        [] -> tmp board (x + cell_size, 0)
      |e::l -> draw_cell (x,y) cell_size (cell_color e);
        tmp(l::board) (x, y + cell_size)
  in tmp board (0,0);;

(*board_matrix*)

let board = [[1;1;1;1;1;1;1;1;1;1];
           [0;0;0;0;0;0;0;0;0;0];
           [1;0;1;0;1;0;1;0;1;0];
           [0;1;0;1;0;1;0;1;0;1];
           [0;0;0;0;0;0;0;0;0;0];
           [1;1;1;1;1;1;1;1;1;1];
           [0;0;0;0;0;0;0;0;0;0];
           [1;0;1;0;1;0;1;0;1;0];
           [0;1;0;1;0;1;0;1;0;1];
           [0;0;0;0;0;0;0;0;0;0]];;

(*display board*)

let test_display board cell_size =
  open_window (length board * cell_size + 40);
  draw_board board cell_size;;

(*The Game*)

(*2.1 rules0*)
let rec rules0 cell n = match cell with
    0 -> if n=3 then 1 else 0
  |1 -> if n=2 || n=3 then 1 else 0
  |_->failwith "cell content not defined";;

(*2.2 count_neighbours*)

let count_neighbours (x,y) board size =
  if board =[[]] then failwith "board must not be empty"
  else
    let tmp (x,y) =  if (x<0 || y<0 || x>=size || y>=size)
      then 0
      else get_cell (x,y) board
    in
    tmp (x-1,y+1)
    + tmp (x,y+1)
    + tmp (x+1,y+1)
    + tmp (x-1,y)
    + tmp (x+1,y)
    + tmp (x-1,y-1)
    + tmp (x,y-1)
    + tmp (x+1,y-1);;

(*Life*)

(*1 seed_life*)
let random = Random.int;;

let seed_life board size nb_cell =
  if board = [] then [[]]
  else
    let rec tmp board nb_cell = match nb_cell with
        0 -> board
      |_-> let (x,y) = (random size, random size) in
           if (get_cell (x,y) board)=1 then tmp board nb_cell
           else tmp (put_cell 1 (x,y) board) (nb_cell-1)
     in tmp board nb_cell;;

(*2 new_board*)

let new_board size nb =
seed_life (init_board (size,size) 0) size nb;;

(*3 next_generation*)

let next_generation board size =
  let rec tmp1 (x,y) new_board = match new_board with
      [] -> []
    |e1::new_board -> let rec tmp2 (x,y) e = match e with
        [] -> []
        |e2::e1 -> (rules0(get_cell (x,y) board) (count_neighbours (x,y) (board) size))::tmp2 (x,y+1) e1
                     in tmp2 (x,y) e1::(tmp1 (x+1,y) new_board)
  in tmp1 (0,0) board;;

(*4 game*)

let cell_size = 10;;

let rec game board size n = match n with
    0 -> test_display board cell_size
  |_ -> test_display board cell_size;
    game (next_generation board size) size (n-1);;

(*5 new_game*)

let rec new_game size nb_cell n =
  open_window size;
  game (new_board size nb_cell) size n;;

(*3.Bonus*)

(*Input/Output*)

(*1 load_board*)
let write filename list =
  let oc = open_out filename in
  let rec aux = function
  [] -> close_out oc
    |e::l -> Printf.fprintf oc "%s " e; aux l
  in aux list;;

let load name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop () = match try_read () with
      Some s -> s::(loop ())
    |None -> close_in ic; []
  in loop ();;

let load_board filename =
  let tmp filename l =
    (load filename)::l in tmp filename [];;

(*save_board*)

let save_board filename board =
  let rec tmp filename board i = match board with
      [] -> []
    |e::board -> write filename e;tmp filename board (i-1)
  in tmp filename board (length board);;

(*remaining*)

let remaining board =
  let rec tmp board (x,y) = match board with
      [] -> false
    |l::board when l=[] -> tmp board (x+1,0)
    |l::board -> match l with
        [] -> tmp board (x+1,0)
        |e::l -> if e=1 then true else tmp board (x,y+1)
  in tmp board (0,0);;

(*new_game*)

let rec infinite board size = if remaining board then test_display board cell_size
  else test_display board cell_size; infinite (next_generation board size) size;;

let bonus_new_game size nb_cell n =
  open_window size;
  if n=0 then infinite board size else
    let rec tmp size nb_cell n = match n with
        0 -> test_display board cell_size
      |_ -> game (new_board size nb_cell) size (n-1)
    in tmp size nb_cell n;;ca
