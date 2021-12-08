#use "list_tools.ml";;

(*Exercise 2: Patterns and matrices*)

(*mat_cross*)

let mat_cross a x y =
  if a <= 1 || a mod 2 = 0 then invalid_arg "mat_cross : n must be odd and < 1"
  else
    let rec aux hist n = match n with
        _ when n = a -> hist
      |_ when n = (a/2) -> aux (put_list (init_list a x) n hist) (n+1)
      |_ -> aux (put_cell x (n, a/2) hist) (n+1)
    in aux (init_board (a,a) y) 0;;

(*mat_cross_diag*)

let mat_cross_diag a x y =
  if a <= 1 then invalid_arg "mat_cross_diag : n must be > 1"
  else let rec aux hist n = match n with
        _ when n = a -> hist
      |_ -> aux(put_cell x (n,n) (put_cell x (n, a-n-1) hist)) (n+1)
    in aux (init_board (a,a) y) 0;;

(*mat_square*)

let mat_square a x y =
  if a <= 1 then invalid_arg "mat_square : n must be > 1"
  else let rec aux board n = match n with
        _ when n >= a*a -> board
      |_ when n mod 2 <> 0 -> aux(put_cell x (n/a, n mod a) board) (n + 2)
      |_ when n/a = 0 -> aux (put_cell x (n/a, n mod a) board) (n + 2)
      |_ -> aux (put_cell x (n/a, (n + 1) mod a) board) (n + 2)
    in aux (init_board (a,a) y) 0;;

(*mat_pattern_batch*)








