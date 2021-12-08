(*1.Toolbox*)

(*Length*)

let rec length list = match list with
    [] -> 0
  |e::list -> 1+(length list);;

(*nth*)

(*nth *)
let nth x l =
  if x<0 then invalid_arg "x must be positive"
  else let rec tmp (x,l) = match (x,l) with
  (_,[])->failwith "list should not be empty"
    |(x,l) when x>length l -> failwith "nth list is too short"
    |(0,e::_)->e
    |(i,_::l)->tmp((i-1),l) in tmp (x,l);;

(*init_list*)

let rec init_list n x =
  if n < 0 then invalid_arg " init_list : n must be a natural "
  else
  match n with
  |0 -> []
  |_ -> x::(init_list(n-1) x);;

(*put_list*)

let put_list v i l =
  if i < 0 then invalid_arg "put_list : index must be a natural"
  else
    let rec aux i l = match (i,l) with
      (_,[]) -> []
      |(0, _::l) -> v::l
      |(i,e::l) -> e::(aux (i-1) l)
    in
    aux i l;;

(*init_board*)

let init_board (l,c) vaL =
  if l < -1 || c < -1 then
    invalid_arg "init_board : both dimensions need to be a natural int"
  else
    init_list l (init_list c vaL);;

 
(*print_board*)

let print_board board =
  let rec aux2 = function
      [] -> ()
    | e::l -> print_char e; aux2 l
  in let rec aux1 = function
        [] -> ()
      | e::l -> aux2 e;print_newline();  aux1 l
  in aux1 board;;


(*get_cell*)

let get_cell (x, y) board =
  nth y (nth x board) ;;

(*put_cell*)


let put_cell cell (x,y) board =
  let rec put_list = function
    | (_, []) -> []
    | (0, e::l) -> cell :: l
    | (n, e::l) -> e :: (put_list ((n-1), l))
  and process_row = function
    | (_, []) -> []
    | (0, e::l) -> (put_list (y,e)) :: l
    | (n, e::l) -> e :: (process_row ((n-1), l))
  in
  process_row (x, board);;



(*let rec put_cell vaL (x,y) board =
  put_list (put_list vaL y (nth x board)) x board;;*)



