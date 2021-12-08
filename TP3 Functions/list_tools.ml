(*1. Toolbox*)

(*1.1. Basics*)

(*Length*)

let rec length list = match list with
    [] -> 0
  |e::list -> 1+(length list);;

(*nth*)

let nth x list =
  if x < 0 then invalid_arg "nth : indew must be a natural"
  else
    let rec aux x list = match list with
        [] -> failwith "nth: list is too short"
      |e::list -> if x = 1 then e
        else aux (x-1) list
    in
    aux x list;;

(*is_pos*)

let rec is_pos list = match list with
    [] -> true
  |e::list -> if e > -1 then is_pos list
    else false;;

(*get_max*)


let rec get_max l = match l with
    e::[] -> e
  |e::l -> let max = get_max l in
    if e > max then e else max
  |[] -> invalid_arg "get_max : empty list"

(*1.2. Build - Modify*)

(*init_list*)

let rec init_list n x =
  if n < 0 then invalid_arg " init_list : n must be a natural "
  else
  match n with
  |0 -> []
  |_ -> x::(init_list(n-1) x);;

(*append*)

let append l1 l2 =
  if l2 = [] then
     l1
  else
     let rec append_rec = function
         [] -> l2
       | e::l -> e :: append_rec l
     in
     append_rec l1;;

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

(*1.3. 'a list list*)

(*init_board*)

let init_board (l,c) vaL =
  if l < -1 || c < -1 then
    invalid_arg "init_board : both dimensions need to be a natural int"
  else
    init_list l (init_list c vaL);;

(*is_board*)

(*let rec isboard l = match l with
    []|[] -> true
  |f::e::l-> (if length (f)=length(e) then isboard (f::l)
              else false);;*)

let is_board l =
  let rec aux l n=
    match l with
      [] -> true
    |e::l-> (if n=length(e) then aux (l) n
          else
            false)
  in aux l (length(l));;

(*print_board*)

(*let rec print_list l = match l with
    [] -> ()
  |e::l ->  print_int e; print_string " "; print_list l;;*)

(*let rec print_board board =
  if is_board board = false then invalid_arg "print_board : matrix isn't a matrix"
  else
    match board with
      [] -> ()
    |l::board -> print_list(l) + "\n"; print_board board*)

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

let rec put_cell vaL (x,y) board =
  put_list (put_list vaL y (nth x board)) x board;;



















