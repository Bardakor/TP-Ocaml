#use "list_tools.ml"

(*Exercise 1: Histogram Sort*)

(*add_occ*)

let add_occ n l =
  if n < 0 then invalid_arg "add_occ : i should be > 0"
  else if length l < n then invalid_arg "add_occ : hist too short"
  else let a = 1 + nth n l in
    put_list a n l;;

(*get_hist*)

let get_hist list =
  let max = get_max list in
  let hist = init_list (max + 1) 0 in
  let rec aux hist list = match list with
    |[] -> hist
    |e::list -> let hist = add_occ e hist in
                aux hist list
  in
  aux hist list;;

(*get_sorted*)

let rec sum = function
  | [] -> 0
  | h::t -> h + (sum t)

let get_sorted hist =
  let rec aux hist list n i = match hist with
    [] -> list
    |e::hist ->
      match e with
        0 ->  aux hist list (n+1) i
      |_ -> aux ((e-1)::hist) (put_list n i list) n (i+1)
  in aux hist (init_list (sum hist) 0) 0 0;;

(*hist_sort*)

(*let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)
and insert elem = function
  | [] -> [elem]
  | x :: l -> if elem < x then elem :: x :: l
    else x :: insert elem l;;*)

let hist_sort list=
  if length list = 0 then [] else
  if is_pos list then get_sorted(get_hist(list))
  else invalid_arg "hist_sort: i cannot sort that list";;





