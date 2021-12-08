(*1. Patterns*)

(*1.1.1.Build Line*)

let rec build_line n str =
  if n < 0 then
    failwith "build_line : number of lines must be positive number"
  else
  if n = 0 then ""
  else str^(build_line (n-1) str);;

(*1.1.2.Square*)

let rec square n str =
  if n < (-1)  then
    failwith "square : number of lines must be a positive number"
  else
    let a = n in
    let rec square_aux n str a =
      if n = 0 then ""
      else build_line a str ^ "\n" ^ square_aux (n - 1) str a
    in
    print_string(square_aux n str a);;

(*1.1.3.Square Bis*)

let square2 a (str1,str2) =
  if a < (-1) then
    failwith "square2 : number of lines must be positive number"
  else
    let rec square2_aux b =
      match b with
      |b when a = b -> ""
      |_ -> if b mod 2 = 0 then
          build_line a (str1^str2)^"\n"^square2_aux(b+1)
        else
          build_line a (str2^str1)^"\n"^square2_aux(b+1)
    in
    print_string(square2_aux 0);;

(*1.1.4.Triangle*)

let triangle a str =
  if a < (-1) then failwith "triangle : number of lines must be a positive number"
  else
    let rec triangle_aux a b str =
      if a = 0 then ""
      else build_line b str ^ "\n" ^ triangle_aux(a-1) (b+1) str
    in
    print_string(triangle_aux a 1 str);;

(*1.2.1.Pyramid*)

let pyramid n (str1,str2) =
  if n <= 0 then failwith "pyramid : number of line must be a positive number"
  else
    let b = n in
    let rec pyramid_aux n a b =
      if b = 0 then ""
      else build_line n str1 ^ build_line a str2
           ^ build_line a str2 ^ build_line n str1 ^ "\n"
           ^ pyramid_aux (n-1) (a+1) (b-1)
    in
    print_string(pyramid_aux (n-1) 1 (b));;

(*1.2.2.Cross*)

let cross n (str1, str2) =
  if n <= 0 then failwith "cross : number of line must be a positive number"
  else
    let line i =
      let l1 = build_line i str1
      and l2= build_line(2*(n-i-1)-1) str1
      in
      l1 ^ str2 ^ l2 ^ str2 ^ l1 ^ "\n"
    in
    let rec aux i =
      if i < n - 1 then
        let linebis = line i in
        (print_string linebis;
         aux (i+1);
         print_string linebis)
      else
        let linebis= build_line(n-1) str1 in
        print_string (linebis ^ str2 ^ linebis ^"\n")
    in
    aux 0;;


