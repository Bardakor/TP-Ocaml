 (*2. Fractals *)

(*Library*)

Random.init;;

#load "graphics.cma" ;;

open  Graphics  ;;

open_graph "";;

open_graph " 300 x100";;

let draw (x,y) (x2,y2) =
  moveto x y;
  lineto x2 y2;;

(*2.1.1. Mountain*)

let rec mountain n (x,y) (x2,y2) =
  if n = 0 then
    draw (x,y) (x2,y2)
  else
    let peak =
      (x + x2)/2
    and
      height =
      (y + y2)/2 + Random.int(abs(x2-x)/5 + 20) in
    begin
      mountain (n-1) (x,y) (peak, height);
      mountain (n-1) (peak,height) (x2,y2)
    end;;

(*2.1.2. Dragon*)

let rec dragon  n (x,y) (x2,y2) =
  if n = 0 then
    draw (x,y) (x2,y2)
  else
    let peak =
        (x + x2) / 2 + (y2 - y) / 2
      and
        height =
        (y + y2) / 2 - (x2 - x) / 2 in
    begin
      dragon (n-1) (x,y) (peak, height);
      dragon (n-1) (x2,y2) (peak, height)
    end;;

(*2.2.1. Sierpienski's sponge*)

let rec draw_squaresponge n  =
  let (x,y) = current_point() in
  if n < 3 then fill_rect x y n n
  else
    begin
      draw_squaresponge (n/3);
      rmoveto 0 (n/3);

      draw_squaresponge (n/3);
      rmoveto 0 (n/3);

      draw_squaresponge (n/3);
      rmoveto (n/3) 0;

      draw_squaresponge (n/3);
      rmoveto (n/3) 0;

      draw_squaresponge (n/3);
      rmoveto 0 (-n/3);

      draw_squaresponge (n/3);
      rmoveto 0 (-n/3);

      draw_squaresponge (n/3);
      rmoveto (-n/3) 0;

      draw_squaresponge (n/3);
      moveto x y;
    end;;

let sponge (x,y) n =
  clear_graph ();
  moveto x y;
  draw_squaresponge n;;

(*2.2.1. Serpienski's Triangle*)


let siertri (x,y) n =
  moveto x y ;
  set_color blue;
  let rec siertri2 (x,y) n i =
    match n with
      n when n < 1 -> ()
       |_ -> begin
           draw (x,y) (x + n,y);
           draw (x + n,y) (x + n/2, y+i);
           draw (x + n/2, y+i) (x,y);
           siertri2 (x + n/2, y) (n/2) (i/2);
           siertri2 (x,y) (n/2) (i/2);
           siertri2 (x + n/4 ,y + i/2) (n/2) (i/2);
         end
  in
  let i = int_of_float(cos(60.) *.(float_of_int n)) in
  siertri2 (x,y) (n) (-i);;

(*2.3.1. Circles*)

let rec circle x y n =
  if n > 1 then
    begin
      draw_circle x y n;
      circle (x + n / 2) y (n/2);
      circle (x - n / 2) y (n/2)
    end;;

(*2.3.2. Arrow*)

let rec arrow x y n o =
  if n > 0 then
    begin
      fill_circle x y n;
      if o = 'N' then
        begin
          arrow x (y+3*n/2) (n/2) 'N';
          arrow (x+3*n/2) y (n/2) 'E';
          arrow (x-3*n/2) y (n/2) 'W';
        end
      else if o = 'W' then
        begin
          arrow x (y+3*n/2) (n/2) 'N';
          arrow x (y-3*n/2) (n/2) 'S';
          arrow (x-3*n/2) y (n/2) 'W';
        end
      else if o = 'S' then
        begin
          arrow (x+3*n/2) y (n/2) 'E';
          arrow x (y-3*n/2) (n/2) 'S';
          arrow (x-3*n/2) y (n/2) 'W';
        end
      else
        begin
          arrow x (y+3*n/2) (n/2) 'N';
          arrow (x+3*n/2) y (n/2) 'E';
          arrow x (y-3*n/2) (n/2) 'S';
        end
    end
;;



let aux_mandel a b n =
  let rec div x y k =
    if ((x *. x +. y *. y) < 4.) && (k < n) then
      div (x *. x -. y *. y +. a) (2. *. x *. y +. b) (k + 1)
    else
      k
  in div a b 0;;


let mandelbrot n d =
  let rec auxy x = function
    | 384 -> ()
    | y -> begin
        let color = (aux_mandel ((float_of_int x) /. d) ((float_of_int y) /. d) n) * 10
        in
	set_color (rgb (200 - color / 2) (200 - color) (200));
	plot (500 + x) (500 + y);
	auxy x (y+1);
      end
  in
  let rec auxx = function
    | 512 -> ()
    | x -> begin auxy x (-383); auxx (x+1); end
  in
  begin
    auxx (-511);
  end;;

