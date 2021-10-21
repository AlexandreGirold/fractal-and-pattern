#use "topfind";;
#require "graphics";;


open Graphics ;;
open_graph " 1200x800";;

(* execice mountain *)

let mountain n =
  clear_graph ();
  let rec aux compt (x,y)(z,t)=
    if compt<=0 then (moveto x y; lineto z t)
    else let m = (x+z)/2 in
         let h = ((y+t)/2) + Random.int(abs (z-x)/5+20) in
         aux (compt-1)(x,y)(m,h);
         aux (compt-1)(m,h)(z,t)
  in aux n (150,20)(1200,10);;


(*exercice dragon *)

let dragon n cord1 cord2 =
  clear_graph ();

  let point_r (x,y)(z,t)=
    let u = (x+z)/2 +(t-y)/2 in
    let v = (y+t)/2 -(z-x)/2 in (u,v) in

    let rec aux n (x,y)(z,t) =
      if n<=0 then (moveto x y ;
                    lineto z t )
      else
        let (u,v) = point_r (x,y)(z,t)
        in
        aux (n-1) (x,y)(u,v);
        aux (n-1) (z,t)(u,v)
           in aux n cord1 cord2;;



(*exercice on the sponge*)
(*we make use of the multiplication 3 because each square has a lenght of n/3 *)

let mult n=
  let p = 1 in
  let rec mult3 n p =
    if n < p*3 then
      p
    else
      mult3 n (p*3) in mult3 n p;;

let sponge (x, y) n =
  clear_graph();
  let n = mult n in
  clear_graph();
  set_color blue;
  fill_rect x y n n;
  set_color black;
  moveto x y;
  let rec draw_sponge n =
    let (u, v) = current_point () in
    if n <= 1 then
      ()
    else
      begin
        let n = n/3 in
        moveto u v;
        draw_sponge n;
        moveto (u+n) v;
        draw_sponge n;
        moveto(u+2*n) v;
        draw_sponge n;
        moveto u (v+n);
        draw_sponge n;
        fill_rect (u+n) (v+n) n n;
        moveto (u+2*n) (v+n);
        draw_sponge n;
        moveto u (v+2*n);
        draw_sponge n;
        moveto (u+n) (v+2*n);
        draw_sponge n;
        moveto (u+2*n) (v+2*n);
        draw_sponge n;
      end in
  draw_sponge n;;





let drawline (x,y)(z,t) =
  moveto x y;
  lineto z t;;

(* exercice on the triangle*)
(*fixing 3 points to be the sommet of the triangle*)

let triangle_fr (x,y) n =
  clear_graph ();
  let rec aux  n x y = if n>9 then
                         let a = (x + (n/2),y) in
                         let b = (x + ((3*n)/4),y + n/2)in
                         let c = (x + (n/4),y + n/2)in
                         let m = (n/2)
                         in
                         begin
                           drawline a b;
                           drawline b c;
                           drawline c a;
                           aux  m x y;
                           let (u,v) = a in
                           aux  m u v;
                           let (u,v) = c in
                           aux  m u v;
                         end
                       else ()
  in
  let cord  = (x,y) in
  drawline cord (x+n,y);
  drawline (x+n,y) (x+(n/2),y +n);
  drawline cord (x+(n/2),y +n);
  aux  n x y ;;



(*bonuses*)

let drawcircle (x,y) r=
  clear_graph ();
  let rec aux x y r=

    if r<=0 then
      draw_circle x y r

    else
      begin
        aux (x-(r/2)) y (r/2);
        draw_circle x y r;
        aux (x+(r/2)) y (r/2);
        draw_circle x y r
      end
  in aux x y r;;


