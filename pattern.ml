(* These are  my answers to the TP2_algo given in class *)


(* ex : 1.1.1 build a line *)

let rec build_line n str =
  if n <=0 then ""
  else str^build_line(n-1) str;;

(* build_line 5 "*";;*)



(* ex : 1.1.2 build a square *)

(* This version isn't the answer to the build square as it uses for loops

let square n str =
  for i=1 to n do
    for i=1 to n do print_string str done;
    print_newline() done;;
*)

(* the correct  version of square *)



let square n str =
  let build = build_line n str
  in
  let rec aux count =
    if count <= 0 then ""
    else build^"\n" ^aux (count-1) in print_string (aux n );;
(*
square 5 "*";;
 *)


(* ex : 1.1.3 build a square2 (based on the same idea as square) *)

let square2 n (str1,str2) =
  let build1 = build_line n (if str1>str2 then str1^str2 else str2^str1) in
  let build2 = build_line n (if str2<str1 then str2^str1 else str1^str2)in
  let rec aux count=
    if count<=0 then ""
    else( if count mod 2 = 0
    then build1
    else build2) ^"\n"^ aux (count-1) in
  print_string (aux n) ;;

(* square2 6 ("T","*") *)




(* ex : 1.1.4 build a triangle *)

let triangle n str =
  let rec aux count = 
    if count = (n+1) then ""
    else (build_line count str)^"\n" ^aux (count+1)
  in print_string (aux 1 );;

(* triangle 5 "*";; *)




(* Bonus : 1.1.5 build a piramid *)

let piramid n (str1,str2)=
  let rec aux count =
    let build_droite = (build_line (n-count) str1)^
                       (build_line (count) str2 )in
    let build_gauche=(build_line (count) str2 )
                     ^(build_line (n-count) str1) in
    if count <= 0
    then ""
    else build_gauche^build_droite^"\n"^aux (count-1)
  in print_string (aux (n-1)) ;;

(*piramid 5 ("*","-");;*)



(* tous ceci ne fonctionne pas parfaitement 

let rec build_line_cross_d n (str1, str2) line count =
  if n<=0 then ""
  else if n= line
  then str1^build_line_cross_d (n-1) (str1, str2) line (count+1)
  else str2^build_line_cross_d (n-1) (str1, str2) line (count+1);;



let rec build_line_cross_g n (str1, str2) line count=
  if n<=1 then ""
  else if n = line+1
  then (build_line_cross_g (n-1) (str1, str2) line)(count+1)) ^str1
  else (build_line_cross_g (n-1) (str1, str2) line) (count+1)) ^str2;;

let build_line_cross n (str1,str2) line =
  (build_line_cross_g n (str1,str2) line)^
    (build_line_cross_d n (str1, str2) line);;
let build_line_cross_bas n (str1,str2) line =
  (build_line_cross_g n (str1,str2) (line-(line-count))
  ^(build_line_cross_d n (str1, str2) (line-9));;

build_line_cross_bas 5 ("T","r") 6 ;;

let cross n (x1,x2) =
  let rec aux  count =
    if count<= 1 then (build_line (n-1) x1 ^ x2 ^ build_line(n-1) x2)
    else
        let numofx1 = (n-count) in
        let numofx2 = (count-2) in
        let str1 = build_line(numofx1) x1 in
        let str2 = build_line(numofx2) x2 in
        let str = str1^x2^str2^x1^str2^x2^str1^x1 in
        str^aux(count-1) ^str in
        aux 0;;

cross 5 ("s","t")


 *)
