(* 
   ========
   Exemples
   ========
 
*)

open Algebra

(* Calcul des zéros d'un polynome rationnel *)
let p = P.normalise [((5,1),7); ((11,12),6); ((-229,4),5); ((257,4),4); ((-137,4),3); ((190,3),2); ((28,1),1)];;
P.rat_zeros p

(* Calcul du résultant de deux polynomes : *)
let p = [((1,1), 0); ((1, 1), 2)];;
let q = [((3,1), 0);((2,1), 1); ((1,1), 4)];;
 P.resultant p  q;; (* Renvoit 20 (Mathematica confirme) *)
 P.resultant [((1, 1), 1)] [((1, 1), 2)];; (* Renvoit 0 car les deux polynomes ne sont pas premiers entres eux *)


(* Calcul du polynome caractéristique d'un endomorphisme *)
module M = Matrix(DRing_Rat);;
let m = [|[|(0,1); (4,1)|];
	  [|(4,3); (3,1)|];
	|];;
print_endline (P.print (M.pc m));;



(* Calcul du determinant d'une matrice 3*3 de rationnels *)

module M = Matrix(DRing_Rat);;

let m = [|[|(0,1); (4,1); (-7,2)|];
	  [|(4,3); (3,1); (1,1)|];
	  [|(2,1); (0,1); (4,3)|]
	|];;
M.det m;; 
M.print m;;

(* Résolution d'un système linéaire, explicitation d'une solution *)
let m = [|[|(0,1); (1,1); (-1,1); (1,1)|];
	  [|(0,1); (0,1); (2,1); (-3,1)|];
	  [|(0,1); (0,1); (0,1); (3,1)|];
	   [|(0,1); (0,1); (0,1); (0,1)|];
	|];;
let v = [|[|(0,1)|]; [|(0,1)|]; [|(1,1)|]; [|(0,1)|]|];;

let m = [|[|(1,1); (-1,1); (2,1)|];
	  [|(3,1); (2,1); (1,1)|];
	  [|(6,1); (4,1); (2,1)|]
	|];;
let v = [|[|(5,1)|]; [|(10,1)|]; [|(10,1)|]|];;


let m', v' = M.forme_echelonnee m v;; 
M.find_a_solution m v;;
M.print m';;
M.print v';;





(* Exemple d'un polynome à deux indeterminées *)
module Q = Frac(DRing_Rat);;
module Qx = Q.DRing_F;;
module Qxy = Frac(Qx);;

let y =  ([((1, 1), 1)], [((1, 1), 0)]);;
(* YX / (1+X^2) *)
print_string (Qxy.print (Qxy.normalise ([(y, 1)], [(Qx.one, 0); (Qx.one,2)])));;
