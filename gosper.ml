#use "algebra.ml";;

(* Notations : *)
(* S_n = sum_k = 0 ^ n (a_k) *)
(* S_n / S_(n-1) doit etre une fraction rationelle pour que l'algorithme fonctionne *)
(* S_n / S_(n-1) est une fraction rationelle <=> a_n/a_(n-1) = R(n) et S_n = alpha(n) a_n*)
(* R et alpha étant des fractions rationelles *)
(* On connait à priori R, il s'agit de calculer alpha *)


(* Exemple : a_n = n^2 *)
(* a_n/a_(n-1) = n^2 / (n-1)^2 = R(n) avec R = X^2 / (X-1)^ 2*)

module C = DRing_Rat;;
module P = Poly(DRing_Rat);;
module Q = Frac(DRing_Rat);;
module Py = Poly(Q.DRing_F);;
P.one;;

let gR = Q.normalise ([((1, 1), 2)], [((1, 1), 0); ((-2, 1), 1); ((1, 1), 2)]);;
print_endline (Q.print gR);;

(* 1iere étape : on écrit R sous la forme : *)
(* R(X) = p(X)q(X) / p(X-1)r(X) avec pour tout k € N : q(X) ^ r(X+k) = 1 *)
(* Il faut donc calculer les polynomes p, q, r *)

let rec calcul (p,q,r) gR = 
  (* Pour tout k € N, q(X) et r(X+k) sont premiers entres deux 
  <=> le polynome en Y : R(q(X), r(X+Y)) (résultant par rapport à X) s'anulle dans N *)


  (* Plongement prend un polynome P € Q[X] et le plonge dans (K[Y])[X] *)
  let plongement p = 
     Py.normalise (List.map (fun (e, d) -> 
       let coef = (([(e, 0)], P.one):Q.frac) in
       (coef, d)) p)
  in
  (* Si p ou q est constant, on a fini *)
  if P.degre q = 0 || P.degre r = 0 then (p,q,r)
  else 
  (* On construit le polynome r(X+Y) *)
  let r' = Py.eval_k (plongement r) (P.x, P.one) in
 
  (* On construit le polynome q(X) sur le corps (Q[Y])[X] *)
  let q' = plongement q in
  let rat_roots = rat_zeros (fst (Py.resultant q' r')) in
 

  let natural (p,q) = q = 1 && p >= 0 in 
  match List.filter natural rat_roots with
  | [] -> (p,q,r)
  | (k, _)::_ -> (* q(X) et r(X+k) ne sont pas premiers entres eux *)
    let r' = P.eval_k r (k,1) in
    let g = P.gcd q r' in
    let q2 = fst (P.div_euclide q g) in
    let r2 = fst (P.div_euclide r (P.eval_k g (-k, 1))) in
    (* p2 = p(X)g(X)g(X-1)...g(X-k+1) *)
    let rec prod i = 
      if i = k then P.one
      else 
        P.mult (P.eval_k g (-i, 1)) (prod (i+1))
    in
    calcul (P.mult p (prod 0),q2,r2)
    
;;

calcul ([((1, 1), 2)], [((1, 1), 0)], [((1, 1), 0)]) gR;;
  
let p = calcul (P.one, fst gR, snd gR) gR;;

P.print (fst p);;
print_string (Py.print a);;

Q.one;;

Py.print (Py.eval_k b (P.x, P.one));;
