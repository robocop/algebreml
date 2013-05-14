open Algebra;;
(* Notations : *)
(* S_n = sum_k = 0 ^ n (a_k) *)
(* S_n / S_(n-1) doit etre une fraction rationelle pour que l'algorithme fonctionne *)
(* S_n / S_(n-1) est une fraction rationelle <=> a_n/a_(n-1) = R(n) et S_n = alpha(n) a_n*)
(* R et alpha étant des fractions rationelles *)
(* On connait à priori R, il s'agit de calculer alpha *)


(* Exemple : a_n = n^2 *)
(* a_n/a_(n-1) = n^2 / (n-1)^2 = R(n) avec R = X^2 / (X-1)^ 2*)

module C = DRing_Rat;;
module Q = Frac(DRing_Rat);;
module Py = Poly(Q.DRing_F);;
let natural (p,q) = q = 1 && p >= 0;;
(*

let gR = Q.normalise ([((1, 1), 2)], [((1, 1), 0); ((-2, 1), 1); ((1, 1), 2)]);;
print_endline (Q.print gR);;

*)
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
  let rat_roots = P.rat_zeros (fst (Py.resultant q' r')) in
 

  
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
    calcul (P.mult p (prod 0),q2,r2) gR
    
;;

let calcul_pqr gR = calcul (P.one, fst gR, snd gR) gR;;
(*  
let p, q, r = calcul_pqr gR;;
*)
(* 2ième étape : majoration du degre de f(X) *)
(* f(X) polynome vérifiant : p(X) = q(X+1)f(X) - f(X-1)r(X) *)
(* On a alors alpha(X) = q(X+1)/ p(X) * f(X) *)

(* Majoration du degré de f *)
(* On pose s+(X) = q(X+1) + r(X) et s-(X) = q(X+1) - r(X) *)

(* 1ier cas : deg s-(X) != deg s+(X) - 1 alors deg f(X) = deg P(X) - max (deg s-(X), deg s+(X)-1) *)
(* 2ieme cas : l = deg s-(X) = deg s+(X) - 1 
   s-(X) = u_l X^l + ...
   s+(X) = v_{l+1} X^(l+1) + ...

   On pose n_0 = - 2 * u_l / v_{l+1}
   Alors si n_0 !€ N  :  deg f <= deg p - l
         si n_0 € N :  def f <= max(deg p -l, n0)
*)

let majoration_degre_f (p, q, r) = 
  let s_plus = P.add (P.eval_k q C.one) r in
  let s_moins =  P.add (P.eval_k q C.one) (P.minus r) in
  if s_moins <> P.zero then 
    let d_s_moins = P.degre s_moins in
    let d_s_plus = P.degre s_plus in
    if d_s_moins <> (d_s_plus-1) then P.degre p - max d_s_moins (d_s_plus -1)
    else 
      let u = P.an s_moins in
      let v = P.an s_plus in
      let n0 = C.mult (-2,1) (C.mult u (C.inv v)) in
      if natural n0 then max (P.degre p - d_s_moins) (fst n0)
      else P.degre p - d_s_moins
  else (* si s- = 0 alors s+ <> 0 (sinon r = q = 0) *)
    (* donc deg s-(X) != deg s+(X) - 1 et deg f(X) = 1+deg P(X) - deg s+(X) *)
    1 + P.degre p - P.degre s_plus 
;;



(* 3ième étape calcul explicite de f(X) *)
(* Soit d la mojoration du degre de f obtenue *)
(* f(X) = w0 + w1X + ... + wd X^d *)
(* On résoud : p(X) = q(X+1)f(X) - f(X-1)r(X) *)

(* qui s'écrit : sum_{p = 0}^d_{w_p * Pp(X)} = p(X) *)
(* avec Pp(X) = X^p*Q(X+1) -r(X)*(X-1)^k *)

let calcul_Pk k (p,q,r) = 
  let x_p = [(1,1), k] in
  P.add (P.mult x_p (P.eval_k q C.one))
    (P.minus (P.mult r (P.eval_k x_p (-1,1))))
;;
module M = Matrix(DRing_Rat);;

(* W = [w0; w1;... wd] alors p(X) = q(X+1)f(X) - f(X-1)r(X) s'écrit : *)
(* M W = B avec M = make_matrix () et B = make_vector () *)
let make_matrix d (p,q,r) () = 
  let m = Array.make_matrix (d+1) (d+1) (0,1) in
  for k = 0 to d do
    let pk = calcul_Pk k (p,q,r) in
    List.iter (fun (e, q) -> m.(q).(k) <- e) pk;
  done;
  (m:M.t)
;;


let make_vector d p () = 
  let b = Array.make (d+1) [|(0,1)|] in
  List.iter (fun (e, q) -> b.(q) <- [|e|]) p;
  b
;;
let solve gR =
  let p, q, r = calcul_pqr gR in
  let d = majoration_degre_f (p, q, r) in
  if d < 0 then None
  else
    let m = make_matrix d (p,q,r) () in 
    let b = make_vector d p () in
    match M.find_a_solution m b with
    | None -> None
    | Some w ->
      let f = ref [] in
      for i = 0 to d do
        if w.(i).(0) <> C.zero then f:= (w.(i).(0), i)::!f;
      done;
      (* alpha(X) = q(X+1)/ p(X) * f(X) *)
      let f = P.normalise (!f) in
      Some (Q.normalise (P.mult (P.eval_k q C.one) f, p))
   
;;

let ($) f x = f x;;
let print = function
  | None -> print_endline "pas de solution."
  | Some alpha -> print_endline $ Q.print alpha
;;

let solve_q a_n k0 = 
  let a_n = Q.normalise a_n in
  let q = Q.mult a_n (Q.inv (Q.eval_k a_n (C.minus C.one))) in
  match solve q with
    | None -> print_endline "pas de solution"
    | Some q' -> 
      let s =  (Q.mult q' a_n) in
      let s0 = Q.eval s (k0-1, 1) in
      print_endline $ Q.print (Q.add s (Q.minus ([(s0, 0)], [((1,1), 0)])))
;;

(* Exemple : a_n = n^2 *)
(* a_n/a_(n-1) = n^2 / (n-1)^2 = R(n) avec R = X^2 / (X-1)^ 2*)
print $ solve (Q.normalise ([((1, 1), 2)], [((1, 1), 0); ((-2, 1), 1); ((1, 1), 2)]));;

(* On peut aussi utiliser solve_q si a_n est une fraction rationelle en n : solve_q a_n n0 renvoit directement sum k = n0 à n de a_n :  *)

solve_q ([((1, 1), 3)], [((1,1), 0)]) 0;;
solve_q ([((1, 1), 1)], [((1,1),0)]) 0 ;;
solve_q ([((1, 1), 0)], [((1,1),2); ((1,1), 1)]) 1;;
solve_q ([((1, 1), 0)], [((2,1),1); ((3,1), 2); ((1,1), 3)]) 1;;



(* Exemple : a_n = n *)
(* a_n/a_(n-1) = n/(n-1) = R(n) avec R = X/(X-1)*)
print $ solve (Q.normalise ([((1, 1), 1)], [((-1, 1), 0); ((1, 1), 1)]));;

(* Exemple : a_n = 1/n! *)
(* a_n/a_(n-1) = 1/n = R(n) avec R = 1/X*)
print $ solve (Q.normalise ([((1, 1), 0)], [((1, 1), 1)]));;

(* Exemple : a_n = 2^n *)
(* a_n/a_(n-1) = 2 = R(n) avec R = 2 *)
print $ solve (Q.normalise ([((2, 1), 0)], [((1, 1), 0)]));;


(* Exemple : a_n = n*2^n *)
(* a_n/a_(n-1) = 2*n/(n-1) = R(n) avec R = 2X/(X-1) *)
print $ solve (Q.normalise ([((2, 1), 1)], [((-1, 1), 0); ((1, 1), 1)]));;
(* Sn = 2^n * (2n-2) + C*)

(* Exemple : a_n = 1/n(n-1) *)
(* a_n / a_(n-1) = (n-1)(n-2) / n(n-1) = (n^2 - 3n + 2) / (n^2 - n) *)
print $ solve (Q.normalise ([((2, 1), 0); ((-3, 1), 1); ((1, 1), 2)], [((-1, 1), 1); ((1, 1), 2)]));;
(* S_n = (1-n)/n*(n-1) = -1/n + C*)
