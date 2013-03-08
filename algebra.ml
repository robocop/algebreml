module type DRing = sig
    type elem
    val zero: elem
    val one : elem
    val add : elem -> elem -> elem
    val minus : elem -> elem
    val mult : elem -> elem -> elem
    val inv : elem -> elem
    val print : elem -> string
end


module Rationnal =
    struct
      type rat = int * int
      let rec gcd a b = if a mod b = 0 then b else gcd b (a mod b);;
      let normalise ((x,y):rat) =  
	if y = 0 then raise Division_by_zero
	else
	  let s = if x*y >= 0 then 1 else -1 in
	  let x' = abs x in let y' = abs y in
	  let d = gcd x' y' in 
	  ( (s * (x'/d), y'/d) : rat)
      ;;
      let mult ((x,y):rat) ((x',y'):rat) = normalise (x*x', y*y');;
      let add ((x,y):rat) ((x',y'):rat) = normalise (y'*x+y*x', y*y');;
      let minus ((x,y):rat) = normalise (-x, y);;
      let inv ((x,y):rat) = if x <> 0 then normalise (y,x) else raise Division_by_zero;;
      let print ((x,y):rat) = Printf.sprintf " %d/%d " x y;;
    end

module DRing_Rat:DRing with type elem= Rationnal.rat =
  struct
    type elem = Rationnal.rat;;
    let zero = ((0,1):(Rationnal.rat));;
    let one = ((1,1):(Rationnal.rat));;
    let add = Rationnal.add;;
    let minus = Rationnal.minus;;
    let mult = Rationnal.mult;;
    let inv = Rationnal.inv;;
    let print = Rationnal.print
  end

	

module Poly = 
  functor ( C : DRing) -> 
   struct 
     exception Empty;;
     type polynome = (C.elem * int) list
     let zero = [(C.zero), 0]
     let one = [(C.one), 0]
     let normalise (poly:polynome) = 
       let p' = List.sort (fun (x, a) (y, b) -> compare a b) poly in
       let rec simpl (p:polynome)= match p with 
	   [] -> []
	 | (a, _)::r when a = C.zero -> simpl r
	 | (a, n)::(b, m)::r when n == m -> (C.add a b, m)::simpl r
	 | x::xs -> x:: simpl xs
       in
       ((match simpl p' with
	 | [] -> zero
	 | p -> p):polynome)
     ;;
     let print (p:polynome) = 
       let print_monome (x, d) = Printf.sprintf "%sX^%d" (C.print x) d in
       let rec print = function
	 | [] -> ""
	 | [x] -> print_monome x
	 | x::xs -> (print_monome x) ^  " + " ^ print xs
       in
       print p     
     ;;  
     let add (p1:polynome) (p2:polynome) = 
       let rec sum_p p1 p2 = match (p1, p2) with
	   ([], p) -> p
	 | (p, []) -> p
	 | ((a, d1)::p1, (b, d2)::p2) when d1 == d2 -> (C.add a b, d2)::(sum_p p1 p2)
	 | ((a, d1)::p1, (_, d2)::_) when d1 < d2 -> (a, d1)::(sum_p p1 p2)
	 | (_, (b, d2)::p2) -> (b, d2) :: (sum_p p1 p2)
       in
       normalise (sum_p p1 p2)
     ;;
     let minus (p:polynome) = (List.map (fun (a, d) -> (C.minus a, d)) p : polynome);;
     let rec mult_monome (a, d) (p:polynome) = match p with
	 [] -> []
       | (a', d')::p' -> normalise ((C.mult a a', d+d')::mult_monome (a, d) p')
     ;;
     let rec mult (p1:polynome) (p2:polynome) = match p1 with
	 [] -> []
       | (a, d)::p1' -> add (mult_monome (a, d) p2) (mult p1' p2)
     ;;
     let degre (p:polynome) = List.fold_left (fun d (_, d') -> max d d') 0 p
     let dominant (p:polynome) = fst (List.hd (List.rev p))
     let rec div_euclide p q = 
       if q = zero then failwith "q = 0"
       else if p = zero then (zero, zero)
       else
	 begin
	   let a, b = degre p, degre q in
	   if a < b then ([(C.zero, 0)], p)
	   else 
	     let pn, qn = dominant p, dominant q in
	     let m = (C.mult pn (C.inv qn), a-b) in
	     let (q', r') = div_euclide (add p (minus (mult_monome m q))) q in
	     (add [m] q', r')
	 end
     ;;
     let rec gcd a b = 
       let _, r = div_euclide a b in
       if r = zero then b else gcd b r
     ;;

 end 


module Frac = functor (C : DRing) ->
  struct
    module P = Poly (C);;
    (* Contraintes pour forme normale : 
       -Le dénominateur est unitaire
       -Le numérateur et le dénominateur sont premiers entre eux
    *)
    type frac = P.polynome * P.polynome
    let normalise ((p,q):frac) = 
      let a = P.gcd p q in
      let p', _ = P.div_euclide p a in
      let q', _ = P.div_euclide q a in
      let e = P.dominant q' in
      let q'' = List.map (fun (x, d) -> (C.mult (C.inv e) x, d)) q' in
      let p'' = List.map (fun (x, d) -> (C.mult (C.inv e) x, d)) p' in
      ((p'', q''):frac)
	
    let zero = (P.zero, P.one)
    let one = (P.one, P.one)

    let print ((p,q):frac) = 
      let a, b = P.print p, P.print q in
      let n = max (String.length a) (String.length b) in
      let f = String.make n '-' in
      a ^ "\n" ^ f ^ "\n" ^ b 
    
    let add ((p,q):frac) ((p',q'):frac) = 
      normalise (P.add (P.mult q' p) (P.mult q p'), P.mult q q')
    let mult ((p,q):frac) ((p',q'):frac) = 
      normalise (P.mult p p', P.mult q q')
    let minus ((p,q):frac) = ((P.minus p, q):frac)
    let inv ((p,q):frac) = 
      	if q = P.zero then raise Division_by_zero
	else normalise (q,p)

    module DRing_F:DRing with type elem= frac =
    struct
      type elem = frac;;
      let zero = zero;;
      let one = one;;
      let add = add;;
      let minus = minus;;
      let mult = mult;;
      let inv = inv;;
      let print = print
    end

  end
;;
module Matrix = functor (C : DRing) ->
  struct
    type matrix = C.elem array array
    (* M € M_n,p p = rows, m = colums *)
    (* size M = (p,n) *)
    let size (m:matrix) = Array.length m, Array.length m.(0)
    (* i € [|1,p|] & j € [|1,n|] *)
    let get (m:matrix) (i,j) = (m.(i-1)).(j-1)
    let print (m:matrix) = 
      let p,n = size m in
      for i = 1 to p do
	for j = 1 to n do
	 print_string ((C.print (get m (i,j))) ^ " | ")
	done;
	print_newline();
      done
    let swap (m:matrix) (i,i') =
      let tmp = m.(i-1) in
      m.(i-1) <- m.(i'-1);
      m.(i'-1) <- tmp
    let copy (m:matrix) = Array.map Array.copy m
    let map f (m:matrix)  = Array.map (fun l -> Array.map f l) m
    let det (m:matrix) = 
      let rec det' (m:matrix) k n = 
	if k = n then get m (n,n)
	else 
	(* on trouve le pivot *)
	  let t = ref false in
	  let k' = ref k in
	  while not (!t) && !k' <= n do
	    if get m (!k',k) <> C.zero then t:=true;
	    incr k';
	  done;
	(*si tous les éléments de la colone sont nuls, le determinant est nul *)
	  if not (!t) then C.zero 
	  else 
	    let k' = !k' in
	    let p = get m (k',k) in
	    swap m (k,k');
	    for i = k+1 to n do
	    (* Li <- Li - alpha Lk, alpha = M_i,k/p *)
	      let alpha = C.mult (get m (i,k)) (C.inv p) in
	      for j = k to n do
		m.(i-1).(j-1) <- C.add (get m (i,j)) (C.minus (C.mult alpha (get m (k,j))));
	      done;
	    done;
	    C.mult (if k' - k mod 2 = 0 then p else C.minus p) (det' m (k+1) n)
      in
      let p,n = size m in
      if p <> n then failwith "echec : matrice non carree"
      else 
	let nm = copy m in
	det' nm 1 n

  end



module M = functor (C : DRing) ->
  struct
    module F0 = Frac(C)
    module F = F0.DRing_F
    module M1 = Matrix(F)  
    module M0 = Matrix(C)
    type matrix = M0.matrix
    type polynom = F0.P.polynome

    let e (x:C.elem) = (([(x,0)], [C.one, 0]) : F.elem )

    let pc (m:M0.matrix) = 
      let p, n = M0.size m in
      let m' = Array.make_matrix p n (F.zero) in
      for i = 1 to p do
	for j = 1 to n do
	  let x = ([(C.one, 1)], [(C.one, 0)]) in
	  if i = j then 
	    m'.(i-1).(j-1) <- F.add x (F.minus (e (M0.get m (i,j))))
	  else 
	    m'.(i-1).(j-1) <- F.minus (e (M0.get m (i,j)))
	done;
      done;
        (fst (M1.det m'):polynom)
      
  end


    
module M = M(DRing_Rat);;
let m = [|[|(0,1); (4,1)|];
	  [|(4,3); (3,1)|];
	|];;
M.F0.P.print (M.pc m);;


module F = Frac(DRing_Rat);;
module F' = F.DRing_F(
module M' = Matrix(F);;

Array.make_matrix 2 3 1;;

let m = [|[|(0,1); (4,1); (-7,2)|];
	  [|(4,3); (3,1); (1,1)|];
	  [|(2,1); (0,1); (4,3)|]
	|];;
M.det m;; 
M.print m;;

module F = Frac(DRing_Rat);;


module Qx = Q.DRing_F;;
module Qxy = Frac(R);;

let y =  ([((1, 1), 1)], [((1, 1), 0)]);;
print_string (Qxy.print (Qxy.normalise ([(y, 1)], [(Qx.one, 0); (Qx.one,2)])));;

let f = Q.normalise ([((1,1), 1); ((1,1), 2)], [((-3,1), 1); ((4,2), 3)]);;
let g = Q.normalise ([((1,1), 1); ((1,1), 8)], [((-3,1), 1)]);;

Q.print g;;
print_string (Q.print (Q.add f g));;

module P = Poly (DRing_Rat);;

P.print (P.gcd [((1,1), 0); ((1,1), 1)] [((1,1), 0); ((2,1), 1); ((1,1), 2)]);;








P.normalise [];;
P.degre (P.normalise [((0,1), 3)]);;
let p1 = P.normalise [((1,1), 0); ((2,1), 1); ((2,1), 3)];;
let p2 = P.normalise [((1,1), 0); ((1,1), 1)];;

P.print (P.gcd p1 p2);;

P.print p1;;
P.print p2;;

P.degre p1 ;;

let q, r = P.div_euclide p1 p2;;
P.print r;;
P.print (P.prod_p p1 p2);;
