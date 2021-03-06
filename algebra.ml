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

module Integers = 
  struct
    let rec gcd a b = if a mod b = 0 then b else gcd b (a mod b);;
    let ppcm a b = a * b / (gcd a b)
    let ppcm_list = List.fold_left ppcm 1
    let pos_div n = 
      let rec div n k = 
	if k*k > n then []
	else 
	  (if n mod k = 0 then 
	      if n/k <> k then [k;n/k]
	      else [k]
	   else [])@div n (k+1)
      in
      div (abs n) 1
      
    let div n = 
      let rec div n k = 
	if k*k > n then []
	else 
	  (if n mod k = 0 then 
	      if n/k <> k then [k;-k;n/k;-n/k]
	      else [k;-k]
	   else [])@div n (k+1)
      in
      div (abs n) 1
  end




module Rationnal =
    struct
      type rat = int * int
	  
      let normalise ((x,y):rat) =  
	if y = 0 then raise Division_by_zero
	else
	  let s = if x*y >= 0 then 1 else -1 in
	  let x' = abs x in let y' = abs y in
	  let d = Integers.gcd x' y' in 
	  ( (s * (x'/d), y'/d) : rat)
     
      let mult ((x,y):rat) ((x',y'):rat) = 
	let a = Integers.gcd x y' in
	let b = Integers.gcd x' y in
	let nx, ny = x/a, y/b in
	let nx', ny' = x'/b, y'/a in
	normalise (nx*nx', ny*ny');;
      let add ((x,y):rat) ((x',y'):rat) = normalise (y'*x+y*x', y*y');;
      let minus ((x,y):rat) = normalise (-x, y);;
      let inv ((x,y):rat) = if x <> 0 then normalise (y,x) else raise Division_by_zero;;
      let print ((x,y):rat) = match (x,y) with
	| (x, 1) -> Printf.sprintf " %d" x;
	| (x,y) -> Printf.sprintf " %d/%d " x y
      ;;
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



module Matrix0 = functor (C : DRing) ->
  struct
    type t = C.elem array array
    (* M € M_n,p p = rows, m = colums *)
    (* size M = (p,n) *)
    let size (m:t) = Array.length m, Array.length m.(0)
    (* i € [|1,p|] & j € [|1,n|] *)
    let get (m:t) (i,j) = (m.(i-1)).(j-1)
    let print (m:t) = 
      let p,n = size m in
      print_string "{";
      for i = 1 to p-1 do
	print_string "{";
	for j = 1 to n-1 do
	  print_string ((C.print (get m (i,j))) ^ ", ")
	done;
	print_string (C.print (get m (i,n)));
	print_string " }, ";
	print_newline();
      done;
      print_string "{";
      for j = 1 to n-1 do
	print_string ((C.print (get m (p,j))) ^ ", ")
      done;
      print_string (C.print (get m (p,n)));
      print_string " }}\n"
    

    (* Echange les lignes i et i' *)
    let swap (m:t) (i,i') =
      let tmp = m.(i-1) in
      m.(i-1) <- m.(i'-1);
      m.(i'-1) <- tmp
    let copy (m:t) = Array.map Array.copy m
    let map f (m:t)  = Array.map (fun l -> Array.map f l) m
    let det (m:t) = 
      let rec det' res (m:t) k n = 
        (* Si on est sur un bloc 1*1 c'est terminé *)
	if k = n then C.mult res (get m (n,n))
	else
          begin
	  (* on trouve le pivot *)
	    let t = ref false in
	    let k' = ref k in
	    while not (!t) && !k' <= n do
	      if (get m (!k',k)) <> C.zero then t:=true
	      else incr k';
	    done;
	  (*si tous les éléments de la colonne sont nuls, le determinant est nul *)
	    if not (!t) then C.zero 
	    else 
              begin
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
	        let r' = C.mult (if k' - k mod 2 = 1 then C.minus p else p) res in
		(det' r' m (k+1) n)
              end
          end
            
      in
      let p,n = size m in
      if p <> n then failwith "echec : matrice non carree"
      else 
	let nm = copy m in
	det' (C.one) nm 1 n
	  (* m matrice n*n, v vecteur (v matrice n*1 ) *)
    let forme_echelonnee (m:t) (v:t) = 
      let n, p = size m in
      let n', p' = size v in
      if not (n = p && p' = 1) then failwith "matrice non carrée ou vecteur de la mauvaise taille"
      else
	let rec calc (m:t) (v:t) k n = 
          (* Si on est sur un bloc 1*1 c'est terminé *)
	  if k = n then (m, v)
	  else 
            begin
	      (* on trouve le pivot *)
	      let t = ref false in
	      let k' = ref k in
	      while not (!t) && !k' <= n do
		if (get m (!k',k)) <> C.zero then t:=true
		else incr k';
	      done;
	    (*si tous les éléments de la colonne sont nuls, on passe directement au sous bloc *)
	      if not (!t) then calc m v (k+1) n
	      else 
		begin
	          let k' = !k' in
	          let p = get m (k',k) in
	          swap m (k,k'); swap v (k, k');
	          for i = k+1 to n do
	          (* Li <- Li - alpha Lk, alpha = M_i,k/p dans M et V *)
	            let alpha = C.mult (get m (i,k)) (C.inv p) in
		    (* On modifie M *)
	            for j = k to n do
		      m.(i-1).(j-1) <- C.add (get m (i,j)) (C.minus (C.mult alpha (get m (k,j))));
	            done;
		    (* On modifie V *)
		    v.(i-1).(0) <- C.add (get v (i,1)) (C.minus (C.mult alpha (get v (k,1))));
	          done;
	          calc m v (k+1) n
		end
            end      
	in
	let m', v' = copy m, copy v in 
	calc m' v' 1 n 

    (* Calcul une solution d'un système linéaire de la forme M X = V, 
       M une matrice n*n et V un vecteur n*1 donné *)
    (* L'ensemble des solutions est soit l'ensemble vide, 
       soit un sous espace affine de codimension r, r rang de M *)
    (* Renvoit donc soit None (cas de l'ensemble vide), 
       soit Some X, X vecteur n*1 appartenant à l'ensemble des solution *)
    let find_a_solution (m:t) (v:t) = 
      let m', v' = forme_echelonnee m v in
      let n, _ = size m' in
      let rec find (x:t) k = 
	if k = 0 then Some x
	else
	  (* On trouve si elle existe la première composante non nulle de la ligne k *)
	  let r = ref (-1) in
	  for i = n downto k do
	    if get m' (k, i) <> C.zero then r := i;
	  done;
	  if !r = -1 then (* si la ligne k est nulle *)
             if get v' (k, 1) =  C.zero then (x.(k-1).(0) <- C.zero; find x (k-1))
	     else None
	  else 
	   ( 
	    (* x_r = (v_r - sum_{i = r+1}^n_{M_k,i * x_i})/M_k,r *)
	    let s = ref C.zero in
	    for i = (!r)+1 to n do
	      s := C.add (!s) (C.mult (get m' (k, i)) (get x (i, 1)));
	    done;
            x.(!r-1).(0) <- C.mult (C.add (get v' (k, 1)) (C.minus !s)) (C.inv (get m' (k,!r)));
	    find x (k-1)
	   )
      in
      let x = copy v in
      find x n
          
  end

module Poly = functor ( C : DRing) -> 
   struct 
     exception Empty;;
     type polynom = (C.elem * int) list
     let zero = [(C.zero), 0]
     let one = [(C.one), 0]
     let x = [(C.one), 1]
     let normalise (poly:polynom) = 
       let p' = List.sort (fun (x, a) (y, b) -> compare a b) poly in
       let rec simpl (p:polynom)= match p with 
	   [] -> []
	 | (a, _)::r when a = C.zero -> simpl r
	 | (a, n)::(b, m)::r when n == m -> (C.add a b, m)::simpl r
	 | x::xs -> x:: simpl xs
       in
       ((match simpl p' with
	 | [] -> zero
	 | p -> p):polynom)
     ;;
     let print (p:polynom) = 
       let print_monome (x, d) = match d with
	 | 0 -> C.print x
	 | 1 -> Printf.sprintf "%sx" (C.print x)
	 | d -> Printf.sprintf "%sx^%d" (C.print x) d 
       in
       let rec print = function
	 | [] -> ""
	 | [x] -> print_monome x
	 | x::xs -> (print_monome x) ^  " + " ^ print xs
       in
       print p     
     ;;  
     let add (p1:polynom) (p2:polynom) = 
       let rec sum_p p1 p2 = match (p1, p2) with
	   ([], p) -> p
	 | (p, []) -> p
	 | ((a, d1)::p1, (b, d2)::p2) when d1 == d2 -> (C.add a b, d2)::(sum_p p1 p2)
	 | ((a, d1)::p1, (_, d2)::_) when d1 < d2 -> (a, d1)::(sum_p p1 p2)
	 | (_, (b, d2)::p2) -> (b, d2) :: (sum_p p1 p2)
       in
       normalise (sum_p p1 p2)
     ;;
     let minus (p:polynom) = (List.map (fun (a, d) -> (C.minus a, d)) p : polynom);;
     let rec mult_monome (a, d) (p:polynom) = match p with
	 [] -> []
       | (a', d')::p' -> normalise ((C.mult a a', d+d')::mult_monome (a, d) p')
     ;;
     let rec mult (p1:polynom) (p2:polynom) = match p1 with
	 [] -> []
       | (a, d)::p1' -> add (mult_monome (a, d) p2) (mult p1' p2)
     ;;
     let rec pow p k = 
       if k = 0 then one
       else 
         if k mod 2 = 0 then 
           let r = pow p (k/2) in
           mult r r
         else 
           mult p (pow p (k-1))
     
     (* Evalue le polynome en k : eval_k P k = P(X+k) *)
     let rec eval_k (p:polynom) k = match p with
       | [] -> []
       | (e, d)::l ->
         let p = mult_monome (e, 0) (pow [(k, 0); (C.one, 1)] d) in
         add p (eval_k l k)
     

     let degre (p:polynom) = List.fold_left (fun d (_, d') -> max d d') 0 p
     let an (p:polynom) = fst (List.hd (List.rev p))
     let a0 (p:polynom) = fst (List.hd p)
     let rec div_euclide p q = 
       if q = zero then failwith "q = 0"
       else if p = zero then (zero, zero)
       else
	 begin
	   let a, b = degre p, degre q in
	   if a < b then ([(C.zero, 0)], p)
	   else 
	     let pn, qn = an p, an q in
	     let m = (C.mult pn (C.inv qn), a-b) in
	     let (q', r') = div_euclide (add p (minus (mult_monome m q))) q in
	     (add [m] q', r')
	 end
     ;;
     let rec gcd a b = 
       let _, r = div_euclide a b in
       if r = zero then b else gcd b r
     ;;
     (* Evalue un polynome en un point précis, à utilisé avec un polynome normalisé ! *)
     let eval (p:polynom) (x:C.elem) =  
       let rec horner r d = function
	 | [] when d <= 0 -> r
	 | (e,d') :: l  when d = d' -> horner (C.add (C.mult x r) e) (d-1) l
	 | l -> horner (C.mult x r) (d-1) l
       in
       match List.rev p with
	 | [] -> raise Empty
	 | (a,d)::p' ->  horner a (d-1) p'

         (* Résultant de deux polynome normalisé non constants *)
     module M = Matrix0(C)
     let resultant p1 p2 = 
       let d1 = degre p1 in
       let d2 = degre p2 in
       let s = d1 +d2 in
       let m = Array.make_matrix s s C.zero in
       let rec copy_ligne l i0 i = function
         | [] -> ()
         | (e,d)::r when i = d -> 
           m.(l-1).(i0+i-1) <- e;
           copy_ligne l i0 (i+1) r
         | p -> copy_ligne l i0 (i+1) p
       in
       for l = 1 to d2 do
         copy_ligne l l 0 p1;
       done;
       for l = d2+1 to s do
         copy_ligne l (l-d2) 0 p2;
       done;
       M.det m

 end 



module Frac = functor (C : DRing) ->
  struct
    module P = Poly (C);;
    (* Contraintes pour forme normale : 
       -Le dénominateur est unitaire
       -Le numérateur et le dénominateur sont premiers entre eux
    *)
    type frac = P.polynom * P.polynom
    let normalise ((p,q):frac) = 
      let p, q = P.normalise p, P.normalise q in
      let a = P.gcd p q in
      let p', q' = 
      if P.degre a > 0 then 
        fst (P.div_euclide p a), fst (P.div_euclide q a)
      else p, q 
      in
      let e = P.an q' in
      let q'' = List.map (fun (x, d) -> (C.mult (C.inv e) x, d)) q' in
      let p'' = List.map (fun (x, d) -> (C.mult (C.inv e) x, d)) p' in
      ((p'', q''):frac)
	
    let zero = (P.zero, P.one)
    let one = (P.one, P.one)

    let print ((p,q):frac) = 
      let a, b = P.print p, P.print q in
      if q =P.one then a
      (* 
      let n = max (String.length a) (String.length b) in
      let f = String.make n '-' in
      a ^ "\n" ^ f ^ "\n" ^ b 
      *)
      else "(" ^  a ^ ")/(" ^ b ^ ")"
    
    let add ((p,q):frac) ((p',q'):frac) = 
      normalise (P.add (P.mult q' p) (P.mult q p'), P.mult q q')
    let mult ((p,q):frac) ((p',q'):frac) = 
      normalise (P.mult p p', P.mult q q')
    let minus ((p,q):frac) = ((P.minus p, q):frac)
    let inv ((p,q):frac) = 
      	if q = P.zero then raise Division_by_zero
	else normalise (q,p)
    let eval ((p,q):frac) (x:C.elem) = 
      C.mult (P.eval p x) (C.inv (P.eval q x))
    let eval_k ((p, q):frac) (x:C.elem) =
      normalise (P.eval_k p x, P.eval_k q x)

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



module Matrix = functor (C : DRing) ->
  struct
    module F0 = Frac(C)
    module F = F0.DRing_F
    module M1 = Matrix0(F)  
    include Matrix0(C)
    
    type polynom = F0.P.polynom

    let e (x:C.elem) = (([(x,0)], [C.one, 0]) : F.elem )

    let pc (m:t) = 
      let p, n = size m in
      let m' = Array.make_matrix p n (F.zero) in
      for i = 1 to p do
	for j = 1 to n do
	  let x = ([(C.one, 1)], [(C.one, 0)]) in
	  if i = j then 
	    m'.(i-1).(j-1) <- F.add x (F.minus (e (get m (i,j))))
	  else 
	    m'.(i-1).(j-1) <- F.minus (e (get m (i,j)))
	done;
      done;
        (fst (M1.det m'):polynom)
      
  end


  
module P = struct
  include Poly (DRing_Rat)
  let rat_zeros (p:polynom) = 
    let rec couples l1 l2 = match l1 with
      | [] -> []
      | x::xs -> 
	let rec add = function
	  | [] -> []
	  | y::r when y > 0  && ((abs x <= 1) || Integers.gcd x y = 1) -> (x,y)::add r
	  | _::r -> add r
	in
	add l2 @ couples xs l2
    in
    let q, r = div_euclide p x in
    let p = if r = zero then q else p in
    let g = Integers.ppcm_list (List.map (fun ((p,q),_) -> q) p) in
    let f x = fst (DRing_Rat.mult x (g,1)) in
    let a0, an = f (a0 p), f (an p) in
    let zeros_possibles = couples (Integers.div a0) (Integers.div an) in
    let l = List.filter (fun z -> eval p z = DRing_Rat.zero) zeros_possibles in
    if r = zero then DRing_Rat.zero::l else l
  let int_zeros (p:polynom) = 
    let q, r = div_euclide p x in
    let p = if r = zero then q else p in
    let g = Integers.ppcm_list (List.map (fun ((p,q),_) -> q) p) in
    let f x = fst (DRing_Rat.mult x (g,1)) in
    let a0 = f (a0 p) in
    let zeros_possibles = Integers.pos_div a0 in
    let l = List.filter (fun z -> eval p (z,1) = DRing_Rat.zero) zeros_possibles in
    let l' = if r = zero then 0::l else l in
    List.map (fun x -> (x,1)) l'
end
;;
