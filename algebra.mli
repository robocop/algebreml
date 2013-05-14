(** Module représentant un corps *)
module type DRing =
  sig
    type elem (** Type des élements du corps *)
    val zero : elem (** Element neutre de l'addition *)
    val one : elem (** Element neutre de la multiplication *)
    val add : elem -> elem -> elem
    val minus : elem -> elem
    val mult : elem -> elem -> elem
    val inv : elem -> elem
    val print : elem -> string (** affiche un element du corps *)
  end

(** Module implémentant les rationnels *)
module Rationnal :
  sig
    type rat = int * int
    val normalise : rat -> rat (** normalise met un rationnel sous forme normal (ce qui assure alors l'unicité de la représentation *)
    val mult : rat -> rat -> rat
    val add : rat -> rat -> rat
    val minus : rat -> rat
    val inv : rat -> rat
    val print : rat -> string
  end
(** Module implémentant le corps des rationnels *)
module DRing_Rat :
  sig
    type elem = Rationnal.rat
    val zero : elem
    val one : elem
    val add : elem -> elem -> elem
    val minus : elem -> elem
    val mult : elem -> elem -> elem
    val inv : elem -> elem
    val print : elem -> string
  end

(** Module polynome d'un corps C *)
module Poly :
  functor (C : DRing) ->
    sig
      type polynom = (C.elem * int) list (** Un polynome est une liste de monomes :  element * degre du monome*)
      val zero : polynom (** polynome 0 *)
      val one : polynom (** polynome 1 *)
      val x : polynom (** Polynome X  *)
      val normalise : polynom -> polynom (** Met un polynome sous forme normale, en particulier le polynome nul est représenté par [(zero, 0)] *)
      val print : polynom -> string
      val add : polynom -> polynom -> polynom
      val minus : polynom -> polynom
      val mult_monome : C.elem * int -> polynom -> polynom
      val mult : polynom -> polynom -> polynom
      val pow : polynom -> int -> polynom 
      val eval_k : polynom -> C.elem -> polynom (** Calcul P(X+k), k € C *)
      val degre : polynom -> int 
      val an : polynom -> C.elem (** Renvoit le coefficiant du terme de degré n, n degré du polynome *)
      val a0 : polynom -> C.elem (** Renvoit le coefficiant du terme constant du polynome *)
      val div_euclide : polynom -> polynom -> polynom * polynom
      val gcd : polynom -> polynom -> polynom
      val eval : polynom -> C.elem -> C.elem (** Evalue un polynome en un point de C  (horner)*)
      val resultant : polynom -> polynom -> C.elem (** Calcul le résultant de deux polynomes *)
    end
module Frac :
  functor (C : DRing) ->
    sig
      type frac =  ((C.elem * int) list) *  ((C.elem * int) list) (** Une fraction rationnelle est un couple de polynomes *)
      val normalise : frac -> frac
      val zero : frac (** Fraction rationnelle 0*)
      val one : frac (** Fraction rationnelle 1*)
      val print : frac -> string
      val add : frac -> frac -> frac
      val mult : frac -> frac -> frac
      val minus : frac -> frac
      val inv : frac -> frac
      val eval : frac -> C.elem -> C.elem  (** Evalue la fraction rationelle en un point de C*)
      val eval_k : frac -> C.elem -> frac (** Calcul F(X+k) *)
      module DRing_F : (** Construit le corps des fractions rationelles sur C *)
        sig
          type elem = frac
          val zero : elem
          val one : elem
          val add : elem -> elem -> elem
          val minus : elem -> elem
          val mult : elem -> elem -> elem
          val inv : elem -> elem
          val print : elem -> string
        end
    end

(** Matrices sur C *)
module Matrix : functor (C : DRing) ->
    sig
      type t = C.elem array array
      val size : t -> int * int
      val get : t -> int * int -> C.elem
      val print : t -> unit
      val swap : t -> int * int -> unit (** Echange les lignes i et i' *)
      val copy : t -> C.elem array array
      val map : (C.elem -> 'a) -> t -> 'a array array
      val det : t -> C.elem (** Calcul le determinant d'une matrice *)
      val forme_echelonnee : t -> t -> t * t (** forme_echelonnee A V met la matrice A sous forme échelonnée et applique les même modifications le vecteur n*1 V *)
      val find_a_solution : t -> t -> t option (** Trouve une solution *)
      (** Calcul une solution d'un système linéaire de la forme M X = V, M une matrice n*n et V un vecteur n*1 donné 
        L'ensemble des solutions est soit l'ensemble vide, soit un sous espace affine de codimension r, r rang de M 
        Renvoit donc soit None (cas de l'ensemble vide), soit Some X, X vecteur n*1 appartenant à l'ensemble des solutions
      *)
      type polynom =  (C.elem * int) list
      val pc : t -> polynom (** Calcul le polynome caractéristique d'une matrice carrée *)
    end


(** Polynome rationnels, contient en plus rat_zeros pour trouver les racinces rationelles d'un tel polynome *)
 module P : 
  sig
    exception Empty
    type polynom = (DRing_Rat.elem * int) list
    val zero : (DRing_Rat.elem * int) list
    val one : (DRing_Rat.elem * int) list
    val x : (DRing_Rat.elem * int) list
    val normalise : polynom -> polynom
    val print : polynom -> string
    val add : polynom -> polynom -> polynom
    val minus : polynom -> polynom
    val mult_monome : DRing_Rat.elem * int -> polynom -> polynom
    val mult : polynom -> polynom -> polynom
    val pow : polynom -> int -> polynom
    val eval_k : polynom -> DRing_Rat.elem -> polynom
    val degre : polynom -> int
    val an : polynom -> DRing_Rat.elem
    val a0 : polynom -> DRing_Rat.elem
    val div_euclide : polynom -> polynom -> polynom * polynom
    val gcd : polynom -> polynom -> polynom
    val eval : polynom -> DRing_Rat.elem -> DRing_Rat.elem
    val resultant : polynom -> polynom -> DRing_Rat.elem
    val rat_zeros : polynom -> DRing_Rat.elem list
  end
