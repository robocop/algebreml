module type DRing =
  sig
    type elem
    val zero : elem
    val one : elem
    val add : elem -> elem -> elem
    val minus : elem -> elem
    val mult : elem -> elem -> elem
    val inv : elem -> elem
    val print : elem -> string
  end
module Integers :
  sig
    val gcd : int -> int -> int
    val ppcm : int -> int -> int
    val ppcm_list : int list -> int
    val div : int -> int list
  end
module Rationnal :
  sig
    type rat = int * int
    val normalise : rat -> rat
    val mult : rat -> rat -> rat
    val add : rat -> rat -> rat
    val minus : rat -> rat
    val inv : rat -> rat
    val print : rat -> string
  end
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
module Matrix :
  functor (C : DRing) ->
    sig
      type matrix = C.elem array array
      val size : matrix -> int * int
      val get : matrix -> int * int -> C.elem
      val print : matrix -> unit
      val swap : matrix -> int * int -> unit
      val copy : matrix -> C.elem array array
      val map : (C.elem -> 'a) -> matrix -> 'a array array
      val det : matrix -> C.elem
      val forme_echelonnee : matrix -> matrix -> matrix * matrix
      val find_a_solution : matrix -> matrix -> matrix option
    end
module Poly :
  functor (C : DRing) ->
    sig
      type polynom = (C.elem * int) list
      val zero : (C.elem * int) list
      val one : (C.elem * int) list
      val x : (C.elem * int) list
      val normalise : polynom -> polynom
      val print : polynom -> string
      val add : polynom -> polynom -> polynom
      val minus : polynom -> polynom
      val mult_monome : C.elem * int -> polynom -> polynom
      val mult : polynom -> polynom -> polynom
      val pow : polynom -> int -> polynom
      val eval_k : polynom -> C.elem -> polynom
      val degre : polynom -> int
      val an : polynom -> C.elem
      val a0 : polynom -> C.elem
      val div_euclide : polynom -> polynom -> polynom * polynom
      val gcd : polynom -> polynom -> polynom
      val eval : polynom -> C.elem -> C.elem
      val resultant : polynom -> polynom -> C.elem
    end
module Frac :
  functor (C : DRing) ->
    sig
      type frac =  ((C.elem * int) list) *  ((C.elem * int) list)
      val normalise : frac -> frac
      val zero : (C.elem * int) list * (C.elem * int) list
      val one : (C.elem * int) list * (C.elem * int) list
      val print : frac -> string
      val add : frac -> frac -> frac
      val mult : frac -> frac -> frac
      val minus : frac -> frac
      val inv : frac -> frac
      val eval : frac -> C.elem -> C.elem
      val eval_k : frac -> C.elem -> frac
      module DRing_F :
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
module M :
  functor (C : DRing) ->
    sig
      type matrix
      type polynom
      val pc : matrix -> polynom
    end

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
  end
val rat_zeros : P.polynom -> DRing_Rat.elem list
