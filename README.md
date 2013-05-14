AlgebraMl : librairie pour faire de l'algèbre et de l'algèbre linéaire
======================================================================

Fonctionalités : 
- Polynomes, matrices, fractions rationelles sur un corps quelconque
- On peut générer le corps des fractions rationelles d'un corps quelconque, ce qui permet de faire des fractions rationelles à plusieurs variables
- Polynome caractéristique, déterminant, résolution d'un système linéaire
- Résultant de deux polynomes sur un corps quelconque, etc. 

Pour compiler
-------------


   ocamlc -c algebra.mli
   ocamlc -c  algebra.ml

Exemple
-------

   ocaml algebra.cmo gosper.ml
   ocaml algebra.cmo exemples.ml

