AlgebraMl : librairie pour faire de l'algèbre et de l'algèbre linéaire
======================================================================

Fonctionalités : 
- Polynômes, matrices, fractions rationelles sur un corps quelconque
- Polynome caractéristique, déterminant, résolution d'un système linéaire
- Résultant de deux polynomes sur un corps quelconque
- Etc.
- On peut générer le corps des fractions rationelles d'un corps quelconque, ce qui permet de faire des fractions rationelles à plusieurs variables
  (Q[X, Y] = (Q[X])[Y], voir le fichier exemples.ml)

Principe : on commence par définir le corps sur lequel on travaille, en implémentant le module DRing.
Ensuite, à l'aide de ce corps on peut générer un module polynome, matrices, fractions rationelles, fonctionnant sur ce corps et contenant des fonctions usuelles.

Pour compiler
-------------

``` 
    make
    make doc #pour générer la doc
```

Exemple
-------

Le fichier exemples.ml contient quelque exemple d'utilisation de la librairie.
Le fichier gosper.ml montre comment utiliser la librairie pour implémenter efficacement l'algorithme de Gosper pour le calcul formel de sommes.

```
   ocaml algebra.cmo exemples.ml

```