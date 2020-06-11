# Vérificateur de preuves

Ce projet a été abandonné à cause de l'annulation du TIPE mais fonctionne
quand même. Il est seulement assez difficile à lire car non documenté
(il tient plus du prototype que du projet fini).

La source est divisée en trois grandes parties :

- `parser` : Un petit parseur de s-expressions qui permet de lire des
  preuves et des déclarations de règles de déduction (qui sont
  respectivement utilisées dans les modules `runner` et `core`).
- `core` : L'ensemble des outils qui permettent de produire des séquents.
  L'ensemble des règles de dérivation est écrit dans `test_theory.txt`.
- `runner` : Un ensemble de classes et méthodes utilitaires pour simplifier
  l'écriture des preuves, dont un interpréteur qui permet d'exécuter une preuve.

Ce programme peut être décliné en deux versions :

- Une version "standalone", utile pour le développement. Le point d'entrée
  est `main.rs`. Utiliser `cargo run` pour lancer. Le programme écoutera
  les changements du fichier `test_proof.txt` et re-vérifiera la preuve
  contenue à chaque changement.
- Une version "bibliothèque", pour l'intégrer dans un site web (a servi
  pour construire la démonstration du programme sur
  https://thejohncrafter.github.io/verifier). Utiliser `wasm-pack build`
  pour compiler (en présence de `wasm-pack` bien sûr).

Je précise encore que ce projet n'est pas documenté car abandonné à cause
de l'annulation des TIPE, donc difficile à lire. J'espère tout de même
qu'une lecture superficielle est suffisante pour comprendre le principe
du fonctionnement de ce programme.
