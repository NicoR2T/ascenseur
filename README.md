# Jeu de l'ascenseur
Projet Ocaml  
Deuxième année de Prépa  
Le jeu se joue de 2 à 5 joueurs et avec un paquet de 52 cartes. Il est composé d’une phase ascendante et d’une phase
descendante. Au début du jeu, le paquet de cartes est divisé par le nombre de joueurs afin de déterminer s’il y aura
ou non un atout et quel sera le nombre maximal de carte que les joueurs pourront avoir en main. Ensuite chaque
joueur annonce combien de plis il espère remporter durant la manche, que l’on appelle le contrat (un pli remporté
s’apparente à une victoire du joueur durant le tour, la somme totale des plis annoncés par les joueurs ne devant
être égale au nombre de cartes distribuées). Débute ensuite la partie, tour à tour chaque joueur doit jouer une carte
de la même couleur que la carte jouée par le premier joueur et si possible supérieure à celles posées par les autres
joueurs avant afin de remporter le pli. Il faut bien évidemment prendre en compte l’existence de l’atout, un joueur
qui possède une carte de la couleur de l’atout (défini au début de chaque manche) peut la jouer et ainsi surpasser
toutes les autres cartes (exceptées les cartes atout de valeur supérieure) et donc remporter le pli. Chaque tour se
termine donc quand chaque joueur a joué une carte et chaque pli remporté octroie un point, et chaque gagne ou
perd des points en fonction du fait qu’ils aient rempli ou non leur contrat.
