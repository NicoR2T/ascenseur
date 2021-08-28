(* Projet 1: Ascenseur - Groupe: CUSSONNEAU ROYER TIREL *)

(*Types et valeurs par défaut*)
(***************************************************************************************************************************************************************************)
type couleur = Coeur | Carreau | Pique | Trefle | Aucun;;
type valeur = Indiennes of int | Valet | Dame | Roi | As;; 
type carte = {valeur: valeur; couleur: couleur};;
type phase = Monte | Descend;;
type atout_actif = Descente | Toujours;;

type options_type = {pts_gag: int; pts_plig: int; pts_per: int; pts_plip: int; atout: atout_actif};;
type partie_type = {phase: phase; nbre_joueurs: int; numero_manche: int; score: int list};;
type manche_type =  {cartes: int; cartes_en_main: int; atout_encours: couleur; contrats: int list; pari: int list; cartes_en_main_J1: carte list; cartes_en_main_J2: carte list; cartes_en_main_J3: carte list; cartes_en_main_J4: carte list; cartes_en_main_J5: carte list; cartes_en_jeu: carte list};;

let options = {pts_gag=0; pts_plig=2; pts_per=0; pts_plip=2; atout=Toujours};;
let partie = {phase= Monte; nbre_joueurs=0; numero_manche=1; score=[0;0;0;0;0]};;
let manche = {cartes=1; cartes_en_main=1; atout_encours=Aucun; contrats=[]; pari=[]; cartes_en_main_J1=[]; cartes_en_main_J2=[]; cartes_en_main_J3=[]; cartes_en_main_J4=[]; cartes_en_main_J5=[]; cartes_en_jeu=[]};;

(***************************************************************************************************************************************************************************)


(* Fonctions annexes *)
(***************************************************************************************************************************************************************************)
let creervaleur valeur= match valeur with
"valet" -> Valet
| "dame" -> Dame
| "roi" -> Roi
| "as" -> As
| _ -> Indiennes (int_of_string valeur)
;;

let creercouleur couleur = match couleur with
"coeur" -> Coeur
| "carreau" -> Carreau
| "trefle" -> Trefle
| "pique" -> Pique
| _ -> Aucun
;;

let cartes_joueur joueur manche = match joueur with
1 -> manche.cartes_en_main_J1
| 2 -> manche.cartes_en_main_J2
| 3 -> manche.cartes_en_main_J3
| 4 -> manche.cartes_en_main_J4
| _ -> manche.cartes_en_main_J5
;;

(* Cette fonction prend en entrée une chaine et un caractère précis et retourne une liste contenant à chaque élément la chaine contenu entre deux caractèreq consécutifs*)
let rec separer chaine separateur acc check res= match (chaine,acc) with
(chaine,acc) when acc=String.length chaine -> res@[String.sub chaine check (acc-check)]
| (chaine,acc) when chaine.[acc]=separateur -> separer chaine separateur (acc+1) (acc+1) (res@[String.sub chaine check (acc-check)])
| _ -> separer chaine separateur (acc+1) check res
;;

(* Cette fonction prend en entrée une liste contenant les cartes en chaine de caractère du fichier de sauvegarde et renvoie une liste de cartes*)
let rec cartes_main liste res = match liste with
t::[] -> res@[{valeur=creervaleur (List.hd (separer t ' ' 0 0 []));couleur=creercouleur (List.nth (separer t ' ' 0 0 []) 1 )}]
| t::r -> cartes_main r res@[{valeur=creervaleur (List.hd (separer t ' ' 0 0 []));couleur=creercouleur (List.nth (separer t ' ' 0 0 []) 1 )}]
;;

(* Les 3 prochaines fonctions permettent de tirer les cartes du paquet: d'abord on tire une carte aléatoirement sur les 52 cartes, puis on vérifie si cette carte n'a pas déjà été tiré pour éviter de sortir deux cartes strictement identique*)
let choix_couleur choix = match choix with
0 -> Pique
| 1 -> Coeur
| 2 -> Carreau
| _ -> Trefle
;;

let rec verifier_carte carte liste manche = match liste with
[] -> carte
| t::r -> if t=carte then (verifier_carte (tirer_carte manche) ((cartes_joueur 1 manche)@(cartes_joueur 2 manche)@(cartes_joueur 3 manche)@(cartes_joueur 4 manche)@(cartes_joueur 5 manche)) manche) else verifier_carte carte r manche

and tirer_carte manche = let liste_cartes = ((cartes_joueur 1 manche)@(cartes_joueur 2 manche)@(cartes_joueur 3 manche)@(cartes_joueur 4 manche)@(cartes_joueur 5 manche)) in let carte = ((Random.int 52)+1) in
	if carte mod 13=1 then (verifier_carte {valeur=As;couleur=(choix_couleur ((carte-1)/13))} liste_cartes manche)
	else if (carte mod 13<=10 && carte mod 13>0) then (verifier_carte {valeur=Indiennes (carte mod 13);couleur=(choix_couleur ((carte-1)/13))} liste_cartes manche)
	else if carte mod 13=11 then (verifier_carte {valeur=Valet;couleur=(choix_couleur ((carte-1)/13))} liste_cartes manche)
	else if carte mod 13=12 then (verifier_carte {valeur=Dame;couleur=(choix_couleur ((carte-1)/13))} liste_cartes manche)
	else (verifier_carte {valeur=Roi;couleur=(choix_couleur ((carte-1)/13))} liste_cartes manche)
;;

let afficher_valeur_carte carte = match carte with
Indiennes x -> string_of_int x
| Valet -> "V"
| Dame -> "D"
| Roi -> "R"
| _ -> "A"
;;

let afficher_couleur_carte carte = match carte with
Pique -> "♠"
| Trefle -> "♣"
| Coeur -> "♥"
| Carreau -> "♦"
| _ -> "aucun"
;;

let afficher_phase phase = match phase with
Monte -> "Montée  "
| _ -> "Descente"
;;

(*Cette fonction prend en entrée une liste de cartes et renvoie en chaine de caractères toutes les cartes de la liste*)
let rec defiler_cartes cartes res= match cartes with
[] -> res
| t::[] -> defiler_cartes [] (res^"⟦"^(afficher_valeur_carte t.valeur)^(afficher_couleur_carte t.couleur)^"⟧  ")
| t::r -> defiler_cartes r (res^"⟦"^(afficher_valeur_carte t.valeur)^(afficher_couleur_carte t.couleur)^"⟧  ")
;;

(* Les 3 prochaines fonctions permettent de définir le gagnant entre tous les joueurs, en prenant en compte l'atout et la valeur de la carte jouée*)
let estTete carte =  (carte.valeur=Roi || carte.valeur=Dame || carte.valeur=Valet || carte.valeur=As);;

let rec estGagnant carte1 carte2 manche = match (carte1,carte2) with
(carte1,carte2) when (carte1.couleur = carte2.couleur) && estTete carte1 && estTete carte2 -> carte1>carte2
| (carte1,carte2) when (carte1.couleur = carte2.couleur) && (estTete carte1 || estTete carte2) -> not(carte1>carte2)
| (carte1,carte2) when (carte1.couleur = carte2.couleur) -> carte1>carte2
| (carte1,carte2) when (carte1.couleur = manche.atout_encours) -> true
| (carte1,carte2) when (carte2.couleur = manche.atout_encours) -> false
| _ -> true
;;

let rec gagnant liste meilleur res acc manche = match liste with
[] -> res 
| t::r when (estGagnant meilleur t manche) -> gagnant r meilleur res (acc+1) manche
| t::r -> gagnant r t (acc+1) (acc+1) manche
;;

(* Cette fonction permet de mettre à jour le contrat du joueur gagnant, c'est à dire de lui enlever un pli à faire*)
let rec maj_contrats indice liste listetemp manche = match (liste,indice) with
(t::r, 1) -> {manche with contrats = listetemp@[(t-1)]@r}
| (t::r, indice) -> maj_contrats (indice-1) r (listetemp@[t]) manche
;;

(* Cette fonction permet d'ajouter les points de tous les joueurs selon le contrat indiqué en début de manche et le résultat final*)
let rec ajouter_points score contrats pari res partie options = match (score,contrats,pari) with
([],[],[]) -> {partie with score = res}
| (t1::r1,[],[]) -> {partie with score = res}
| (t1::r1, 0::r2, t3::r3) -> ajouter_points r1 r2 r3 (res@[t1+options.pts_gag+t3*(options.pts_plig)]) partie options
| (t1::r1, t2::r2, t3::r3) -> ajouter_points r1 r2 r3 (res@[t1-(options.pts_per+abs(t2)*(options.pts_plip))]) partie options
;;

(* Cette fonction permet d'annoncer les cartes de chaque joueur, elle fait appel à la fonction "defiler_cartes" *)
let rec annoncer_cartes joueur partie manche = match joueur with
joueur when joueur=partie.nbre_joueurs -> print_string ("Cartes du joueur "^string_of_int(joueur)^": "^(defiler_cartes (cartes_joueur joueur manche) "")^"\n\n")
| _ -> begin print_string ("Cartes du joueur "^string_of_int(joueur)^": "^(defiler_cartes (cartes_joueur joueur manche) "")^"\n"); annoncer_cartes (joueur+1) partie manche end
;;

(* Cette fonction permet de tester si le dernier joueur a rentré un contrat respectant les règles du jeu*)
let rec def_contrats contrat manche =
	if (((List.fold_left (fun x y -> x+y) 0 manche.contrats)+contrat)=(manche.cartes))
		then (print_string "\n⚠ La somme des contrats ne peut pas être égale aux nombres de cartes ⚠\n\nVeuillez redonner votre contrat: "; def_contrats (read_int()) manche)
	else {manche with contrats = manche.contrats@[contrat]}
;;

(*Cette fonction récupère les paris des joueurs et fait appel à la fonction précédente: def_contrats *)
let rec def_pari joueur derjoueur manche partie = match joueur with
joueur when joueur=derjoueur -> begin print_string ("Joueur "^string_of_int(joueur)^", combien de plis pensez vous faire? "); def_contrats (read_int()) manche end
| joueur when joueur=partie.nbre_joueurs -> begin print_string ("Joueur "^string_of_int(joueur)^", combien de plis pensez vous faire? "); let manche = {manche with contrats = manche.contrats@[(read_int())]} in def_pari 1 derjoueur manche partie end
| _ -> begin print_string ("Joueur "^string_of_int(joueur)^", combien de plis pensez vous faire? "); let manche = {manche with contrats = manche.contrats@[(read_int())]} in def_pari (joueur+1) derjoueur manche partie end
;;

let def_cartes_en_main joueur cartes manche = match joueur with
1 -> {manche with cartes_en_main_J1 = cartes}
| 2 -> {manche with cartes_en_main_J2 = cartes}
| 3 -> {manche with cartes_en_main_J3 = cartes}
| 4 -> {manche with cartes_en_main_J4 = cartes}
| _ -> {manche with cartes_en_main_J5 = cartes}
;;

(* Cette fonction permet d'enlever une carte de la main d'un joueur une fois que celle-ci est jouée*)
let rec enlever_carte joueur carte listetemp liste manche = match liste with
[] -> failwith "Erreur enlever_carte"
| t::r when t=carte -> def_cartes_en_main joueur (listetemp@r) manche
| t::r -> enlever_carte joueur carte (listetemp@[t]) r manche
;;

(* Cette fonction permet à chaque joueur de jouer sa carte à condition que le joueur indique bien une carte possible de son jeu, à savoir ne renvoie pas la nème carte quand il a n-1 cartes ou quand n<=0*)
let rec jouer_carte choix joueur manche =
	try
		let carte = (List.nth (cartes_joueur joueur manche) (choix-1)) in
			print_string (("⟦"^afficher_valeur_carte (carte).valeur)^(afficher_couleur_carte (carte).couleur)^"⟧");
			let manche = {manche with cartes_en_jeu = manche.cartes_en_jeu@[carte]} in
				enlever_carte joueur carte [] (cartes_joueur joueur manche) manche
	with
		Failure "nth" | Invalid_argument "List.nth" -> print_string "Cette carte n'est pas dans votre jeu: "; jouer_carte (read_int()) joueur manche
		| _ -> failwith "Erreur jouer_carte"
;;

(* Cette fonction prend en compte le choix de chaque joueur sur la carte qu'il souhaite jouer, elle fait appel à la fonction précédente: jouer_carte *)
let rec poser_cartes joueur derjoueur manche partie = match joueur with
joueur when joueur=derjoueur -> begin print_string ("\nJoueur "^string_of_int(joueur)^", quelle carte voulez vous jouer? "); jouer_carte (read_int()) joueur manche end
| joueur when joueur=partie.nbre_joueurs -> begin print_string ("\nJoueur "^string_of_int(joueur)^", quelle carte voulez vous jouer? "); let manche = jouer_carte (read_int()) joueur manche in poser_cartes 1 derjoueur manche partie end
| _ -> begin print_string ("\nJoueur "^string_of_int(joueur)^", quelle carte voulez vous jouer? "); let manche = jouer_carte (read_int()) joueur manche in poser_cartes (joueur+1) derjoueur manche partie end
;;

(* Cette fonctions permet d'éviter au joueur d'indiquer un nombre de points négatif dans les points atribués défini en option *)
let rec def_pts pts = if pts<0 then begin print_string "Veuillez rentrer un nombre de points positif\n\n"; def_pts (read_int()) end else pts;;

(* Cette fonction permet de définir quand l'atout est actif pour les options*)
let rec def_atout atout = if (atout ="Descente") then Descente else if (atout="Toujours") then Toujours else begin print_string "Veuillez rentrer Montee Descente ou Toujours"; def_atout (read_line()) end;;
(***************************************************************************************************************************************************************************)


(* Lancement d'une nouvelle partie *)
(* nouv_part -> lancer_partie *)
(***************************************************************************************************************************************************************************)
let valeur_carte carte = match carte with
Indiennes x -> string_of_int x
| Valet -> "valet"
| Dame -> "dame"
| Roi -> "roi"
| _ -> "as"
;;

let couleur_carte couleur = match couleur with
Coeur -> "coeur"
| Carreau -> "carreau"
| Trefle -> "trefle"
| _ -> "pique"
;;

(* Cette fonction permet d'écrire les cartes en main de chaque joueur dans le fichier de sauvegarde *)
let rec ecrire_cartes liste res = match liste with
t::[] -> (res^(valeur_carte t.valeur)^" "^(couleur_carte t.couleur))
| t::r -> ecrire_cartes r (res^(valeur_carte t.valeur)^" "^(couleur_carte t.couleur)^",")
;;

(* Cette fonction permet d'écrire les dernières lignes du fichier de sauvegarde contenant les cartes de chaque joueurs, les paris, le contrats et les points *)
let rec ecrire_contrats partie manche joueur liste1 liste2 liste3 res = match (joueur,liste1,liste2,liste3) with
(joueur,t1::r1,t2::r2,t3::r3) when joueur=partie.nbre_joueurs -> (res^(ecrire_cartes (cartes_joueur joueur manche) "")^"|"^string_of_int(t1)^"|"^string_of_int(t2)^"|"^string_of_int(t3))
| (_,t1::r1,t2::r2,t3::r3) -> ecrire_contrats partie manche (joueur+1) r1 r2 r3 (res^(ecrire_cartes (cartes_joueur joueur manche) "")^"|"^string_of_int(t1)^"|"^string_of_int(t2)^"|"^string_of_int(t3)^"\n")
;;

(*Cette fonction permet de sauvegarder la partie dans un fichier *)
let sauvegarder partie manche options = print_string ("Voulez-vous sauvegarder la partie? ");
	if (read_line())="oui" then begin print_string ("\nEntrez le nom du fichier de sauvegarde\n⚠Attention le fichier ne doit pas déjà exister sinon la sauvegarde sera écrasée⚠\n");
		let ecriture = open_out (read_line()) in
			output_string ecriture (string_of_int(options.pts_gag)^"|"^string_of_int(options.pts_plig)^"|"^string_of_int(options.pts_per)^"|"^string_of_int(options.pts_plip)^"\n"^(if partie.phase=Monte then "monte" else "descend")^"\n"^string_of_int(partie.nbre_joueurs)^"\n"^(match manche.atout_encours with Coeur -> "coeur" | Carreau -> "carreau" | Pique -> "pique" | Trefle -> "trefle" | _ -> "aucun")^"\n"^string_of_int(manche.cartes)^"\n"^string_of_int(manche.cartes_en_main)^"\n"^(ecrire_contrats partie manche 1 manche.contrats partie.score manche.pari ""));
			close_out ecriture
	end

;;

(* Cette fonction distribue les cartes à chaque joueur *)
let rec distribuer_cartes joueur nbre_cartes partie manche = match (joueur,nbre_cartes) with
		(1,1) -> def_cartes_en_main joueur ((cartes_joueur joueur manche)@[tirer_carte manche]) manche
		| (1,_) -> let manche = def_cartes_en_main joueur ((cartes_joueur joueur manche)@[tirer_carte manche]) manche in distribuer_cartes (partie.nbre_joueurs) (nbre_cartes-1) partie manche
		| _  -> let manche = def_cartes_en_main joueur ((cartes_joueur joueur manche)@[tirer_carte manche]) manche in distribuer_cartes (joueur-1) nbre_cartes partie manche
;;

(* Les 2 fonctions suivantes affichent le score de chaque joueur à la fin de la partie *)
let rec afficher_score joueur liste partie = match (joueur,liste) with
(joueur,t::r) when joueur = partie.nbre_joueurs -> print_string ("\t    ┃ Score joueur "^string_of_int(joueur)^": "^(if t<0 then "" else (String.make t '*'))^ "("^string_of_int(t)^")\n")
| (_,t::r) -> begin print_string ("\t    ┃ Score joueur "^string_of_int(joueur)^": "^(if t<0 then "" else (String.make t '*'))^ "("^string_of_int(t)^")\n"); afficher_score (joueur+1) r partie end
;;

let rec fin_partie partie manche options =
	Sys.command("clear");
	print_string "\t    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";
	afficher_score 1 partie.score partie;
	print_string "\t    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"; 
	print_string "Appuyez sur la touche \"Entrée\" pour continuer\n"; read_line();
	let options = {pts_gag=0; pts_plig=2; pts_per=0; pts_plip=2; atout=Toujours} in
		let partie = {phase= Monte; nbre_joueurs=0; numero_manche=1; score=[0;0;0;0;0]} in
			let manche = {cartes=1; cartes_en_main=1; atout_encours=Aucun; contrats=[]; pari=[]; cartes_en_main_J1=[]; cartes_en_main_J2=[]; cartes_en_main_J3=[]; cartes_en_main_J4=[]; cartes_en_main_J5=[]; cartes_en_jeu=[]} in 
				menu partie manche options


and lancer_partie premier_joueur partie manche options =
	print_string("\t   ╔══════════════════════════════════════════════════════╗\n");
	print_string("\t   ║                  Phase: "^(afficher_phase partie.phase)^"                     ║\n");
	print_string("\t   ╚══════════════════════════════════════════════════════╝\n\n");
	let manche = if (manche.cartes_en_main=manche.cartes) then distribuer_cartes partie.nbre_joueurs manche.cartes partie manche else manche in
		annoncer_cartes 1 partie manche;
		let manche = if (manche.cartes_en_main=manche.cartes) then (if ((partie.phase=Monte && options.atout=Descente)||(manche.cartes_en_main*partie.nbre_joueurs=52)) then {manche with atout_encours=Aucun} else {manche with atout_encours = (tirer_carte manche).couleur}) else manche in
			print_string("\t     ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
			print_string("\t     ┃\t        Atout pour cette manche: "^(afficher_couleur_carte manche.atout_encours)^"\t        ┃\n");
			print_string("\t     ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n");
			let der_joueur = if premier_joueur=1 then partie.nbre_joueurs else (premier_joueur - 1) in
				let manche = if (manche.cartes_en_main=manche.cartes) then let manche = def_pari premier_joueur der_joueur manche partie in {manche with pari = manche.contrats} else manche in
					sauvegarder partie manche options;
					let manche = poser_cartes premier_joueur der_joueur manche partie in
						let liste = manche.cartes_en_jeu in let joueur_gagnant_mod = ((gagnant (List.tl liste) (List.hd liste) 1 1 manche)+premier_joueur-1) mod partie.nbre_joueurs in let joueur_gagnant = if joueur_gagnant_mod = 0 then partie.nbre_joueurs else joueur_gagnant_mod in Sys.command("clear"); print_string (("\n\t     ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n\t     ┃     ✅  Le pli est remporté par le joueur ")^(string_of_int(joueur_gagnant))^" ✅     ┃\n\t     ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");
						let manche = {manche with cartes_en_jeu = []} in
							let manche = maj_contrats joueur_gagnant manche.contrats [] manche in
								let manche = {manche with cartes_en_main = (manche.cartes_en_main -1)} in
									print_string("--------------------------------------------------------------------------------\n");
									let partie = if (manche.cartes_en_main=0) then ajouter_points partie.score manche.contrats manche.pari [] partie options else partie in
										let manche = if (manche.cartes_en_main=0) then {manche with contrats = []; pari = []} else manche in 
											let partie = if (manche.cartes_en_main=0) then (if partie.phase = Monte && (52 - manche.cartes*partie.nbre_joueurs<partie.nbre_joueurs) then {partie with phase=Descend} else {partie with numero_manche = partie.numero_manche+1}) else partie in
												let manche = if (manche.cartes_en_main=0) then (if partie.phase = Monte then {manche with cartes = manche.cartes+1; cartes_en_main=manche.cartes+1} else {manche with cartes = manche.cartes-1; cartes_en_main=manche.cartes-1}) else manche in
													if (manche.cartes=manche.cartes_en_main) then (if manche.cartes=0 then fin_partie partie manche options else (lancer_partie (if ((partie.numero_manche mod partie.nbre_joueurs)=0) then partie.nbre_joueurs else (partie.numero_manche mod partie.nbre_joueurs)) partie manche options)) else lancer_partie joueur_gagnant partie manche options



and nouv_part partie manche options =
	print_string "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
	print_string "┃Combien de joueurs?                               ┃\n"; 
	print_string "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n";
	let choix = read_int() in match choix with
		1|2|3|4 -> begin let partie = {partie with nbre_joueurs = choix} in Sys.command("clear"); lancer_partie 1 partie manche options end
		| _ -> print_string "Le nombre de joueurs rentré est incorrect, veuillez choisir un nombre entre 2 et 5\n"; nouv_part partie manche options


(***************************************************************************************************************************************************************************)


(* Lecture du fichier de sauvegarde *)
(* cont_part -> lirefichier *)
(***************************************************************************************************************************************************************************)
and enregistrer_contrats acc lecture partie manche options = match acc with
acc when acc=(partie.nbre_joueurs+1) -> close_in lecture; lancer_partie 1 partie manche options
| _ -> let ligne_contrat_joueur = (separer (input_line lecture) '|' 0 0 []) in let manche = def_cartes_en_main acc ((cartes_joueur acc manche)@(cartes_main (separer (List.nth ligne_contrat_joueur 0) ',' 0 0 [] ) [])) manche in
			let manche = {manche with contrats = manche.contrats@[(int_of_string (List.nth ligne_contrat_joueur 1))]; pari = manche.pari@[(int_of_string (List.nth ligne_contrat_joueur 3))]} in
				let partie = {partie with score = partie.score@[(int_of_string (List.nth ligne_contrat_joueur 2))]} in
					enregistrer_contrats (acc+1) lecture partie manche options

and lirefichier fichier partie manche options = let lecture = open_in fichier in
	let ligne_options = (separer (input_line lecture) '|' 0 0 []) in
		let options={options with pts_gag =int_of_string(List.nth ligne_options 0); pts_plig = int_of_string(List.nth ligne_options 1); pts_per = int_of_string(List.nth ligne_options 2); pts_plip = int_of_string(List.nth ligne_options 3)} in
			let partie = {partie with phase= if (input_line lecture)="monte" then Monte else Descend } in
				let partie={partie with nbre_joueurs = int_of_string(input_line lecture); } in
					let manche = {manche with atout_encours = creercouleur (input_line lecture); cartes = int_of_string(input_line lecture)} in
						let partie = {partie with numero_manche = manche.cartes} in
							let manche = {manche with cartes_en_main = int_of_string(input_line lecture)} in
								let partie = {partie with score = []} in
									enregistrer_contrats 1 lecture partie manche options									

and cont_part partie manche options =
	print_string "Entrez le nom du fichier de sauvegarde:\n⚠ Attention, celui-ci doit être dans le même dossier que ce fichier ⚠\n";
	lirefichier (read_line()) partie manche options


(***************************************************************************************************************************************************************************)


(* Menu et options *)
(* set_options -> menu *)
(***************************************************************************************************************************************************************************)
and set_options partie manche options=
	Sys.command("clear");
	print_string "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
	print_string "┃Veuillez rentrer les modalités de victoire et de défaite ainsi que l'atout  ┃\n";
	print_string "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n";
	print_string "Nombre de points octroyés par défaut pour les gagnants: \n";
	let options={options with pts_gag=def_pts (read_int())} in
		print_string "Nombre de points octroyés par pli remporté: \n";
		let options={options with pts_plig=def_pts (read_int())} in
			print_string "Nombre de points perdus par défaut pour les perdants: \n";
			let options={options with pts_per=def_pts (read_int())} in
				print_string "Nombre de points perdus par pli en trop ou en moins: \n";
				let options={options with pts_plip=def_pts (read_int())} in
					print_string "Atout actif (Descente/Toujours): \n";
					let options={options with atout=def_atout (read_line())} in
						menu partie manche options

and menu partie manche options = 
	Sys.command("clear");
	print_string "\t\t\t\t   ♦♥♣♠ \t\t\t\t \n";
	print_string "\t    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
	print_string "\t    ┃ 1. Nouvelle partie                               ┃\n";
	print_string "\t    ┃ 2. Continuer la partie                           ┃\n";
	print_string "\t    ┃ 3. Options                                       ┃\n";
	print_string "\t    ┃ 4. Quitter                                       ┃\n";
	print_string "\t    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n";
	print_string "\t\t\t\t   ♦♥♣♠ \t\t\t\t \n";
	let choix = read_int() in match choix with
		1 -> nouv_part partie manche options
		| 2 -> cont_part partie manche options
		| 3 ->  set_options partie manche options
		| 4 -> Sys.command("clear"); print_string "A bientot!\n"
		| _ -> print_string "Veuillez rentrer un nombre entre 1 et 4\n\n"; menu partie manche options
;;

let rec home partie manche options =
	Sys.command("clear");
	print_string "\t\t\t\t   ♦♥♣♠ \t\t\t\t \n";
	print_string "\t\t ◈  Bienvenue sur le jeu de l'Ascenseur ◈ \t\t \n";
	print_string "\t    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
	print_string "\t    ┃                    ▶ Jouer ◀                     ┃\n";
	print_string "\t    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n";
	print_string "\t\t\t\t   ♦♥♣♠ \t\t\t\t \n";
	print_string "\t\t\t   Jeu développé par:\n \t\t\t ▸ Gauthier CUSSONNEAU \n \t\t\t ▸ Axel ROYER \n \t\t\t ▸ Nicolas TIREL \n";
	print_string "\t\t\t\t   ♦♥♣♠ \t\t\t\t \n";
	let choix2 = read_line() in match choix2 with
	backspace -> menu partie manche options
	| _ -> home partie manche options
;;

(***************************************************************************************************************************************************************************)


(* Lancement du programme*)
(* *)
(***************************************************************************************************************************************************************************)
Random.self_init();; (* Initialisation de la seed de la fonction random pour éviter d'obtenir toujours les même valeurs *)
home partie manche options;;

(***************************************************************************************************************************************************************************)