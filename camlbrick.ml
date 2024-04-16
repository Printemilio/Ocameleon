(**
Ce module Camlbrick représente le noyau fonctionnel du jeu de casse-brique nommé <b>camlbrick</b>
(un jeu de mot entre le jeu casse-brique et le mot ocaml).

Le noyau fonctionnel consiste à réaliser l'ensemble des structures et autres fonctions capables
d'être utilisées par une interface graphique. Par conséquent, dans ce module il n'y a aucun
aspect visuel! Vous pouvez utiliser le mode console.

Le principe du jeu de casse-brique consiste à faire disparaître toutes les briques d'un niveau
en utilisant les rebonds d'une balle depuis une raquette contrôlée par l'utilisateur.

@author Hakim Ferrier-Belhaouari
@author Agnès Arnould

@version 1
*)

(** Compteur utilisé en interne pour afficher le numéro de la frame du jeu vidéo. 
    Vous pouvez utiliser cette variable en lecture, mais nous ne devez pas modifier
    sa valeur! *)

(**************************************VARIABLES et TYPES GLOBALES*********************************************************************************)
let frames = ref 0;;

(**
  type énuméré représentant les couleurs gérables par notre moteur de jeu. Vous ne pouvez pas modifier ce type!
  @deprecated Ne pas modifier ce type! 
*)
type t_camlbrick_color = WHITE | BLACK | GRAY | LIGHTGRAY | DARKGRAY | BLUE | RED | GREEN | YELLOW | CYAN | MAGENTA | ORANGE | LIME | PURPLE;;

(**
  Cette structure regroupe tous les attributs globaux,
  pour paramétrer notre jeu vidéo.
  <b>Attention:</b> Il doit y avoir des cohérences entre les différents paramètres:
  <ul>
  <li> la hauteur totale de la fenêtre est égale à la somme des hauteurs de la zone de briques du monde et
  de la hauteur de la zone libre.</li>
  <li>la hauteur de la zone des briques du monde est un multiple de la hauteur d'une seule brique. </li>
  <li>la largeur du monde est un multiple de la largeur d'une seule brique. </li>
  <li>initialement la largeur de la raquette doit correspondre à la taille moyenne.</li>
  <li>la hauteur initiale de la raquette doit être raisonnable et ne pas toucher un bord de la fenêtre.</li>
  <li>La variable <u>time_speed</u> doit être strictement positive. Et représente l'écoulement du temps.</li>
  </ul>
*)
type t_camlbrick_param = {
  world_width : int; (** largeur de la zone de dessin des briques *)
  world_bricks_height : int; (** hauteur de la zone de dessin des briques *)
  world_empty_height : int; (** hauteur de la zone vide pour que la bille puisse évoluer un petit peu *)

  brick_width : int; (** largeur d'une brique *)
  brick_height : int; (** hauteur d'une brique *)

  paddle_init_width : int; (** largeur initiale de la raquette *)
  paddle_init_height : int; (** hauteur initiale de la raquette *)

  time_speed : int ref; (** indique l'écoulement du temps en millisecondes (c'est une durée approximative) *)
};;

(** Enumeration des différents types de briques. 
  Vous ne devez pas modifier ce type.    
*)
type t_brick_kind = BK_empty | BK_simple | BK_double | BK_block | BK_bonus;;

(**
  Cette fonction renvoie le type de brique pour représenter les briques de vide.
  C'est à dire, l'information qui encode l'absence de brique à un emplacement sur la grille du monde.
  @return Renvoie le type correspondant à la notion de vide.
  @deprecated  Cette fonction est utilisé en interne.    
*)
let make_empty_brick() : t_brick_kind = 
  BK_empty
;;

(** Cette type definie un vecteur simple, qui sera utilisé pour construire le mouvement et la vitesse de la raquete et de la balle *)
type t_vec2 = {dx: int ; dy: int}
;;

(** 
    Enumeration des différentes tailles des billes. 
    La taille  normale d'une bille est [BS_MEDIUM]. 
  
    Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_ball_size = BS_SMALL | BS_MEDIUM | BS_BIG;;

(** 
Enumeration des différentes taille de la raquette. Par défaut, une raquette doit avoir la taille
[PS_SMALL]. 

  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_BIG;;

type t_caml_table = (t_brick_kind array array) ;;

(** 
  Enumération des différents états du jeu. Nous avons les trois états de base:
    <ul>
    <li>[GAMEOVER]: qui indique si une partie est finie typiquement lors du lancement du jeu</li>
    <li>[PLAYING]: qui indique qu'une partie est en cours d'exécution</li>
    <li>[PAUSING]: indique qu'une partie en cours d'exécution est actuellement en pause</li>
    </ul>
    
    Dans le cadre des extensions, vous pouvez modifier ce type pour adopter d'autres états du jeu selon
    votre besoin.
*)
type t_gamestate = GAMEOVER | PLAYING | PAUSING;;

(** Ce type definie tout les paramètres qui sont attribués à une balle *)
type t_ball = 
  {
    ball_size : int ref; (** taille de la balle *)
    ball_coordonates : t_vec2 ref; (** coordonnées de la balle sur le plan *)
    ball_velocity : t_vec2 ref; (** vitesse de deplacement de la balle *)
    ball_color : t_camlbrick_color ;  (** couleur de la balle *)
  } 
;;
(** Ce type definie tout les paramètres qui sont attribués à la raquette *)
type t_paddle =
  {
    paddle_height: int; (** hauteur de la raquette *)
    paddle_width: int ref; (** longueur de la raquette *)
    paddle_speed: int ref; (** vitesse de la raquette  *)
    paddle_color: t_camlbrick_color ; (** couleur de la raquette *)
    paddle_size: t_paddle_size ; (** catergorie de taille de la raquette *)
    paddle_position: t_vec2 ref ;  (** coordonnées de la raquette sur le plan *)
  }
;;


(** Ce type definie tout les elements qui sont contenue dans le jeu *)
type t_camlbrick = {
  param : t_camlbrick_param ; (** valeur de plusieurs paramètres basiques essentielles pour la construction du jeu *)
  brick_wall : t_caml_table ; (** tableau de briques *)
  paddle_track: t_paddle; (** raquette dans le jeu *)
  game_speed: int ref; (** vitesse general du jeu *)
  ball_list : t_ball list ref; (** liste de balles dans le jeu *)
  ball : t_ball; (** ball dans le jeu *)
  game_state: t_gamestate ref; (** état du jeu *)
}
;;

(**************************************END VARIABLES et TYPES GLOBALES*********************************************************************************)

(**************** VECTOR PART******************************************************************************************)
(* Itération 1 *) 


(**
  Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur.

  Vous devez modifier cette fonction.
  @param x première composante du vecteur
  @param y seconde composante du vecteur
  @return Renvoie le vecteur dont les composantes sont (x,y).
  @author Erwan Beau
*)
  (* Itération 1 *) 
let make_vec2(x,y : int * int) : t_vec2 = 
  {dx= x ; dy = y}
;;

(**
  Cette fonction renvoie un vecteur qui est la somme des deux vecteurs donnés en arguments.
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur égale à la somme des vecteurs.
  @author Erwan Beau
*)
  (* Itération 1 *)
let vec2_add(a,b : t_vec2 * t_vec2) : t_vec2 =  
  {dx= a.dx + b.dx ; dy= a.dy + b.dy }
;;

(**
  Cette fonction renvoie un vecteur égale à la somme d'un vecteur
  donné en argument et un autre vecteur construit à partir de (x,y).
  
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  @param a premier vecteur
  @param x première composante du deuxième vecteur
  @param y deuxième composante du deuxième vecteur
  @return Renvoie un vecteur égale à la somme des vecteurs.
  *)
  (* Itération 1 *)
let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  vec2_add(a, make_vec2(x,y))
;;
    
(**
  @param a premier vecteur
  @param x composante en x du second vecteur
  @param y composante en y du second vecteur
  @return Renvoie un vecteur qui est la résultante du vecteur a et du nouvelle vecteur formé par x et y
  @author Erwan Beau
   *)
   (* Itération 1 *)
let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 = 
  {dx= a.dx + x; dy= a.dy + y} 
;;


(**
  Cette fonction calcul un vecteur où 
  ses composantes sont la résultante de la multiplication  des composantes de deux vecteurs en entrée.
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes. 
  @author Erwan Beau
*)
  (* Itération 1 *)
let vec2_mult(a,b : t_vec2 * t_vec2) : t_vec2 = 
  {dx= a.dx*b.dx; dy= a.dy*b.dy}
;;

(**
  Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit à partir de (x,y).
  @param a premier vecteur
  @param x composante en x du second vecteur
  @param y composante en y du second vecteur
  @return Renvoie un vecteur qui est la résultante de multiplier le vecteur a et du nouvelle vecteur formé par x et y
  @author Erwan Beau
*)
  (* Itération 1 *)
let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 = 
  {dx= a.dx * x; dy= a.dy * y}
;;
(**************************END VECTOR PART******************************************************************************) 

(**************************PARAMETRES PART******************************************************************************)


(**
  Cette fonction construit le paramétrage du jeu, avec des informations personnalisable avec les contraintes du sujet.
  Il n'y a aucune vérification et vous devez vous assurer que les valeurs données en argument soient cohérentes.
  @return Renvoie un paramétrage de jeu par défaut      
*)
let make_camlbrick_param() : t_camlbrick_param = {
  world_width = 800;
  world_bricks_height = 600;
  world_empty_height = 200;

  brick_width = 40;
  brick_height = 20;

  paddle_init_width = 100;
  paddle_init_height = 20;

  time_speed = ref 20;
}
;;


(**
  Cette fonction extrait le paramétrage d'un jeu à partir du jeu donné en argument.
  @param game jeu en cours d'exécution.
  @return Renvoie le paramétrage actuel.
  @author Pavel Chauveau
  *)
    (* Itération 1 *)
let param_get(game : t_camlbrick) : t_camlbrick_param = 
  game.param
;;

(**
  Cette fonction permet de construire de matrice d'elements 'a
  @param n nombre de lignes
  @param m nombre de colonnes
  @param v element 'a 
  @return matrice de taille nm d'elements 'a
  @author Erwan Beau
  *)

let mat_make(n, m, v : int * int * 'a) : 'a array array = 
  if n < 0 || m < 0
  then failwith("erreur mat_make ; parametre invalide")
  else Array.make_matrix n m v 
;; 

(**
  Cette fonction attribue un numero à un type de brique
  @param p_num numero à attribuer
  @return brique associé aux numero mis en entré
  @author Pavel Chauveau
  *)
let int_to_type(p_num : int) : t_brick_kind =
  if p_num = 0 then BK_empty
  else if p_num = 1 then BK_simple
  else if p_num = 2 then BK_double
  else if p_num = 3 then BK_block
  else if p_num = 4 then BK_bonus
  else failwith ("erreur")
;;
  
(**
  Cette fonction construit la matrice de briques de façon aleatoire
  @param p_nb_line nombre de lignes
  @param p_nb_col nombre de colonnes
  @return matrice de briques
  @author Pavel Chauveau
  *)
let caml_table (p_nb_line , p_nb_col : int * int) : t_caml_table =
  let l_mat : t_brick_kind array array = mat_make (p_nb_line , p_nb_col , BK_empty)
  in
  (
    for i = 0 to (p_nb_line - 1) do
      for j = 0 to (p_nb_col -1) do
        l_mat.(i).(j) <- int_to_type(Random.int(5))
      done
    done ;
    l_mat
  )
;;

(**
  Cette fonction crée une raquette par défaut au milieu de l'écran et de taille normal.  
  @deprecated Cette fonction est là juste pour le debug ou pour débuter certains traitements de test.
  @author Sebastian Constain
*)
  (* Itération 2 *) 
let make_paddle() : t_paddle =
  let l_param : t_camlbrick_param = make_camlbrick_param() in
  {
    paddle_height = (l_param.paddle_init_height); (** hateur de la raquette *)
    paddle_width = ref ((l_param.paddle_init_width)); (** longueur de la raquette *)
    paddle_speed = ref 20 ; (** vitesse de la raquette *)
    paddle_color = RED ; (** couleur de la raquette *)
    paddle_size = PS_MEDIUM ; (** categorie de taille de la raquette *)
    paddle_position = ref (make_vec2(10,10)) ; (** coordonnées de la raquette dans le plan *)
  }
;;

(**
  Cette fonction attribue une valeur entier a une categorie de taille de la balle
  @param ball balle du jeu
  @param size categorie de taille attribué a la balle
  @author Pavel Chauveau
*)
let make_size (ball,size : t_ball*t_ball_size): unit =
  if size = BS_SMALL
    then ball.ball_size := 5
else
  if size = BS_MEDIUM
    then ball.ball_size := 10
else ball.ball_size := 15
;;

(**
  Cette fonction crée une ball par défaut dans le coordonées indiqués
  @author Pavel Chauveau
*)
  (* Itération 3 *) 
let make_ball(x,y, size : int * int * int) : t_ball = 
  {
    ball_size = ref size; 
    ball_coordonates = ref {dx = x ; dy = y}; 
    ball_velocity = ref {dx = 1 ; dy = -1};
    ball_color = YELLOW 
  } 
;;

(**
  Cette fonction crée une nouvelle structure qui initialise le monde avec aucune brique visible.
  Une raquette par défaut et une balle par défaut dans la zone libre.
  @return Renvoie un jeu correctement initialisé
  @author Pavel Chauveau, Emilio Decaix, Sebastian Constain
*)
  (* Itération 1, 2, 3 et 4 *)
let make_camlbrick() : t_camlbrick = 
  ( let res : t_camlbrick = 
  {
    brick_wall = caml_table(20,30) ;
    param = make_camlbrick_param() ;
    paddle_track = make_paddle();
    game_speed = ref 1;
    ball_list = ref [make_ball(400,600,10)];
    ball = make_ball(400,600,10);
    game_state = ref PLAYING
  } in
  (
    (*print_....;*)
    res
  )
  )
;;

(**
  Fonction utilitaire qui permet de traduire l'état du jeu sous la forme d'une chaîne de caractère.
  Cette fonction est appelée à chaque frame, et est affichée directement dans l'interface graphique.
  
  Vous devez modifier cette fonction.

  @param game représente le jeu en cours d'exécution.
  @return Renvoie la chaîne de caractère représentant l'état du jeu.
  @author Pavel Chauveau, Sebastian Constain
*)
let string_of_gamestate(game : t_camlbrick) : string =
  (* Itération 1,2,3 et 4 *) 
  if game.game_state = ref PLAYING
  then "PLAYING"
  else if game.game_state = ref GAMEOVER
    then "GAMEOVER"
    else "PAUSING" 
;;
(**********************************END PARAMETRES PART********************************************************************************************************)

(**********************************BRICKS PART****************************************************************************************************************)
(**
  Cette fonction cherche un brique par ses coordonées
  @param game jeu 
  @param i coordonnée des abscisse du brique
  @param j coordonnée des ordonnées du brique
  @return brique
  @author Erwan Beau
*)
  (* Itération 1 *)
let brick_get(game, i, j : t_camlbrick * int * int)  : t_brick_kind = 
  (game.brick_wall).(i).(j)
;;

(**
  Cette fonction affecte un brique lors qu'il y a une collision
  @param game jeu 
  @param i coordonné des abscisse du brique
  @param j coordon des ordonnées du brique
  @return type du brique cherché 
  @author Pavel Chauveau
*)
  (* Itération 1 *)
let brick_hit(game, i, j : t_camlbrick * int * int)  : t_brick_kind = 
  if (game.brick_wall).(i).(j) = BK_block
  then BK_block
  else
  if (game.brick_wall).(i).(j) = BK_simple
  then BK_empty
  else
  if (game.brick_wall).(i).(j) = BK_double
  then BK_simple
  else BK_empty
;;
 
(**
  Cette fonction attribue une couleur a un type de brique
  @param brick type de brique
  @return couleur attribué au brique
  @author Pavel Chauveau    
*)
let aux_brick_color(brick : t_brick_kind) : t_camlbrick_color =
  if brick = BK_block
  then WHITE
  else
  if brick = BK_simple
  then GREEN
  else
  if brick = BK_double
  then ORANGE
  else GRAY
;;

(**
  Cette fonction appelle la fonction aux_brick_colors pour le brique des coordonnées i et j
  @param game jeu 
  @param i coordonné des abscisse du brique
  @param j coordon des ordonnées du brique
  @return couleur attribué au brique
  @author Pavel Chauveau    
*)
  (* Itération 1 *)
let brick_color(game,i,j : t_camlbrick * int * int) : t_camlbrick_color = 
  aux_brick_color(game.brick_wall.(i).(j))
;;


(*****************************************************************END BRICKS PART*****************************************************************************)

(*****************************************************************PADDLE ET BALLES PART***********************************************************************)
(**
  Cette fonction renvoit la position x de la raquette
  @param game jeu 
  @return coordonnée x des abscisses de la raquette
  @author Erwan Beau
*) 
  (* Itération 2 *)
let paddle_x(game : t_camlbrick) : int=
  !(game.paddle_track.paddle_position).dx
;;

(**
  Cette fonction renvoit la longueur de la raquette
  @param game jeu 
  @return longueur de la raquette
  @author Erwan Beau
*) 
  (* Itération 2 *)
let paddle_size_pixel(game : t_camlbrick) : int = 
  !(game.paddle_track.paddle_width)
;;

(**
  Cette fonction deplace la raquette vers la gauche
  @param game jeu 
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain
*)       
(* Itération 2 *)
let paddle_move_left(game : t_camlbrick) : unit = 
  let move_left : t_vec2 = make_vec2(-1* !(game.game_speed),0) in
  if paddle_x(game) <= - 340 then
    ()
  else
    game.paddle_track.paddle_position := vec2_add(!(game.paddle_track.paddle_position),move_left);
;;
 
(**
  Cette fonction deplace la raquette vers la droite
  @param game jeu 
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain
*)       
      (* Itération 2 *)
let paddle_move_right(game : t_camlbrick) : unit =
  let move_right : t_vec2 = make_vec2(1* !(game.game_speed),0) in
  if paddle_x(game) >=  360 then
    ()
  else
    game.paddle_track.paddle_position := vec2_add(!(game.paddle_track.paddle_position),move_right);
;;
   
(**
  Cette fonction verifie s'il y a au moins une balle en jeu
  @param game jeu 
  @return booléen en fonction de s'il y au moins une balle en jeu
  @author Pavel Chauveau
*)         
      (* Itération 2 *)
let has_ball(game : t_camlbrick) : bool =
  if List.length !(game.ball_list) <= 0
  then false
  else true
;;

(**
  Cette fonction renvoit le nombre de balles dans en jeu
  @param game jeu 
  @return nombre de balles dans le jeu
  @author Pavel Chauveau
*)
  (* Itération 2 *)
let balls_count(game : t_camlbrick) : int =
  List.length !(game.ball_list)
;;

(**
  Cette fonction renvoit la liste de balles du jeu
  @param game jeu 
  @return liste de balles du jeu
  @author Pavel Chauveau
*)
  (* Itération 2 *)
let balls_get(game : t_camlbrick) : t_ball list = 
  !(game.ball_list)
;;

(**
  Cette fonction renvoit la ième balle de la liste de balles en jeu
  @param game jeu 
  @param i rang de la balle dans la liste de balles
  @return ième balle de la liste de balles en jeu
  @author Pavel Chauveau
*)
  (* Itération 2 *) 
let ball_get(game, i : t_camlbrick * int) : t_ball =
 List.nth (balls_get(game))i
;;

(**
  Cette fonction renvoit la position x de la ball en jeu
  @param game jeu 
  @return coordonnée x de la balle en jeu
  @author Pavel Chauveau
*) 
(*Itération 2*)
let ball_x(game,ball : t_camlbrick * t_ball) : int =
  !(ball.ball_coordonates).dx
;;

(**
  Cette fonction renvoit la position y de la ball en jeu
  @param game jeu 
  @return coordonnée y de la balle en jeu
  @author Pavel Chauveau
*)
let ball_y(game, ball : t_camlbrick * t_ball) : int =
  !(ball.ball_coordonates).dy
;;

(**
  Cette fonction renvoit la taille de la ball en jeu
  @param game jeu 
  @pama ball balle en jeu
  @return taille de la balle en jeu
  @author Pavel Chauveau
*)
let ball_size_pixel(game, ball : t_camlbrick * t_ball) : int =
  (* Itération 2 *)
  !(game.ball.ball_size)
;;

(**
  Cette fonction renvoit la couleur de la ball en jeu
  @param game jeu 
  @param ball balle en jeu
  @return couleur de la balle en jeu
  @author Pavel Chauveau
*)
let ball_color(game, ball : t_camlbrick * t_ball) : t_camlbrick_color =
  (* Itération 2 *)
  ball.ball_color
;;

(**
  Cette fonction modifie la vitesse de la ball en jeu en ajoutant des vecteurs
  @param game jeu 
  @param ball balle en jeu
  @param dv vecteur ajouté au vecteur vitesse de la balle en jeu
  @return unit (La fonction modifie une valeur du jeu)
  @author Emilio Decaix
*)
let ball_modif_speed(game, ball, dv : t_camlbrick * t_ball * t_vec2) : unit =
  (* Itération 3 *)
  (
    game.ball.ball_velocity := vec2_add_scalar(!(game.ball.ball_velocity), dv.dx , dv.dy )
  )
;;

(**
  Cette fonction modifie la vitesse de la ball en jeu en multipliant des vecteurs
  @param game jeu 
  @param ball balle en jeu
  @param ds vecteur multiplié au vecteur vitesse de la balle en jeu
  @return unit (La fonction modifie une valeur du jeu)
  @author Emilio Decaix
*)
let ball_modif_speed_sign(game, ball, sv : t_camlbrick * t_ball * t_vec2) : unit =
  (* Itération 3 *)
  (
    game.ball.ball_velocity := vec2_mult_scalar(!(game.ball.ball_velocity), sv.dx , sv.dy )
  )
;;

(**
  Cette fonction verifie si une entité est dans un cercle
@param cx abscisse x du cercle que l'on teste
@param cy ordonnée y du cercle que l'on teste
@param rad radiant du cercle que l'on teste
@param x coordonnée x de l'entité a tester
@param y coordonnée y de l'entité a tester
@return booléen en fonction de si l'entité est à l'interieur du cercle
@author Emilio Decaix  
*)
let is_inside_circle(cx,cy,rad, x, y : int * int * int * int * int) : bool =
  (* Itération 3 *)
  (((x-cx)*(x-cx))*((y-cy)*(y-cy))) <= (rad*rad)
;;

(**
  Cette fonction verifie si une entité est dans un quadrilatère
@param x1 première coordonnée limite x du quadrilatère
@param y1 première coordonnée limite y du quadrilatère
@param x2 deuxième coordonnée limite x du quadrilatère
@param y2 deuxième coordonnée limite y du quadrilatère
@param x coordonnée x de l'entité a tester
@param y coordonnée y de l'entité a tester
@return booléen en fonction de si l'entité est à l'interieur du quadrilatère
@author Emilio Decaix  
*)
let is_inside_quad(x1,y1,x2,y2, x,y : int * int * int * int * int * int) : bool =
  (* Itération 3 *)
  (x >= x1 && x <= x2) && (x >= y1 && x <= y2)
;;


(**
 Cette fonction enleve un balle de la liste de balles lorsque qu'elle depasse la zone de la raquette
 @param game jeu
 @param balls liste de balls
 @return liste de balles modifié
 @author Emilio Decaix
*)
let ball_remove_out_of_border(game,balls : t_camlbrick * t_ball list ) : t_ball list = 
  (* Itération 3 *)
  let fin_list : t_ball list ref = ref [] in 
  for i=0 to balls_count(game) -1 do
    let ball : t_ball = ball_get(game, i)in
    if !(ball.ball_coordonates).dy<=800 then
    fin_list := ball :: !(fin_list)
  done;
  !fin_list
;;

(**
 Cette fonction change la direction de la balle lorsqu'elle se retrouve avec un des limites du plan autre que la zone interdit (en dessous de la raquette)
 @param game jeu
 @param ball balle en jeu
 @return unit (La fonction modifie une valeur du jeu)
 @author Sebastian Constain
*)
let ball_hit_border(game, ball : t_camlbrick * t_ball) : unit =
  if (((!(ball.ball_coordonates).dx <= 0) && (!(ball.ball_velocity).dx < 0)) || ((!(ball.ball_coordonates).dx >= 800) && !(ball.ball_velocity).dx > 0))
    then ball.ball_velocity := vec2_mult_scalar(!(ball.ball_velocity), -1,1)
  else if (!(ball.ball_coordonates).dy <= 0) && (!(ball.ball_velocity).dy < 0) then
    ball.ball_velocity := vec2_mult_scalar(!(ball.ball_velocity), 1,-1)
;;

(* Itération 3 *)
(**
 Cette fonction change la direction et vitesse de la balle lorsqu'elle collisionne avec la raquette
 @param game jeu
 @param ball balle en jeu
 @param paddle raquette en jeu
 @return unit (La fonction modifie une valeur du jeu)
 @author Sebastian Constain
*)
let ball_hit_paddle(game,ball,paddle : t_camlbrick * t_ball * t_paddle) : unit =
(
  print_int(!(ball.ball_velocity).dx);print_int(!(ball.ball_velocity).dy);
  if (!(ball.ball_coordonates).dx-(400- !(ball.ball_size)) + !(ball.ball_size)) < (paddle_x(game) + paddle_size_pixel(game)/8) && 
      !(ball.ball_coordonates).dx-(400- !(ball.ball_size)) - !(ball.ball_size) > (paddle_x(game)- paddle_size_pixel(game)/8) &&
    (!(ball.ball_coordonates).dy >= 755)
  then 
    ball.ball_velocity := make_vec2(0,-1* !(game.game_speed)) 
  else
    if (!(ball.ball_coordonates).dx-(400- !(ball.ball_size)) + !(ball.ball_size)) < (paddle_x(game) + paddle_size_pixel(game)/4) && 
      !(ball.ball_coordonates).dx-(400- !(ball.ball_size)) - !(ball.ball_size) > (paddle_x(game)- paddle_size_pixel(game)/4) &&
    (!(ball.ball_coordonates).dy >= 755)
  then (
    ball.ball_velocity := make_vec2(1* !(game.game_speed)/2, -1* !(game.game_speed)/2)
  )
  else 
    if (!(ball.ball_coordonates).dx-(400- !(ball.ball_size)) + !(ball.ball_size)) < (paddle_x(game) + paddle_size_pixel(game)/2) && 
    !(ball.ball_coordonates).dx-(400- !(ball.ball_size)) - !(ball.ball_size) > (paddle_x(game)- paddle_size_pixel(game)/2) &&
  (!(ball.ball_coordonates).dy >= 755)
  then (
  ball.ball_velocity := make_vec2(1* !(game.game_speed)/3, -1* !(game.game_speed)/3)
  )
  else
  ();
  print_int(!(ball.ball_velocity).dx);print_int(!(ball.ball_velocity).dy);
)
;;


(* lire l'énoncé choix à faire
let ball_hit_corner_brick(game,ball, i,j : t_camlbrick * t_ball * int * int) : bool =
  if not(brick_get(game,i,j)=BK_empty) then
    if (is_inside_circle(ba))    
;; *)

(** Cette fonction verifie s'il y a une collision entre la balle est côté d'un brique de coordonnées i et j
@param game jeu
@param ball balle en jeu
@param i coordonnée d'abscisse du brique
@pama j coordonnée d'ordonnée du brique
@return booléen en fonction de s'il y a une collision entre la balle et le côté d'un brique
@author Sebastian Constain *)
let ball_hit_side_brick(game,ball, i,j : t_camlbrick * t_ball * int * int) : bool =
  (* Itération 3 *)
  if i<20 && j<30 then
    (if (brick_get(game,i,j)=BK_simple || brick_get(game,i,j)=BK_double || brick_get(game,i,j)=BK_block) && (is_inside_quad(i*40,j*20,(i+1)*40,(j+1)*20,!(ball.ball_coordonates).dx,!(ball.ball_coordonates).dy)) then
      not(brick_get(game,i,j)=BK_empty) && (is_inside_quad(i*40,j*20,(i+1)*40,(j+1)*20,!(ball.ball_coordonates).dx,!(ball.ball_coordonates).dy)) else true
    )
    else
      false
;;

(**
 Cette fonction execute tout les fonction associées aux collisions lorsque le jeu est en cours
 @param game jeu
 @param balls liste de balles du jeu
 @return unit (La fonction modifie une valeur du jeu)
 @author Sebastian Constain
*)
let game_test_hit_balls(game, balls : t_camlbrick * t_ball list) : unit =
  (* Itération 3 *)
  for i=0 to balls_count(game)-1 do
    let ball : t_ball = ball_get(game,i) in

      ball.ball_coordonates := vec2_add(!(ball.ball_coordonates),!(ball.ball_velocity));
      ball_hit_paddle(game,ball,game.paddle_track);

      game.ball_list := ball_remove_out_of_border(game,!(game.ball_list));
      ball_hit_border(game,ball);

    let dx : int = !(ball.ball_coordonates).dx/40 and dy : int = !(ball.ball_coordonates).dy/20 in
    
      if ball_hit_side_brick(game,ball,(dx),(dy)) then(

        game.brick_wall.(dx).(dy)<-brick_hit(game,dx,dy);

          if (!(ball.ball_coordonates).dx - !(ball.ball_velocity).dx < dx*40 || !(ball.ball_coordonates).dx + !(ball.ball_velocity).dx > dx*40+40) &&
            (!(ball.ball_coordonates).dy - !(ball.ball_velocity).dy > dy*20 && !(ball.ball_coordonates).dy + !(ball.ball_velocity).dy < dy*20+20) then
              (
                ball.ball_coordonates := (
                  if !(ball.ball_velocity).dx > 0 then
                    vec2_add_scalar(!(ball.ball_coordonates), -5, 0)
                  else
                    vec2_add_scalar(!(ball.ball_coordonates), 5,0)
                );

                ball.ball_velocity := vec2_mult_scalar(!(ball.ball_velocity), -1, 1);

              )

          else 
            (
              ball.ball_coordonates := (
                if !(ball.ball_velocity).dy > 0 then
                  vec2_add_scalar(!(ball.ball_coordonates), 0, -5)
                else
                  vec2_add_scalar(!(ball.ball_coordonates), 0, 5)
              );
              
              ball.ball_velocity := vec2_mult_scalar(!(ball.ball_velocity), 1, -1);
            )
          )
      else();
  done;
;;

(********************************************END PADDLE ET BALLE PART****************************************************************************************)

(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'elle se déplace. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_move(game,x,y : t_camlbrick * int * int) : unit = 
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est enfoncé. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param button numero du bouton de la souris enfoncé.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_click_press(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;


(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est relaché. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param button numero du bouton de la souris relaché.
  @param x l'abscisse de la position du relachement
  @param y l'ordonnée de la position du relachement   
*)
let canvas_mouse_click_release(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;



(**
  Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est appuyée.
  Les arguments sont le jeu en cours, la touche enfoncé sous la forme d'une chaine et sous forme d'un code
  spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param keyString nom de la touche appuyée.
  @param keyCode code entier de la touche appuyée.   
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain
*)
let canvas_keypressed(game, keyString, keyCode : t_camlbrick * string * int) : unit =
  if keyCode = 65363 then
    paddle_move_right(game)
  else
  if keyCode = 65361 then
    paddle_move_left(game)
  else 
  if keyCode = 97 then
    paddle_move_left(game)
  else
  if keyCode = 100 then
    paddle_move_right(game)
  else
    ()
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est relachée.
  Les arguments sont le jeu en cours, la touche relachée sous la forme d'une chaine et sous forme d'un code
  spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param keyString nom de la touche relachée.
  @param keyCode code entier de la touche relachée.  
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain 
*)
let canvas_keyreleased(game, keyString, keyCode : t_camlbrick * string * int) =
if keyCode = 65363 then
  paddle_move_right(game)
else
if keyCode = 65361 then
  paddle_move_left(game)
else 
if keyCode = 97 then
  paddle_move_left(game)
else
if keyCode = 100 then
  paddle_move_right(game)
else
  ()
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom1 de la zone du menu.
*)
let custom1_text() : string =
  (* Iteration 4 *)
  "<Rien1>"
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom2 de la zone du menu.
*)
let custom2_text() : string =
  (* Iteration 4 *)
  "<Rien2>"
;;


(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Start".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain
*)
let start_onclick(game : t_camlbrick) : unit=
  (game.game_state) := PLAYING
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Stop".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain
*)
let stop_onclick(game : t_camlbrick) : unit =
  (game.game_state) := GAMEOVER
;;

(**
  Cette fonction est appelée par l'interface graphique pour connaitre la valeur
  du slider Speed dans la zone du menu.

  Vous pouvez donc renvoyer une valeur selon votre désir afin d'offrir la possibilité
  d'interagir avec le joueur.
  @param game jeu
  @return renvoit la vitesse du jeu
  @author Sebastian Constain
*)
let speed_get(game : t_camlbrick) : int = 
  !(game.game_speed)
;;


(**
  Cette fonction est appelée par l'interface graphique pour indiquer que le 
  slide Speed dans la zone de menu a été modifiée. 
  
  Ainsi, vous pourrez réagir selon le joueur.
  @param game jeu
  @param xspeed vitesse du jeux
  @return unit (La fonction modifie une valeur du jeu)
  @author Sebastian Constain
*)
let speed_change(game,xspeed : t_camlbrick * int) : unit=
  for i = 0 to List.length(!(game.ball_list)) -1 do
    let ball : t_ball = ball_get(game,i) in
    if xspeed/ !(game.game_speed) >= 1 then
      ball.ball_velocity := vec2_mult_scalar(!(ball.ball_velocity), xspeed/ !(game.game_speed), xspeed/ !(game.game_speed))
  done;
  game.game_speed := xspeed;
;;

(**
    Cette fonction est appelée par l'interface graphique à chaque frame
    du jeu vidéo.
    Vous devez mettre tout le code qui permet de montrer l'évolution du jeu vidéo.
    @param game jeu
    @return unit (La fonction modifie une valeur du jeu)
    @author Sebastian Constain
  *)
let animate_action(game : t_camlbrick) : unit =  
  (game.ball.ball_coordonates) := vec2_add (!(game.ball.ball_velocity) , !(game.ball.ball_coordonates));
  if !(game.game_state)= PLAYING then 
    game_test_hit_balls(game,!(game.ball_list))
  else ();
  if game.brick_wall = [||] || game.ball_list = ref [] then
    game.game_state := GAMEOVER
  else()
;;
