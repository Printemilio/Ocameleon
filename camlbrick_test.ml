#use "CPtest.ml";;
#use "camlbrick.ml";;
(*****************Test Vecteur******************************************************************************************************)

(**Cette fonction vérifie la fonction make_vec2 
@author Emilio Decaix    
*) 
let test_make_vec2 () : unit =
  let l_res : t_vec2 t_test_result = test_exec (make_vec2,"make_vec2(3,0)", (3,0)) in
  assert_equals_result_m ("result_make_vec2", {dx=3;dy=0}, l_res)
;;

(**Cette fonction vérifie la fonction vec2_add
@author Emilio Decaix    
*) 
let test_vec2_add () : unit =
  let l_res : t_vec2 t_test_result = test_exec (vec2_add,"vec2_add(3,0)(3,0)", ({dx = 3; dy = 0},{dx = 3; dy = 0})) in
  assert_equals_result_m ("result_vec2_add", ({dx = 6; dy = 0}), l_res)
;;

(**Cette fonction vérifie la fonction vec2_mult
@author Emilio Decaix    
*) 
let test_vec2_mult () : unit =
  let l_res : t_vec2 t_test_result = test_exec (vec2_mult,"vec2_mult(3.0)(3.0)", ({dx = 3; dy = 0},{dx = 3; dy = 0})) in
  assert_equals_result_m ("result_vec2_add", ({dx = 9; dy = 0}), l_res)
;;
  
(**Cette fonction initialise le jeu pour les autre tests.
@author Emilio Decaix    
*) 
let init_game() : t_camlbrick = 
  let prm :  t_camlbrick_param = make_camlbrick_param() 
  in
  let game_test : t_camlbrick = 
          {           
            brick_wall = caml_table(prm.world_width , prm.world_bricks_height) ; 
            param = prm ;
            paddle_track  = { 
                              paddle_height = 5;
                              paddle_width = ref 5;
                              paddle_speed = ref 5;
                              paddle_color = RED ; 
                              paddle_size = PS_MEDIUM ;
                              paddle_position = ref {dx= 10 ; dy= 10};
                            };
            game_speed = ref 5 ;
            ball_list = ref [];
            ball = {
                     ball_size = ref 5; 
                     ball_coordonates = ref {dx= 10 ; dy= 10}; 
                     ball_velocity = ref {dx= 10 ; dy= 10};
                     ball_color = YELLOW;
                   };
            game_state = ref PAUSING;
          } 
  in
  (
    game_test.brick_wall.(1).(1) <- BK_block ;
    game_test.brick_wall.(0).(1) <- BK_simple ;
    game_test.brick_wall.(1).(0) <- BK_double ;
    game_test.brick_wall.(0).(0) <- BK_empty ;
  );
  game_test
;;

(*****************Test briques***************************************************************************************)

(**Cette fonction vérifie la fonction brick_get1
@author Emilio Decaix    
*) 
let test_brick_get1 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(0,1)", (init_game(),0,1)) in
  assert_equals_result_m ("result_brick_get 1", BK_simple, l_res)
;;

(**Cette fonction vérifie la fonction brick_get2
@author Emilio Decaix    
*) 
let test_brick_get2 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(0,0)", (init_game(),0,0)) in
  assert_equals_result_m ("result_brick_get 2", BK_empty, l_res)
;;

(**Cette fonction vérifie la fonction brick_get3
@author Emilio Decaix    
*) 
let test_brick_get3 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(1,1)", (init_game(),1,1)) in
  assert_equals_result_m ("result_brick_get 3", BK_block, l_res)
;;

(**Cette fonction vérifie la fonction brick_get4
@author Emilio Decaix    
*) 
let test_brick_get4 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(1,0)", (init_game(),1,0)) in
  assert_equals_result_m ("result_brick_get 4", BK_double, l_res)
;;

(**Cette fonction vérifie la fonction brick_hit1
@author Emilio Decaix    
*) 
let test_brick_hit1 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(1,0)", (init_game(),1,0)) in
  assert_equals_result_m ("result_brick_hit1",  BK_simple, l_res)
;;

(**Cette fonction vérifie la fonction brick_hit2
@author Emilio Decaix    
*) 
let test_brick_hit2 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(1,1)", (init_game(),1,1)) in
  assert_equals_result_m ("result_brick_hit2", BK_block, l_res)
;;

(**Cette fonction vérifie la fonction brick_hit3
@author Emilio Decaix    
*) 
let test_brick_hit3 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(0,1)", (init_game(),0,1)) in
  assert_equals_result_m ("result_brick_hit3", BK_empty, l_res)
;;

(**Cette fonction vérifie la fonction brick_hit4
@author Emilio Decaix    
*) 
let test_brick_hit4 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(0,0)", (init_game(),0,0)) in
  assert_equals_result_m ("result_brick_hit4", BK_empty, l_res)
;;

(**Cette fonction vérifie la fonction brick_color1
@author Emilio Decaix    
*) 
let test_brick_color1 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(1,1)", (init_game(),1,1)) in
  assert_equals_result_m ("result_brick_color1", BLACK, l_res)
;;

(**Cette fonction vérifie la fonction brick_color2
@author Emilio Decaix    
*) 
let test_brick_color2 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(0,1)", (init_game(),0,1)) in
  assert_equals_result_m ("result_brick_color2", GREEN, l_res)
;;

(**Cette fonction vérifie la fonction brick_color3
@author Emilio Decaix    
*) 
let test_brick_color3 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(1,0)", (init_game(),1,0)) in
  assert_equals_result_m ("result_brick_color3", ORANGE, l_res)
;;

(**Cette fonction vérifie la fonction brick_color4
@author Emilio Decaix    
*) 
let test_brick_color4 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(0,0)", (init_game(),0,0)) in
  assert_equals_result_m ("result_brick_color4", GRAY, l_res)
;;

(**Cette fonction vérifie la fonction aux_brick_color1
@author Emilio Decaix    
*) 
let test_aux_brick_color1 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_block)", (BK_block)) in
  assert_equals_result_m ("result_aux_brick_color1", BLACK, l_res)
;;

(**Cette fonction vérifie la fonction aux_brick_color2
@author Emilio Decaix    
*) 
let test_aux_brick_color2 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_simple)", (BK_simple)) in
  assert_equals_result_m ("result_aux_brick_color2", GREEN, l_res)
;;

(**Cette fonction vérifie la fonction aux_brick_color3
@author Emilio Decaix    
*) 
let test_aux_brick_color3 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_double)", (BK_double)) in
  assert_equals_result_m ("result_aux_brick_color3", ORANGE, l_res)
;;

(**Cette fonction vérifie la fonction aux_brick_color4
@author Emilio Decaix    
*) 
let test_aux_brick_color4 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_empty)", (BK_empty)) in
  assert_equals_result_m ("result_aux_brick_color4", GRAY, l_res)
;;

(**Ici on défini les type pour faire un test structurel 
@author Emilio Decaix    
*) 
type t_camlbrick_color = RED | BLUE 
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_LARGE 
type t_caml_table_paddle = { x: int ref; y: int } 

let mat_make (n, m, init) = { x = ref (snd init); y = fst init } 


(**Cette fonction vérifie la fonction make paddle
@author Emilio Decaix    
*) 
let test_make_paddle () =
  let result = make_paddle () in
  assert (result.paddle_color = RED);
  assert (result.paddle_size = PS_MEDIUM);
  assert (result.paddle_height = 20);
  assert (result.paddle_width = ref 100);
  assert (result.paddle_speed = ref 5);
  assert (result.paddle_position = ref {dx = 10 ; dy = 10} );
  print_endline "All tests passed for make_paddle."
;;

let x = 100;;
let y = 100;;
let size = 5;;

(**Cette fonction vérifie la fonction make_ball
@author Emilio Decaix    
*) 
let test_make_ball () =
  let result = make_ball (x,y, size : int * int * int) in
 
  assert (result.ball_size = ref size );
  assert (result.ball_coordonates = ref {dx = x ; dy = y});
  assert (result.ball_velocity = ref {dx= 10 ; dy= 10});
  assert (result.ball_color = YELLOW);
  print_endline "All tests passed for make_ball."
;;

(**Ici on défini ball pour d'autre fonction
@author Emilio Decaix    
*) 
let ball = {
  ball_size = ref 5 ; 
  ball_coordonates = ref {dx = 100 ; dy = 100}; 
  ball_velocity = ref {dx = 10 ; dy = 10};
  ball_color = YELLOW ; 
  }

(**Cette fonction vérifie la fonction make_size
@author Emilio Decaix    
*) 
let test_make_size () : unit =
  let l_res : unit t_test_result = test_exec (make_size,"make_size(5)", (ball ,BS_SMALL)) in
  assert_equals_m ("result_make_size1", 5, ! (ball.ball_size))
;;

(**Cette fonction vérifie la fonction make_camlbrick
@author Emilio Decaix    
*) 
let test_make_camlbrick() : unit =
  let l_res_final : bool ref = ref true in
  let l_mat : t_camlbrick = make_camlbrick() in
  let l_type_found : t_brick_kind list ref = ref [] in
  let l_types : t_brick_kind list = [BK_simple;BK_block;BK_bonus;BK_double;BK_empty] in
  for i = 0 to Array.length((l_mat.brick_wall)) -1 do
    for j = 0 to Array.length((l_mat.brick_wall).(i)) -1 do
      (
        if !l_type_found =[] then
          l_type_found := (l_mat.brick_wall).(i).(j) :: !l_type_found
        else 
          let is_not_in_list : bool ref = ref true in
          for k=0 to List.length(!l_type_found)-1 do
            
            if List.nth !l_type_found k = (l_mat.brick_wall).(i).(j) then
              is_not_in_list:=false;
          done;
          if !is_not_in_list then
            l_type_found := (l_mat.brick_wall).(i).(j) :: !l_type_found
            
      )
    done;
  done;
  assert_similar_list (!(l_type_found), l_types)
;;

(**Cette fonction vérifie la fonction make_ball
@author Emilio Decaix    
*) 
let test_make_ball() : unit =
  let l_res: t_ball t_test_result = test_exec (make_ball , "make_ball", (100,100,5)) in
  assert_equals_result (ball, l_res)
;;
(********************************************************************************************************************************)

test_reset_report();;

test_make_vec2 ();;
test_vec2_add ();;
test_vec2_mult ();;

test_brick_get1 ();;
test_brick_get2 ();;
test_brick_get3 ();;
test_brick_get4 ();;

test_brick_hit1 ();;
test_brick_hit2 ();;
test_brick_hit3 ();;
test_brick_hit4 ();;

test_brick_color1 ();;
test_brick_color2 ();;
test_brick_color3 ();;
test_brick_color4 ();;

test_aux_brick_color1 ();;
test_aux_brick_color2 ();;
test_aux_brick_color3 ();;
test_aux_brick_color4 ();;

test_make_paddle ();;
test_make_ball ();;
test_make_size ();;
test_make_camlbrick ();;
test_make_ball ();;

test_report();;