#use "CPtest.ml";;
(*#use "camlbrick.ml";;*)

(*****************Test Vecteur******************************************************************************************************)

let test_make_vec2 () : unit =
  let l_res : t_vec2 t_test_result = test_exec (make_vec2,"make_vec2(3,0)", (3,0)) in
  assert_equals_result_m ("result_make_vec2", {dx=3;dy=0}, l_res)
;;

let test_vec2_add () : unit =
  let l_res : t_vec2 t_test_result = test_exec (vec2_add,"vec2_add(3,0)(3,0)", ({dx = 3; dy = 0},{dx = 3; dy = 0})) in
  assert_equals_result_m ("result_vec2_add", ({dx = 6; dy = 0}), l_res)
;;

let test_vec2_mult () : unit =
  let l_res : t_vec2 t_test_result = test_exec (vec2_mult,"vec2_mult(3.0)(3.0)", ({dx = 3; dy = 0},{dx = 3; dy = 0})) in
  assert_equals_result_m ("result_vec2_add", ({dx = 9; dy = 0}), l_res)
;;
   
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

(*brick_get*)
let test_brick_get1 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(0,1)", (init_game(),0,1)) in
  assert_equals_result_m ("result_brick_get 1", BK_simple, l_res)
;;

let test_brick_get2 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(0,0)", (init_game(),0,0)) in
  assert_equals_result_m ("result_brick_get 2", BK_empty, l_res)
;;

let test_brick_get3 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(1,1)", (init_game(),1,1)) in
  assert_equals_result_m ("result_brick_get 3", BK_block, l_res)
;;

let test_brick_get4 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(1,0)", (init_game(),1,0)) in
  assert_equals_result_m ("result_brick_get 4", BK_double, l_res)
;;

(*brick_hit*)

let test_brick_hit1 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(1,0)", (init_game(),1,0)) in
  assert_equals_result_m ("result_brick_hit1",  BK_simple, l_res)
;;

let test_brick_hit2 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(1,1)", (init_game(),1,1)) in
  assert_equals_result_m ("result_brick_hit2", BK_block, l_res)
;;

let test_brick_hit3 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(0,1)", (init_game(),0,1)) in
  assert_equals_result_m ("result_brick_hit3", BK_empty, l_res)
;;

let test_brick_hit4 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(0,0)", (init_game(),0,0)) in
  assert_equals_result_m ("result_brick_hit4", BK_empty, l_res)
;;

(*brick_color*)

let test_brick_color1 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(1,1)", (init_game(),1,1)) in
  assert_equals_result_m ("result_brick_color1", BLACK, l_res)
;;

let test_brick_color2 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(0,1)", (init_game(),0,1)) in
  assert_equals_result_m ("result_brick_color2", GREEN, l_res)
;;

let test_brick_color3 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(1,0)", (init_game(),1,0)) in
  assert_equals_result_m ("result_brick_color3", ORANGE, l_res)
;;

let test_brick_color4 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(0,0)", (init_game(),0,0)) in
  assert_equals_result_m ("result_brick_color4", GRAY, l_res)
;;

(*aux_brick_color*)

let test_aux_brick_color1 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_block)", (BK_block)) in
  assert_equals_result_m ("result_aux_brick_color1", BLACK, l_res)
;;

let test_aux_brick_color2 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_simple)", (BK_simple)) in
  assert_equals_result_m ("result_aux_brick_color2", GREEN, l_res)
;;

let test_aux_brick_color3 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_double)", (BK_double)) in
  assert_equals_result_m ("result_aux_brick_color3", ORANGE, l_res)
;;

let test_aux_brick_color4 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (aux_brick_color,"aux_brick_color(BK_empty)", (BK_empty)) in
  assert_equals_result_m ("result_aux_brick_color4", GRAY, l_res)
;;


type t_camlbrick_color = RED | BLUE 
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_LARGE 
type t_caml_table_paddle = { x: int ref; y: int } 

let mat_make (n, m, init) = { x = ref (snd init); y = fst init } 


(*test make_paddle, make_ball,make_size et make_camlbrick*)
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

let test_make_ball () =
  let result = make_ball (x,y, size : int * int * int) in
 
  assert (result.ball_size = ref size );
  assert (result.ball_coordonates = ref {dx = x ; dy = y});
  assert (result.ball_velocity = ref {dx= 10 ; dy= 10});
  assert (result.ball_color = YELLOW);
  print_endline "All tests passed for make_ball."
;;

(*make_size*)
let test_make_size1 () : unit =
  let l_res : unit t_test_result = test_exec (make_size,"make_size(5)", ({
    ball_size = ref 5 ; 
    ball_coordonates = ref {dx = 100 ; dy = 100}; 
    ball_velocity = ref {dx = 10 ; dy = 10};
    ball_color = YELLOW ;  
  },BS_SMALL)) in
  assert_equals_result_m ("result_make_size1", ball.ball_size, l_res)
;;

(*make_camlbrick*)

(*
let test_make_camlbrick_structural () =
  let camlbrick = make_camlbrick () in
  (* Vérifiez la structure du mur de briques *)
  assert (camlbrick.brick_wall = caml_table (20,30));
  (* Assurez-vous que les références et les listes sont initialisées *)
  assert (!(camlbrick.game_speed) = 1);
  assert (!(camlbrick.ball_list) = [{ ball_size = ref 10; 
                                      ball_coordonates = ref { dx = 100; dy = 100 }; 
                                      ball_velocity = ref { dx = 0; dy = 0 };
                                      ball_color = GREEN (* valeur de couleur attendue *) }]);
  (* Vérifiez que l'état de jeu est initialisé correctement *)
  assert (!(camlbrick.game_state) = PLAYING);
  (* Vérifiez que le paddle est correctement initialisé *)
  assert (camlbrick.paddle_track = { paddle_height = 20 (* valeur attendue *);
                                     paddle_width = ref 100 (* valeur attendue *);
                                     paddle_speed = ref 20 (* valeur attendue *);
                                     paddle_color = RED (* valeur de couleur attendue *);
                                     paddle_size = PS_MEDIUM (* taille de la raquette attendue *);
                                     paddle_position = ref { dx = 10; dy = 10 } (* position de la raquette attendue *) });
  (* Vérifiez que la balle est correctement initialisée *)
  assert (camlbrick.ball = { ball_size = ref 10; 
                             ball_coordonates = ref { dx = 100; dy = 100 }; 
                             ball_velocity = ref { dx = 0; dy = 0 };
                             ball_color = GREEN (* valeur de couleur attendue *) });
  print_endline "Structural test make_camlbrick passed!"
;;
*)

(**************************caml_table*********************************************************************)

let init_number

let test_caml_table_1 () : unit =
  let l_res : int*int*t_caml_table t_test_result = test_exec (caml_table,"caml_table(BK_EMPTY)", (0,0)) in
  assert_equals_result_m ("result_caml_table_1", caml_table, l_res)
;;

let test_caml_table_2 () : unit =
  let l_res : t_caml_table t_test_result = test_exec (caml_table,"caml_table(-10,0)", (-10,0)) in
  assert_equals_result_m ("result_caml_table_2", caml_table, l_res)
;;

(*****************************************)

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
(* test_make_camlbrick_structural ();; *)

test_caml_table_1 ();;
test_caml_table_2 ();;

test_report();;
