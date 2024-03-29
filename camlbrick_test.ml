#use "CPtest.ml";;
(*#use "camlbrick.ml";;*)

(*Test Vecteur*)

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

(*Test brick*)
(*
   type t_camlbrick ={
      brick_wall : t_caml_table ; 
      param : t_camlbrick_param
      }
    ;;

*)
   
let init_game() : t_camlbrick = 
  let prm :  t_camlbrick_param = make_camlbrick_param() 
  in
  let game_test : t_camlbrick = 
          {           
            brick_wall = mat_make(prm.world_width , prm.world_bricks_height, BK_empty ) ; 
            param = prm
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

test_report();;
