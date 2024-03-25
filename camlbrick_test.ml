#use "CPtest.ml";;
#use "camlbrick.ml";;

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
(*brick_get*)
let test_brick_get1 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(0,1)", (0,1)) in
  assert_equals_result_m ("result_brick_get 1", BK_simple, l_res)
;;

let test_brick_get2 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(0,0)", (0,0)) in
  assert_equals_result_m ("result_brick_get 2", BK_empty, l_res)
;;

let test_brick_get3 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(1,1)", (1,1)) in
  assert_equals_result_m ("result_brick_get 3", BK_double, l_res)
;;

let test_brick_get4 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_get,"brick_get(1,0)", (1,0)) in
  assert_equals_result_m ("result_brick_get 4", BK_bonus, l_res)
;;

(*brick_hit / à refaire car je comprend pas la signature de la fonction*)

let test_brick_hit1 () : unit =
  let l_res : t_brick_kind t_test_result = test_exec (brick_hit,"brick_hit(1,1)", (1,1)) in
  assert_equals_result_m ("result_brick_hit1", BK_double, l_res)
;;

(*brick_color*)
let init_game() : t_camlbrick = 
  let game_test : t_camlbrick = {           
          brick_wall : mat_make(...) ; 
          param = make_camlbrick_param();
          } in
  (
    brick_wall.(1).(1) <- BK_block ;
    brick_wall.(0).(1) <- BK_simple ;
    brick_wall.(1).(0) <- BK_double ;
    brick_wall.(0).(0) <- Bk_empty ;

  );
  brick_wall
;;


let test_brick_color1 () : unit =
  let game : t_camlbrick = int_game() in
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(1,1)", (game,1,1)) in
  assert_equals_result_m ("result_brick_color1", BLACK, l_res)
;;

let test_brick_color2 () : unit =
  (***********************)
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(0,1)", (0,1)) in
  assert_equals_result_m ("result_brick_color2", GREEN, l_res)
;;

let test_brick_color3 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(1,0)", (1,0)) in
  assert_equals_result_m ("result_brick_color3", ORANGE, l_res)
;;

let test_brick_color4 () : unit =
  let l_res : t_camlbrick_color t_test_result = test_exec (brick_color,"brick_color(0,0)", (0,0)) in
  assert_equals_result_m ("result_brick_color4", GREY, l_res)
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

test_brick_color1 ();;
test_brick_color2 ();;
test_brick_color3 ();;
test_brick_color4 ();;

test_aux_brick_color1 ();;
test_aux_brick_color2 ();;
test_aux_brick_color3 ();;
test_aux_brick_color4 ();;

test_report();;