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

(*aux_brick_color*)

type t_camlbrick_color = RED | BLUE 
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_LARGE 
type t_caml_table_paddle = { x: int ref; y: int } 

let mat_make (n, m, init) = { x = ref (snd init); y = fst init } 

type t_paddle = {
  paddle_color: t_camlbrick_color;
  paddle_size: t_paddle_size;
  paddle_wall: t_caml_table_paddle;
};;

let make_paddle () : t_paddle =
  let pos: t_caml_table_paddle = mat_make (1, 800, { x = ref 400; y = 50 }) in
  {
    paddle_color = RED;
    paddle_size = PS_MEDIUM;
    paddle_wall = pos
  }
;;

let test_make_paddle () =
  let result = make_paddle () in
  assert (result.paddle_color = RED);
  assert (result.paddle_size = PS_MEDIUM);
  assert (!(result.paddle_wall.x) = 400);
  assert (result.paddle_wall.y = 50);
  print_endline "All tests passed for make_paddle."
;;

(*make_camlbrick*)

let mat_make (n, m, value) = n + m + value 
let make_camlbrick_param () = { dummy = 42 } 

type t_paddle = {
  paddle_color: t_camlbrick_color;
  paddle_size: t_paddle_size;
  paddle_wall: t_caml_table_paddle;
};;

let make_paddle () : t_paddle =
  let pos: t_caml_table_paddle = { x = ref 400; y = 50 } in 
  { paddle_color = RED; paddle_size = PS_MEDIUM; paddle_wall = pos };;

type t_camlbrick = {
  brick_wall: t_caml_table;
  param: t_camlbrick_param;
  paddle_track: t_paddle;
};;

let make_camlbrick () : t_camlbrick =
  {
    brick_wall = mat_make (20, 30, BK_empty);
    param = make_camlbrick_param ();
    paddle_track = make_paddle ()
  }
;;

let test_make_camlbrick () =
  let result = make_camlbrick () in
  assert (result.brick_wall = 20 + 30 + BK_empty);
  assert (result.param.dummy = 42);
  assert (result.paddle_track.paddle_color = RED);
  assert (!(result.paddle_track.paddle_wall.x) = 400);
  assert (result.paddle_track.paddle_wall.y = 50);
  print_endline "All tests passed for make_camlbrick."
;;

(**************************caml_table*********************************************************************)

let test_caml_table_1 () : unit =
  let l_res : t_caml_table t_test_result = test_exec (caml_table,"caml_table(10,30)", (10,30)) in
  assert_equals_result_m ("result_caml_table_1", t_caml_table, l_res)
;;

let test_caml_table_2 () : unit =
  let l_res : t_caml_table t_test_result = test_exec (caml_table,"caml_table(-10,0)", (-10,0)) in
  assert_equals_result_m ("result_caml_table_2", t_caml_table, l_res)
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
test_make_camlbrick ();;

test_caml_table_1 ();;
test_caml_table_2 ();;

test_report();;
