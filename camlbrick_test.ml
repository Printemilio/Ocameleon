#use "CPtest.ml";;
#use "camlbrick.ml";;

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

(*****************************************)

test_reset_report();;

test_make_vec2 ();;
test_vec2_add ();;
test_vec2_mult ();;

test_report();;