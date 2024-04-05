let animate_action(game : t_camlbrick) : unit =
  let world_width = game.param.world_width in
  let world_height = game.param.world_bricks_height + game.param.world_empty_height in
  
  game.ball_list <- List.map (fun ball ->
    (* Mise à jour de la position de la balle *)
    let new_position = {dx = ball.ball_coordonates.dx + ball.ball_velocity.dx; 
                        dy = ball.ball_coordonates.dy + ball.ball_velocity.dy} in
    let new_velocity = 
      (* Gestion des collisions avec les bords latéraux de l'écran *)
      let dx = if new_position.dx <= 0 || new_position.dx >= world_width then -ball.ball_velocity.dx else ball.ball_velocity.dx in
      (* Gestion des collisions avec le bord supérieur de l'écran *)
      let dy = if new_position.dy <= 0 then -ball.ball_velocity.dy else ball.ball_velocity.dy in
      {dx = dx; dy = dy} in
    {ball with ball_coordonates = new_position; ball_velocity = new_velocity}
  ) game.ball_list;
  ()
;;


let ball_modif_speed(game, ball, dv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.ball_velocity <- {dx = ball.ball_velocity.dx + dv.dx; dy = ball.ball_velocity.dy + dv.dy};
  ()
;;

let ball_modif_speed_sign(game, ball, sv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.ball_velocity <- {dx = ball.ball_velocity.dx * sv.dx; dy = ball.ball_velocity.dy * sv.dy};
  ()
;;

let is_inside_circle(cx, cy, rad, x, y : int * int * int * int * int) : bool =
  let square n = n * n in
  square(x - cx) + square(y - cy) < square(rad)
;;

let is_inside_quad(x1, y1, x2, y2, x, y : int * int * int * int * int * int) : bool =
  x >= min x1 x2 && x <= max x1 x2 && y >= min y1 y2 && y <= max y1 y2
;;

let ball_remove_out_of_border(game, balls : t_camlbrick * t_ball list) : t_ball list =
  let world_height = game.param.world_bricks_height + game.param.world_empty_height in
  List.filter (fun ball -> ball.ball_coordonates.dy < world_height) balls
;;
