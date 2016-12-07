
type point =
  { x : float;
    y : float; }

(* Represented as JSON { "x" : 1.0, "y" : 2.0 } *)

let codec : point Jsont.codec =
  let objc = Jsont.objc ~kind:"point" () in
  let mem_x = Jsont.mem objc "x" Jsont.float in
  let mem_y = Jsont.mem objc "y" Jsont.float in
  let obj_codec = Jsont.obj objc in
  let decoder o =
    `Ok { x = Jsont.get mem_x o;
          y = Jsont.get mem_y o; }
  in
  let encoder p =
    Jsont.(new_obj obj_codec [ memv mem_x p.x; memv mem_y p.y ])
  in
  Jsont.view (decoder, encoder) obj_codec
