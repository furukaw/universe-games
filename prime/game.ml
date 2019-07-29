open UniverseJs
open World
open Image
open Color

type world_t = {
  message : string;
}

let initial_world = {
  message = "start"
}

let width = 300
let height = 450
let rate = 1.
let fwidth = float_of_int width
let fheight = float_of_int height

let background : Image.t = empty_scene fwidth fheight

let button_color : Color.t = make_color 255 255 200

let button_width = 50.
let button_height = 30.

let ground : Image.t =
  let keys : string list =
    ["１"; "２"; "３"; "４"; "５"; "６"; "７"; "８"; "９"; "０"] in
  let buttons : Image.t list =
    List.map
      (fun i ->
         place_image
           (text i ~size:20 gray30)
           (14., 3.)
           (place_image
              (rectangle button_width button_height
                 ~fill:false ~outline_size:1. gray20)
              (0., 0.)
              (rectangle button_width button_height button_color))) keys in
  let x1 = (fwidth *. 0.2) -. (button_width /. 2.) in
  let x2 = (fwidth *. 0.5) -. (button_width /. 2.) in
  let x3 = (fwidth *. 0.8) -. (button_width /. 2.) in
  let y1 = 200. in
  let y2 = 260. in
  let y3 = 320. in
  let y4 = 380. in
  let poss : (float * float) list =
    [(x1, y1); (x2, y1); (x3, y1);
     (x1, y2); (x2, y2); (x3, y2);
     (x1, y3); (x2, y3); (x3, y3);
     (x2, y4)] in
  place_images buttons poss background

let draw world = ground

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"prime"
    ~width:width
    ~height:height
    ~to_draw:draw
    (* ~on_mouse:on_mouse
     * ~on_tick:on_tick
     * ~stop_when:stop_when
     * ~to_draw_last:to_draw_last *)
    ~rate:rate			(* ゲームの動く速さ *)
    
