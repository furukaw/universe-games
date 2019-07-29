open UniverseJs
open World
open Image
open Color

type world_t = {
  mouse : string;
  key : string;
}

let initial_world = {
  mouse = "";
  key = "";
}

let width = 400
let height = 60

let draw (world : world_t) : Image.t =
  let mouse_text = text ("mouse: " ^ world.mouse) black in
  let key_text = text ("key: " ^ world.key) black in
  place_images
    [mouse_text; key_text; circle ~fill:false 5. black]
    [(0., 0.); (0., 30.); (5., 5.)]
    (empty_scene (float_of_int width) (float_of_int height))

let on_mouse world (x : float) (y : float) (command : string) : world_t =
  {world with mouse =
                "(" ^ (string_of_float x) ^ ", " ^ string_of_float y ^ ")  "
                ^ command}
let on_key_press world key_name : world_t =
  {world with key = key_name ^ " pressed"}
let on_key_release world key_name : world_t =
  {world with key = key_name ^ " released"}

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"test"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_mouse:on_mouse
    ~on_key_press:on_key_press
    ~on_key_release:on_key_release
    ~rate:1.			(* ゲームの動く速さ *)
    
