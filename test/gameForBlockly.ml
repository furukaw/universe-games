open UniverseJs
open World
open Image
open Color
open TransformToInt

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

let draw {mouse = mouse; key = key} =
  let mouse_text = text ("mouse: " ^ mouse) 20 Color.black in
  let key_text = text ("key: " ^ key) 20 Color.black in
  place_images
    [mouse_text; key_text]
    [(0, 0); (0, 30)]
    (empty_scene width height)
    
let on_mouse {mouse = mouse; key = key} x y command =
  {mouse =
     "(" ^ (string_of_int x) ^ ", " ^ string_of_int y ^ ")  "
     ^ command;
   key = key}

let on_key_press {mouse = mouse; key = key} key_name =
  {key = key_name ^ " pressed";
  mouse = mouse}

let on_key_release {mouse = mouse; key = key} key_name =
  {key = key_name ^ " released";
  mouse = mouse}

;;big_bang
    initial_world
    ~name:"test"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_mouse:on_mouse
    ~on_key_press:on_key_press
    ~on_key_release:on_key_release

