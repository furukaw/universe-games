open UniverseJs
open World
open Image
open Color

type color = White | Black
type state = U | W | B | N

type vector = int * int

type group = {
  cells : cell list;
  id : int;
  size : int option;
  color : color;
}

and cell = {
  pos : vector;
  state : state;
  group : group option;
}

type world_t = {
  cells : cell list;
  message : string;
}

let lines = 8
let rows = 6
let length = 64

let rec make_initial_cells (x : int) (y : int) : cell list =
  if lines < y then []
  else if rows < x then make_initial_cells 1 (y + 1)
  else {pos = (x, y); state = U; group = None} :: make_initial_cells (x + 1) y

let initial_world = {
  cells = make_initial_cells 1 1;
  message = "start"
}

let width = rows * length
let height = lines * length
let fwidth = float_of_int width
let fheight = float_of_int height
let background = empty_scene fwidth fheight

let draw ({cells; message} : world_t) : Image.t =
  let message_image = text message black in
  place_image
    message_image
    (0., 0.)
    background

let on_mouse world x y up_down = match up_down with
  | "button_up" -> {world with message = "up"}
  | _ -> {world with message = up_down}

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"kabe"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_mouse:on_mouse
    ~rate:1.			(* ゲームの動く速さ *)
    
