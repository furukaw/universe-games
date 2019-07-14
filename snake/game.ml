open UniverseJs
open World
open Image
open Color

type direction = Ue | Shita | Hidari | Migi

type vector = int * int

type world_t = {
  head : vector;
  body : (int * vector) list;
  others : vector list;
  length : int;
  esa : vector;
  last_direction : direction;
  direction : direction;
}

;;Random.self_init ()

let rate = 0.1
let lines = 32
let rows = 32
let size = 16
let width = (rows + 2) * size
let height = (lines + 2) * size
let fwidth = float_of_int width
let fheight = float_of_int height
let fsize = float_of_int size

let initial_head = (16, 16)
let create_initial_others_and_esa head =
  let rec loop x y =
    if y > lines then []
    else if x > rows then loop 1 (y + 1)
    else if (x, y) = head then loop (x + 1) y
    else (x, y) :: loop (x + 1) y in
  loop 1 1

let rec generate_esa others =
  let new_esa_num = Random.int (List.length others) in
  let new_esa = List.nth others new_esa_num in
  let (esa_list, others) = List.partition ((=) new_esa) others in
  (List.hd esa_list, others)

let (initial_esa, initial_others) =
  generate_esa (create_initial_others_and_esa initial_head)

let initial_world = {
  head = initial_head;
  body = [];
  others = initial_others;
  length = 1;
  esa = initial_esa;
  last_direction = Ue;
  direction = Ue;
}

let background =
  place_image
    (rectangle
       (float_of_int (size * rows))
       (float_of_int (size * lines))
       ~fill:true black)
    (fsize, fsize)
    (rectangle fwidth fheight ~fill:true gray70)
let me = rectangle fsize fsize ~fill:true white
let esa = rectangle fsize fsize ~fill:true red
let to_pos (x, y) =
  (float_of_int (x * size), float_of_int (y * size))

let draw (world : world_t) =
  let score = text (string_of_int world.length) ~size:(size - 2) black in
  place_images
    [me; esa; score]
    [to_pos world.head; to_pos world.esa; (5., 0.)]
    (place_images
       (List.map (fun _ -> me) world.body)
       (List.map (fun (_, pos) -> to_pos pos) world.body)
       background)

let move (x, y) dir = match dir with
  | Ue -> (x, y - 1)
  | Shita -> (x, y + 1)
  | Hidari -> (x - 1, y)
  | Migi -> (x + 1, y)

let on_tick ({head; body; others; length; direction; esa} as world : world_t)
  : world_t =
  let new_head = move head direction in
  if new_head = esa
  then
    let new_body =
      (2, head)
      :: (List.map (fun (n, xy) -> (n + 1, xy)) body) in
    let new_length = length + 1 in
    let (new_esa, new_others) = generate_esa others in
    {world with
     head = new_head;
     body = new_body;
     others = new_others;
     length = new_length;
     last_direction = world.direction;
     esa = new_esa}
  else
    let (other, new_body) =
      List.partition (fun (n, _) -> n > length)
        ((2, head)
         :: (List.map (fun (n, xy) -> (n + 1, xy)) body)) in
    let new_others =
      if other = [] then (List.filter ((<>) new_head) others)
      else
        (snd (List.hd other))
        :: (List.filter ((<>) new_head) others) in
  {world with
   head = new_head;
   body = new_body;
   others = new_others;
   last_direction = world.direction}

let can_turn old dir = match (old, dir) with
  | (Ue, Shita) | (Shita, Ue) | (Hidari, Migi) | (Migi, Hidari) -> None
  | _ -> Some dir

let key_to_dir old key = match key with
  | "up" | "I" | "W" | "P" -> can_turn old Ue
  | "down" | "K" | "S" | "N" -> can_turn old Shita
  | "left" | "J" | "A" | "B" -> can_turn old Hidari
  | "right" | "L" | "D" | "F" -> can_turn old Migi
  | _ -> None
    
let on_key_release (world : world_t) (key : string) =
  match key_to_dir world.last_direction key with
  | Some (dir) -> {world with direction = dir}
  | None -> world

let stop_when ({head; body} : world_t) : bool =
  let (x, y) = head in
  x = 0 || x > rows || y = 0 || y > lines
  || List.exists (fun (_, xy) -> xy = head) body

let blue_me = rectangle fsize fsize ~fill:true blue

let to_draw_last (world : world_t) : Image.t =
  let score = text (string_of_int world.length) ~size:(size - 2) black in
  place_images
    [blue_me; esa; score]
    [to_pos world.head; to_pos world.esa; (5., 0.)]
    (place_images
       (List.map (fun _ -> me) world.body)
       (List.map (fun (_, pos) -> to_pos pos) world.body)
       background)

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"snake"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_tick:on_tick
    ~on_key_release:on_key_release
    ~stop_when:stop_when
    ~to_draw_last:to_draw_last
    ~rate:rate			(* ゲームの動く速さ *)
