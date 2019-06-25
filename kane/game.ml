open UniverseJs
open World
open Image
open Color

let time_per_second = 10
let time_limit = 1200
let move_speed = 16.
let truly_random = true

;;if truly_random then Random.self_init () else Random.init 0

type yen_t = Y1 | Y5 | Y10 | Y50 | Y100 | Y500 | No

type cell_t = {
  pos : (int * int);
  yen : yen_t;
  moving : (float * float) option;
}

type world_t = {
  cells : cell_t list;
  score : int;
  time : int;
  message : string;
  select : (int * int) option;
}

let int_to_yen i =
  let mod6 = i mod 6 in
  if mod6 = 0 then Y1
  else if mod6 = 1 then Y5
  else if mod6 = 2 then Y10
  else if mod6 = 3 then Y50
  else if mod6 = 4 then Y100
  else Y500

let yen_exist yen = match yen with
  | No -> false
  | a -> true

let next_yen yen = match yen with
  | Y1 -> Y5
  | Y5 -> Y10
  | Y10 -> Y50
  | Y50 -> Y100
  | Y100 -> Y500
  | Y500 -> No
  | No -> No

let yen_need yen = match yen with
  | Y1 | Y10 | Y100 -> 5
  | Y5 | Y50 | Y500 -> 2
  | No -> max_int

let rec yen_upgrade yen num =
  let need = yen_need yen in
  if need <= num
  then yen_upgrade (next_yen yen) (num / need)
  else yen

let rate = (1. /. (float_of_int time_per_second))

let lines = 8
let rows = 12
let top = 1
let bottom = 0

let length = 64	(* １マスの辺の長さ *)

let width	= rows * length + 1
let height	= (lines + top + bottom) * length + 1

let background_image = read_image "images/background.png"
let yen1_image = read_image "images/yen1.png"
let yen5_image = read_image "images/yen5.png"
let yen10_image = read_image "images/yen10.png"
let yen50_image = read_image "images/yen50.png"
let yen100_image = read_image "images/yen100.png"
let yen500_image = read_image "images/yen500.png"

let yen_to_image yen = match yen with
  | Y1 -> yen1_image
  | Y5 -> yen5_image
  | Y10 -> yen10_image
  | Y50 -> yen50_image
  | Y100 -> yen100_image
  | Y500 -> yen500_image
  | _ -> empty_scene 40. 40.

let rec make_initial_cells x y =
  if rows < x then []
  else if lines < y then make_initial_cells (x + 1) 1
  else {
    pos = (x, y);
    yen = int_to_yen (Random.int 6);
    moving = None;
  } :: make_initial_cells x (y + 1)

let initial_world = {
  cells = make_initial_cells 1 1;
  score = 0;
  time = 0;
  message = "";
  select = None;
}

let sanketa (i : int) : string =
  let str = string_of_int i in
  if i < 10 then "00" ^ str
  else if i < 100 then "0" ^ str
  else str

let rokketa (i : int) : string =
  let str = string_of_int i in
  if i < 10 then "          " ^ str
  else if i < 10000 then "    " ^ str
  else if i < 100000 then "  " ^ str
  else str

let time_to_second time =
  (time_limit - time) / time_per_second

let draw ({cells; score; time; message} : world_t) : Image.t =
  let field = 
    let cells_with_yen = List.filter (fun {yen} -> yen_exist yen) cells in
    let poss =
      List.map
        (fun {pos = (x, y); moving} ->
           let (xm, ym) = match moving with
             | None -> (0., 0.)
             | Some (x, y) -> (x, y) in
           (float_of_int (length * (x - 1)) +. xm,
            float_of_int (length * (top + y - 1)) +. ym))
        cells_with_yen in
    let images = List.map (fun {yen} -> yen_to_image yen) cells_with_yen in
    place_images images poss background_image in
  let with_texts =
    let time_str = sanketa (time_to_second time) in
    let score_str = rokketa score in
    let time_text = text time_str ~size:30 blue in
    let score_text = text score_str ~size:30 blue in
    let yen_text = text "円" blue in
    place_images
      [time_text; score_text; yen_text]
      [(30., 15.); (600., 15.); (725., 23.)]
      field in
  match message with
  | "" -> with_texts
  | _ ->
    place_image (text message blue) (350., 20.) with_texts

let zahyou_to_pos_op x y =
  let x = int_of_float x in
  let y = int_of_float y in
  if y < length + 1 || width - 1 <= x || height - 1 <= y
  then None
  else Some ((x + 1) / 64 + 1, ((y + 1) / 64))

let pos_to_string (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let next (xa, ya) (xb, yb) =
  if xa = xb then (abs (ya - yb)) = 1
  else if ya = yb then (abs (xa - xb)) = 1
  else false

let swap world (xa, ya) (xb, yb) =
  let (ab, rest) =
    List.partition
      (fun {pos = (x, y)} -> x = xa && y = ya || x = xb && y = yb)
      world.cells in
  let new_cells =
    match ab with
    | [{yen = c_yen} as c; {yen = d_yen} as d] ->
      {c with yen = d_yen} :: {d with yen = c_yen} :: rest
    | _ -> world.cells in
  {world with cells = new_cells}

let group (cells : cell_t list) (cell : cell_t) : cell_t list * cell_t list =
  let rec group inner border outer others = match border with
    | [] -> (inner, outer @ others)
    | {pos = posf; yen = yenf} as first :: rest ->
      let (next_to_first, new_others) =
        List.partition
          (fun {pos} -> next pos posf)
          others in
      let (new_border, new_outer) =
        List.partition
          (fun {yen} -> yen = yenf)
          next_to_first in
      group
        (first :: inner)
        (new_border @ rest)
        (new_outer @ outer)
        new_others in
  group [] [cell] [] cells

let click world (x, y) =
  let (target, other_cells) =
    List.partition
      (fun {pos} -> pos = (x, y))
      world.cells in
  let (inner, outer) = group other_cells (List.hd target) in
  let (clicked, other_inner) =
    List.partition
      (fun {pos} -> pos = (x, y))
      inner in
  let clicked = List.hd clicked in
  let (new_inner, add_score) =
    let num = List.length other_inner + 1 in
    if num < yen_need clicked.yen
    then (inner, 0)
    else
      let new_yen = yen_upgrade clicked.yen num in
      ({clicked with yen = new_yen}
       :: List.map (fun cell -> {cell with yen = No}) other_inner,
       if new_yen = No then 1000 else 0) in
  {world with cells = new_inner @ outer;
              score = world.score + add_score}

let on_valid_click ({select} as world) (x, y) =
  match select with
  | None ->
    {world with select = None}
  | Some (dx, dy) ->
    if x = dx && y = dy
    then
      {(click world (x, y)) with
       select = None}
    else if next (x, y) (dx, dy)
    then
      {(swap world (x, y) (dx, dy)) with select = None}
    else
      {world with select = None}

let on_mouse world x y mouse =
  match mouse with
  | "button_down" ->
    begin
      let pos = zahyou_to_pos_op x y in
      {world with select = pos}
    end
  | "button_up" ->
    begin
      let pos = zahyou_to_pos_op x y in
      match pos with
      | None -> {world with select = None}
      | Some pos -> 
        let new_world = on_valid_click world pos in
        new_world
    end
  | _ -> world

let inc_time world = {world with time = world.time + 1}

let compare_y {pos = (xa, ya)} {pos = (xb, yb)} =
  if ya < yb
  then 1
  else if ya = yb then 0
  else -1

let move f = f +. move_speed

let fall no_cells all_cells =
  let rec fall no_cells all_cells = match no_cells with
    | [] -> all_cells
    | {pos = (xf, yf)} as first :: rest ->
      if yf = 1
      then
        let new_first =
          {first with
           yen = int_to_yen (Random.int 6);
           moving = None} in
        let others =
          List.filter
            (fun {pos = (x, y)} -> not (x = xf && y = yf))
            all_cells in
        fall rest (new_first :: others)
      else
        let (old_first, others) =
          List.partition (fun {pos} -> pos = (xf, yf)) all_cells in
        let old_first = List.hd old_first in
        let (old_parent, others) =
          List.partition (fun {pos = (x, y)} -> x = xf && y = yf - 1) others in
        let old_parent = List.hd old_parent in
        let new_all_cells =
          {old_first with
           yen = old_parent.yen;
           moving = None}
          :: {old_parent with yen = No; moving = None}
          :: others in
        fall rest new_all_cells
  in
  let sorted_no_cells = List.sort compare_y no_cells in
  fall sorted_no_cells all_cells

let move_on_tick world =
  let no_cells = List.filter (fun {yen} -> yen = No) world.cells in
  let new_all_cells = fall no_cells world.cells in
  {world with cells = new_all_cells}

let on_tick world =
  let world = inc_time world in
  let moved_world = move_on_tick world in
  moved_world

let stop_when {time} = time_limit <= time
let to_draw_last world =
  let last_world = {world with message = "TIME UP"} in
  draw last_world

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"kane"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_mouse:on_mouse
    ~on_tick:on_tick
    ~stop_when:stop_when
    ~to_draw_last:to_draw_last
    ~rate:rate			(* ゲームの動く速さ *)
    
