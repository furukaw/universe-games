open UniverseJs
open World
open Image
open Color

type color = White | Black
type state = U | W | B | N of int
type vector = int * int
type process =
  | Input of vector option
  | Solve

type button =
  | BSolve
  | BInput
  | BReset

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
  process : process;
  message : string;
 }

let lines = 8
let rows = 6
let length = 64
let margin = 32
let button_width = 80
let button_height = 32
let button_padding = 10

let rec make_initial_cells (x : int) (y : int) : cell list =
  if lines < y then []
  else if rows < x then make_initial_cells 1 (y + 1)
  else {pos = (x, y); state = U; group = None} :: make_initial_cells (x + 1) y

let initial_cells = make_initial_cells 1 1

let initial_world = {
   cells = initial_cells;
  process = Input None;
  message = "start"
}

let flength = float_of_int length
let width = rows * length + margin * 2
let height = lines * length + button_height + margin * 3
let fwidth = float_of_int width
let fheight = float_of_int height
let fmargin = float_of_int margin
let fbutton_width = float_of_int button_width
let fbutton_height = float_of_int button_height
let fbutton_padding = float_of_int button_padding
let button =
  place_image
    (rectangle fbutton_width fbutton_height ~fill:false ~outline_size:1. black)
    (0., 0.)
    (rectangle fbutton_width fbutton_height ~fill:true ~outline_size:0. gray90)
let solve_button =
  place_image
    (text "Solve" ~size:20 black)
    (fbutton_padding, 4.)
    button
let input_button =
  place_image
    (text "Input" ~size:20 black)
    (fbutton_padding, 4.)
    button
let reset_button =
  place_image
    (text "Reset" ~size:20 black)
    (fbutton_padding , 4.)
    button
let button_posy = height - margin - button_height
let background =
  place_images
    [input_button; solve_button; reset_button]
    [(float_of_int margin, float_of_int button_posy);
     (float_of_int (margin + margin + button_width), float_of_int button_posy);
     (float_of_int (margin * 3 + button_width * 2), float_of_int button_posy)]
    (empty_scene fwidth fheight)
let yellow_ = make_color ~alpha:0.5 255 255 100
let no_box =
  place_image
    (rectangle flength flength ~fill:false ~outline_size:1. black)
    (0., 0.)
    (rectangle flength flength ~fill:false ~outline_size:0. transparent)
let yellow_box =
  rectangle flength flength ~fill:true ~outline_size:0. yellow_
let white_box =
  place_image
    (circle 5. ~fill:true black)
    (flength /. 2. -. 5., flength /. 2. -. 5.)
    no_box
let black_box =
  place_image
    (rectangle flength flength ~fill:true ~outline_size:0. black)
    (0., 0.)
    no_box

let vector_to_xy ((x, y) : int * int) : float * float =
  (float_of_int ((x - 1) * length + margin),
   float_of_int ((y - 1) * length + margin))

let state_to_image state = match state with
  | U -> no_box
  | W -> white_box
  | B -> black_box
  | N n ->
    let str = match n with
      | 1 -> "１"
      | 2 -> "２"
      | 3 -> "３"
      | 4 -> "４"
      | 5 -> "５"
      | 6 -> "６"
      | 7 -> "７"
      | 8 -> "８"
      | 9 -> "９"
      | _ -> string_of_int n in
    if n < 10
    then
      place_image
        (text str ~size:(length - 20) black)
        (10., 6.)
      no_box
    else
      place_image
        (text str ~size:(length - 24) black)
        (5., 9.)
        no_box

let cell_to_image {state} = state_to_image state
let cell_to_xy {pos} = vector_to_xy pos

let draw ({cells; process; message} : world_t) : Image.t =
  let selected = match process with
    | Input (Some xy) ->
      place_image
        yellow_box
        (vector_to_xy xy)
        background
    | _ -> background in
  let with_cells =
    place_images
      (List.map cell_to_image cells)
      (List.map cell_to_xy cells)
      selected in
  with_cells
(* let with_message =
   *   place_image
   *     (text message black)
   *     (0., 0.)
   *     with_cells in
   * with_message *)

let xy_to_vector x y =
  let x = int_of_float x in
  let y = int_of_float y in
  let rx = (x - margin) / length + 1 in
   let ry = (y - margin) / length + 1 in
  if 1 <= rx && rx <= rows && 1 <= ry && ry <= lines
  then Some (rx, ry)
  else None

let xy_to_button x y =
  let x = int_of_float x in
  let y = int_of_float y in
  print_endline ("x = " ^ string_of_int x ^ ", y = " ^ string_of_int y);
  if height - button_height - margin < y && y < height - margin
  then
    if margin < x && x < margin + button_width
    then Some BInput 
    else if margin + button_width + margin < x
         && x < (margin + button_width) * 2
    then Some BSolve
    else if margin * 3 + button_width * 2 < x
         && x < (margin + button_width) * 3
    then Some BReset
    else None
  else None

let on_mouse world x y up_down = match up_down with
  | "button_up" ->
    let button = xy_to_button x y in
    begin match button with
      | Some BSolve -> {world with process = Solve}
      | Some BInput -> {world with process = Input None}
      | Some BReset ->
        {world with
         cells =
           List.map
             (fun cell ->
                let new_state = match cell.state with
                  | N n -> N n
                  | _ -> U in
                {cell with state = new_state})
             world.cells}
      | None ->
        begin match world.process with
          | Input _ ->
            let clicked_pos = xy_to_vector x y in
            begin match clicked_pos with
              | Some pos ->
                {world with process = Input (Some pos)}
              | None ->
                {world with process = Input None}
            end
          | Solve ->
            let clicked_pos = xy_to_vector x y in
            begin match clicked_pos with
              | Some clicked ->
                let (target_in_list, others) =
                  List.partition (fun {pos} -> pos = clicked) world.cells in
                let target = List.hd target_in_list in
                let old_state = target.state in
                let new_state = match old_state with
                  | U -> B
                  | W -> U
                  | B -> W
                  | N n -> old_state in
                {world with cells = {target with state = new_state} :: others}
              | None -> world
            end
        end
    end
  | _ -> world
    
let on_key_release ({cells; process} as world) key = match process with
  | Input (Some xy) ->
    begin match key with
      | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0" ->
        let (target_in_list, others) =
          List.partition (fun {pos} -> pos = xy) cells in
        let target = List.hd target_in_list in
        let old_num = match target.state with
          | N n -> Some n
          | _ -> None in
        let new_num = match old_num with
          | Some 0 | None -> int_of_string key
          | Some n -> n * 10 + (int_of_string key) in
        if new_num < 1 then world
        else {world with cells = {target with state = N new_num} :: others}
      | "\\b" ->
        let (target_in_list, others) =
          List.partition (fun {pos} -> pos = xy) cells in
        let target = List.hd target_in_list in
        let old_num = match target.state with
          | N n -> Some n
          | _ -> None in
        let new_num = match old_num with
          | None -> 0
          | Some n -> n / 10 in
        {world with
         cells = {target with state = if new_num < 1 then U else N new_num}
                 :: others}
      | _ -> {world with message = "(" ^ key ^ ")"}
    end
  | _ -> world

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"kabe"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_mouse:on_mouse
    ~on_key_release:on_key_release
    ~rate:1.			(* ゲームの動く速さ *)
    
