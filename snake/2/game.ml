open UniverseJs
open World
open Image
open Color

type direction = Ue | Shita | Hidari | Migi

type vector = int * int

type world_t = {
  head1 : vector;
  body1 : (int * vector) list;
  head2 : vector;
  body2 : (int * vector) list;
  others : vector list;
  length1 : int;
  length2 : int;
  esa : vector list;
  last1 : direction;
  direction1 : direction;
  last2 : direction;
  direction2 : direction;
  pause : bool;
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

let initial_head1 = (22, 16)
let initial_head2 = (11, 16)
let create_initial_others_and_esa () =
  let rec loop x y =
    if y > lines then []
    else if x > rows then loop 1 (y + 1)
    else if (x, y) = initial_head1 || (x, y) = initial_head2
    then loop (x + 1) y
    else (x, y) :: loop (x + 1) y in
  loop 1 1

let rec generate_esa others =
  let new_esa_num = Random.int (List.length others) in
  let new_esa = List.nth others new_esa_num in
  let (esa_list, others) = List.partition ((=) new_esa) others in
  (List.hd esa_list, others)

let (initial_esa, initial_others) =
  let others_and_esas = create_initial_others_and_esa () in
  let (esa1, others1) = generate_esa others_and_esas in
  let (esa2, others2) = generate_esa others1 in
  let (esa3, others3) = generate_esa others2 in
  ([esa1; esa2; esa3], others3)

let initial_world = {
  head1 = initial_head1;
  body1 = [];
  head2 = initial_head2;
  body2 = [];
  others = initial_others;
  length1 = 1;
  length2 = 1;
  esa = initial_esa;
  last1 = Ue;
  direction1 = Ue;
  last2 = Ue;
  direction2 = Ue;
  pause = false;
}

let background =
  place_image
    (rectangle
       (float_of_int (size * rows))
       (float_of_int (size * lines))
       ~fill:true black)
    (fsize, fsize)
    (rectangle fwidth fheight ~fill:true gray70)
let p1 = rectangle fsize fsize ~fill:true green
let p2 = rectangle fsize fsize ~fill:true blue
let esa = rectangle fsize fsize ~fill:true red
let to_pos (x, y) =
  (float_of_int (x * size), float_of_int (y * size))

let draw (world : world_t) =
  let score1 = text (string_of_int world.length1) ~size:(size - 2) green in
  let score2 = text (string_of_int world.length2) ~size:(size - 2) blue in
  place_images
    [p1;  p2; score1; score2; esa; esa; esa]
    (to_pos world.head1 :: to_pos world.head2
     :: (fwidth -. 40., 0.) :: (30., 0.)
     :: List.map to_pos world.esa)
    (place_images
       (List.map (fun _ -> p1) world.body1)
       (List.map (fun (_, xy) -> to_pos xy) world.body1)
       (place_images
          (List.map (fun _ -> p2) world.body2)
          (List.map (fun (_, xy) -> to_pos xy) world.body2)
          background))

let move (x, y) dir = match dir with
  | Ue -> (x, y - 1)
  | Shita -> (x, y + 1)
  | Hidari -> (x - 1, y)
  | Migi -> (x + 1, y)

let on_tick ({head1; body1; head2; body2; others; length1; length2;
              direction1; direction2; esa; pause} as world
             : world_t) : world_t =
  if pause then world
  else
    let world1 =
      let new_head1 = move head1 direction1 in
      if List.exists ((=) new_head1) esa
      then
        let new_body1 =
          (2, head1)
          :: (List.map (fun (n, xy) -> (n + 1, xy)) body1) in
        let new_length1 = length1 + 1 in
        let new_esa = List.filter ((<>) new_head1) esa in
        {world with
         head1 = new_head1;
         body1 = new_body1;
         length1 = new_length1;
         last1 = world.direction1;
         esa = new_esa}
      else
        let (other, new_body1) =
          List.partition (fun (n, _) -> n > length1)
            ((2, head1)
             :: (List.map (fun (n, xy) -> (n + 1, xy)) body1)) in
        let new_others =
          if other = [] then (List.filter ((<>) new_head1) others)
          else
            (snd (List.hd other))
            :: (List.filter ((<>) new_head1) others) in
        {world with
         head1 = new_head1;
         body1 = new_body1;
         others = new_others;
         last1 = direction1} in
    let world2 =
      let new_head2 = move head2 direction2 in
      if List.exists ((=) new_head2) world1.esa
      then
        let new_body2 =
          (2, head2)
          :: (List.map (fun (n, xy) -> (n + 1, xy)) body2) in
        let new_length2 = length2 + 1 in
        let new_esa = List.filter ((<>) new_head2) world1.esa in
        {world1 with
         head2 = new_head2;
         body2 = new_body2;
         length2 = new_length2;
         last2 = direction2;
         esa = new_esa}
      else
        let (other, new_body2) =
          List.partition (fun (n, _) -> n > length2)
            ((2, head2)
             :: (List.map (fun (n, xy) -> (n + 1, xy)) body2)) in
        let new_others =
          if other = [] then (List.filter ((<>) new_head2) world1.others)
          else
            (snd (List.hd other))
            :: (List.filter ((<>) new_head2) world1.others) in
        {world1 with
         head2 = new_head2;
         body2 = new_body2;
         others = new_others;
         last2 = direction2} in
    let esa_num = List.length world2.esa in
    let (new_esa, new_others) =
      match esa_num with
      | 1 ->
        let (esa1, others1) = generate_esa world2.others in
        let (esa2, others2) = generate_esa others1 in
        (esa1 :: esa2 :: world2.esa, others2)
      | 2 ->
        let (esa1, others1) = generate_esa world2.others in
        (esa1 :: world2.esa, others1)
      | _ -> (world2.esa, world2.others) in
    {world2 with esa = new_esa; others = new_others}

let can_turn old dir = match (old, dir) with
  | (Ue, Shita) | (Shita, Ue) | (Hidari, Migi) | (Migi, Hidari) -> None
  | _ -> Some dir

let key_to_dir1 old key = match key with
  | "up" | "I" -> can_turn old Ue
  | "down" | "K" -> can_turn old Shita
  | "left" | "J" -> can_turn old Hidari
  | "right" | "L" -> can_turn old Migi
  | _ -> None

let key_to_dir2 old key = match key with
  | "W" -> can_turn old Ue
  | "S" -> can_turn old Shita
  | "A" -> can_turn old Hidari
  | "D" -> can_turn old Migi
  | _ -> None

let on_key_release (world : world_t) (key : string) =
  if world.pause
  then if key = " " then {world with pause = false}
    else world
  else if key = " " then {world with pause = true}
  else
    let new_direction1 = match key_to_dir1 world.last1 key with
      | Some (dir) -> dir
      | None -> world.direction1 in
    let new_direction2 = match key_to_dir2 world.last2 key with
      | Some (dir) -> dir
      | None -> world.direction2 in
    {world with direction1 = new_direction1; direction2 = new_direction2}

let on_mouse world x y ud =
  match ud with
  | "button_up" -> {world with pause = not world.pause}
  | _ -> world

let stop_when ({head1; head2; body1; body2} : world_t) : bool =
  let (x1, y1) = head1 in
  let (x2, y2) = head2 in
  x1 = 0 || x1 > rows || y1 = 0 || y1 > lines
  || x2 = 0 || x2 > rows || y2 = 0 || y2 > lines
  || head1 = head2
  || List.exists (fun (_, xy) -> xy = head1) body1
  || List.exists (fun (_, xy) -> xy = head1) body2
  || List.exists (fun (_, xy) -> xy = head2) body1
  || List.exists (fun (_, xy) -> xy = head2) body2

let blue_p = rectangle fsize fsize ~fill:true yellow

let to_draw_last (world : world_t) : Image.t =
  let score1 = text (string_of_int world.length1) ~size:(size - 2) green in
  let score2 = text (string_of_int world.length2) ~size:(size - 2) blue in
  let l1 =
    let (x1, y1) = world.head1 in
    x1 = 0 || x1 > rows || y1 = 0 || y1 > lines
    || world.head1 = world.head2
    || List.exists (fun (_, xy) -> xy = world.head1) world.body1
    || List.exists (fun (_, xy) -> xy = world.head1) world.body2 in
  let l2 =
    let (x2, y2) = world.head2 in
    x2 = 0 || x2 > rows || y2 = 0 || y2 > lines
    || List.exists (fun (_, xy) -> xy = world.head2) world.body1
    || List.exists (fun (_, xy) -> xy = world.head2) world.body2 in
  place_images
    [if l1 then blue_p else p1; if l2 then blue_p else p2;
     score1; score2; esa; esa; esa]
    (to_pos world.head1 :: to_pos world.head2
     :: (fwidth -. 40., 0.) :: (30., 0.)
     :: List.map to_pos world.esa)
    (place_images
       (List.map (fun _ -> p1) world.body1)
       (List.map (fun (_, xy) -> to_pos xy) world.body1)
       (place_images
          (List.map (fun _ -> p2) world.body2)
          (List.map (fun (_, xy) -> to_pos xy) world.body2)
          background))

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"snake2"
    ~width:width
    ~height:height
    ~to_draw:draw
    ~on_tick:on_tick
    ~on_key_release:on_key_release
    ~on_mouse:on_mouse
    ~stop_when:stop_when
    ~to_draw_last:to_draw_last
    ~rate:rate			(* ゲームの動く速さ *)
