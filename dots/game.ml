open UniverseJs
open World
open Image
open Color

(* 描く時の１マスの幅 *)
let length = 9

(* １辺のドット数 *)
let dots = 64

(* 画面の大きさ *)
let width = 1000
let height = 600

(* 左の部分の幅 *)
let bar_width = 200

(* 更新の速さ *)
let rate = 0.025

type cell_t = int * int * Color.t

type picture_t = {
  number : int;
  cells : cell_t list;
  time : int;
}

(* 全体の状態 *)
type world_t = {
  pictures : picture_t list;
  editing : picture_t;
  message : string;
}

let make_white_picture () : cell_t list =
  let rec f (x : int) (y : int) : cell_t list =
    if x > dots then []
    else if y > dots then f (x + 1) 1
    else (x, y, white) :: f x (y + 1)
  in
  f 1 1

;;Random.init 0
let make_random_picture () : cell_t list =
  let rec f x y =
    if x > dots then []
    else if y > dots then f (x + 1) 1
    else (x, y, make_color (Random.int 256) (Random.int 256) (Random.int 256))
         :: f x (y + 1)
  in
  f 1 1

(* 起動時の状態 *)
let initial_world = {
  pictures = [{number = 1; cells = make_white_picture (); time = 1}];
  editing = {number = 1; cells = make_random_picture (); time = 1};
  message = "start";
}

(* 実数での画面の大きさ（座標計算用） *)
let fwidth = float_of_int width
let fheight = float_of_int height
let flength = float_of_int length
let fbar_width = float_of_int bar_width

(* 描く部分の辺の長さ *)
let canvas_size = float_of_int (length * dots)
(* 描く部分の座標 *)
let canvas_x = (fwidth -. canvas_size) /. 2.
let canvas_y = (fheight -. canvas_size) /. 2.
let canvas_pos = (canvas_x, canvas_y)

(* 描く部分の大きさの白い長方形 *)
let empty_picture : Image.t = empty_scene canvas_size canvas_size

(* 描く部分のための画像を作る *)
let picture_to_image (picture : picture_t) : Image.t =
  let images =
    List.map
      (fun (_, _, color) -> rectangle flength flength ~outline_size:0. color)
      picture.cells in
  let poss =
    List.map
      (fun (x, y, _) -> (float_of_int ((x - 1) * length)),
                        float_of_int ((y - 1) * length))
      picture.cells in
  place_images images poss empty_picture

(* 白い背景画像 *)
let background = empty_scene fwidth fheight

(* 左の部分の背景画像 *)
let barbg : Image.t = rectangle fbar_width fheight ~outline_size:0. gray80

(* 描画関数 *)
let draw world =
  place_image
    barbg
    (0., 0.)
    (place_image
    (picture_to_image world.editing)
    canvas_pos
    background)

(* ゲーム開始 *)
let _ =
  big_bang
    initial_world
    ~name:"dots"
    ~width:width
    ~height:height
    ~to_draw:draw
    (* ~on_mouse:on_mouse
     * ~on_tick:on_tick
     * ~stop_when:stop_when
     * ~to_draw_last:to_draw_last *)
    ~rate:rate			(* ゲームの動く速さ *)
    
