open UniverseJs
open World
open Image
open Color

(* １秒あたり10周する *)
let time_per_second : int = 10

(* 制限時間120秒 *)
let time_limit : int = 1200

(* １周で動く幅(px)（使ってない） *)
let move_speed : float = 16.

(* false にすると毎度同じ順番で銭が出てくる *)
let truly_random : bool = true

(* 乱数の初期化 *)
;;if truly_random then Random.self_init () else Random.init 0

(* -------------------------------- 定数 -------------------------------- *)

(* １周の時間 *)
let rate : float = (1. /. (float_of_int time_per_second))

(* 広さ *)
let lines : int = 8   (* 上下の列数 *)
let rows : int = 12   (* 左右の列数 *)
let top : int = 1     (* 上に何マス分の余白があるか *)
let bottom : int = 0  (* 下に何マス文の余白があるか *)
let length = 64       (* １マスの辺の長さ *)

(* ゲーム画面の広さ *)
let width : int = rows * length + 1
let height : int = (lines + top + bottom) * length + 1


(* -------------------------------- 型定義 -------------------------------- *)

(* 小銭を表す型 *)
type yen_t = Y1 | Y5 | Y10 | Y50 | Y100 | Y500 | No

(* １マスを表す型 *)
type cell_t = {
  pos : (int * int);                (* 位置 *)
  yen : yen_t;                      (* 銭の金額 *)
  moving : (float * float) option;  (* マスの中心からの小銭のずれ（使ってない） *)
}

(* ゲームの状態を表す型 *)
type world_t = {
  cells : cell_t list;          (* マスのリスト、順番はどんどん変わる *)
  score : int;                  (* 点数(円) *)
  time : int;                   (* １周で１ずつインクリメントする *)
  message : string;             (* 上に出す文字列、主にデバッグ用 *)
  select : (int * int) option;  (* マウスボタンを押した時のカーソルの位置 *)
}

(* 画像 *)
let background_image = read_image "images/background.png"  (* 自作 *)
let background_end_image = read_image "images/background_end.png"
let yen1_image = read_image "images/yen1.png"  (* いらすとやのいらすとを縮小 *)
let yen5_image = read_image "images/yen5.png"
let yen10_image = read_image "images/yen10.png"
let yen50_image = read_image "images/yen50.png"
let yen100_image = read_image "images/yen100.png"
let yen500_image = read_image "images/yen500.png"

(* -------------------------------- 初期値 -------------------------------- *)

(* 最初のマスを全部作る *)
let rec make_initial_cells (x : int) (y : int) : cell_t list =
  if rows < x then []
  else if lines < y then make_initial_cells (x + 1) 1
  else {
    pos = (x, y);
    yen = No;
    moving = None;
  } :: make_initial_cells x (y + 1)

(* 世界の初期値 *)
let initial_world = {
  cells = make_initial_cells 1 1;
  score = 0;
  time = 0;
  message = "";
  select = None;
}

(* -------------------------------- 銭処理 -------------------------------- *)

(* 整数を受け取って適当に小銭のどれかを返す *)
let int_to_yen (i : int) : yen_t =
  let mod6 = i mod 6 in
  if mod6 = 0 then Y1
  else if mod6 = 1 then Y5
  else if mod6 = 2 then Y10
  else if mod6 = 3 then Y50
  else if mod6 = 4 then Y100
  else Y500

(* ランダムに新しい小銭を返す *)
let new_yen () : yen_t = int_to_yen (Random.int 6)

(* 小銭を受け取ってその金額(円)を返す *)
let yen_to_int : yen_t -> int = function
  | Y1 -> 1
  | Y5 -> 5
  | Y10 -> 10
  | Y50 -> 50
  | Y100 -> 100
  | Y500 -> 500
  | _ -> 0

(* （あるマスに）小銭があるかどうかを返す *)
let yen_exist : yen_t -> bool = function
  | No -> false
  | a -> true

(* 次に小さい金額の小銭を返す *)
let next_yen : yen_t -> yen_t = function
  | Y1 -> Y5
  | Y5 -> Y10
  | Y10 -> Y50
  | Y50 -> Y100
  | Y100 -> Y500
  | Y500 -> No
  | No -> No

(* 次に小さい金額の小銭と同じ金額になるのに最低限必要な枚数を返す *)
let yen_need : yen_t -> int = function
  | Y1 | Y10 | Y100 -> 5
  | Y5 | Y50 | Y500 -> 2
  | No -> max_int

(* 小銭の種類と枚数を受け取って、その金額以下の最大の額の小銭を返す *)
let rec yen_upgrade (yen : yen_t) (num : int) : yen_t =
  let need = yen_need yen in
  if need <= num
  then yen_upgrade (next_yen yen) (num / need)
  else yen

(* 小銭を受け取ってその画像を返す *)
let yen_to_image : yen_t -> Image.t = function
  | Y1 -> yen1_image
  | Y5 -> yen5_image
  | Y10 -> yen10_image
  | Y50 -> yen50_image
  | Y100 -> yen100_image
  | Y500 -> yen500_image
  | _ -> empty_scene 40. 40.  (* 多分ここに来ない *)

(* -------------------------------- 描画 -------------------------------- *)

(* 整数を受け取って、３桁になるように頭に"0"を付けた文字列を返す *)
let sanketa (i : int) : string =
  let str = string_of_int i in
  if i < 10 then "00" ^ str
  else if i < 100 then "0" ^ str
  else str

(* 整数を受け取って、桁数によって適当に最初に半角スペースを付けて横に広くした文字列を返す *)
let rokketa (i : int) : string =
  let str = string_of_int i in
  if i < 10 then "          " ^ str
  else if i < 10000 then "    " ^ str
  else if i < 100000 then "  " ^ str
  else str

(* rate 単位の経過時間を受け取って、残り秒数を返す *)
let time_to_second (time : int) : int =
  (time_limit - time) / time_per_second

(* 状態を受け取ってゲーム画像を返す *)
let draw_with_bg ({cells; score; time; message} : world_t) (bg : Image.t)
  : Image.t =
  (* 背景の上に小銭画像の羅列 *)
  let field : Image.t = 
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
    place_images images poss bg in
  (* 小銭羅列画像に残り時間と点数の文字列を足したもの *)
  let with_texts =
    let time_str = sanketa (time_to_second time) in
    let score_str = rokketa score in
    let time_text = text time_str ~size:30 blue in
    let score_text = text score_str ~size:30 blue in
    let yen_text = text "円" blue in
    place_images
      [time_text; score_text; yen_text]
      [(30., 15.);
       (float_of_int (width - 169), 15.);
       (float_of_int (width - 44), 23.)]
      field in
  (* 言うことがあれば画像に足す *)
  match message with
  | "" -> with_texts
  | _ ->
    place_image
      (text message blue)
      (float_of_int ((width / 2) - 40), 20.)
      with_texts

let draw (world : world_t) : Image.t =
  draw_with_bg world background_image

(* -------------------------------- 座標計算 -------------------------------- *)

(* ゲーム画面上の座標を受け取って、その座標にあるマスの位置を返す *)
let zahyou_to_pos_op (x : float) (y : float) : (int * int) option =
  let x = int_of_float x in
  let y = int_of_float y in
  if y < length + 1 || width - 1 <= x || height - 1 <= y
  then None
  else Some ((x + 1) / 64 + 1, ((y + 1) / 64))

(* デバッグ用：マスの位置を受け取ってそれを表す文字列返す *)
let pos_to_string ((x, y) : int * int) : string
  = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(* マスの位置を２つ受け取って、隣のマスかどうかを返す *)
let next : int * int -> int * int -> bool = fun (xa, ya) (xb, yb) ->
  if xa = xb then (abs (ya - yb)) = 1
  else if ya = yb then (abs (xa - xb)) = 1
  else false

(* -------------------------------- ドラッグ -------------------------------- *)

(* 世界とマスの位置２つを受け取って、２マスを入れ替えた世界を返す *)
let swap (world : world_t) ((xa, ya) : int * int) ((xb, yb) : int * int)
  : world_t =
  let (ab, rest) : cell_t list * cell_t list =
    List.partition
      (fun {pos = (x, y)} -> x = xa && y = ya || x = xb && y = yb)
      world.cells in
  let new_cells : cell_t list =
    match ab with
    | [{yen = c_yen} as c; {yen = d_yen} as d] ->
      {c with yen = d_yen} :: {d with yen = c_yen} :: rest
    | _ -> world.cells in
  {world with cells = new_cells}

(* -------------------------------- クリック -------------------------------- *)

(* クリックされたマスとそれ以外のマスらを受け取って、消えるマスとそれ以外のマスの組を返す *)
let group (cell : cell_t) (cells : cell_t list) : cell_t list * cell_t list =
  let rec group : cell_t list -> cell_t list -> cell_t list -> cell_t list
    -> cell_t list * cell_t list =
    fun inner border outer others ->
      match border with
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

(* クリック時の処理をした世界を返す *)
let click (world : world_t) ((x, y) : int * int) : world_t =

  (* clicked：クリックしたマスを含むリスト  other_cells：それ以外のマスのリスト *)
  let (clicked, other_cells) : cell_t list * cell_t list =
    List.partition
      (fun {pos} -> pos = (x, y))
      world.cells in

  (* clicked：クリックしたマス *)
  let clicked : cell_t = List.hd clicked in

  (* inner：消えるマス  outer：消えないマス *)
  let (inner, outer) : cell_t list * cell_t list =
    group clicked other_cells in

  (* other_inner：消えるマスのうちクリックしたマス以外 *)
  let (_, other_inner) : cell_t list * cell_t list =
    List.partition
      (fun {pos} -> pos = (x, y))
      inner in

  (* new_inner：消えたマス  add_score：今回のクリックで得られる特典(円) *)
  let (new_inner, add_score) : cell_t list * int =
    (* num：消えるマスの数 *)
    let num : int = List.length other_inner + 1 in
    if num < yen_need clicked.yen
    then (inner, 0)
    else
      (* new_yen：新しい小銭 *)
      let new_yen : yen_t = yen_upgrade clicked.yen num in
      ({clicked with yen = new_yen}
       :: List.map (fun cell -> {cell with yen = No}) other_inner,
       (((yen_to_int clicked.yen) * num) / 1000) * 1000) in
  {world with cells = new_inner @ outer;
              score = world.score + add_score}
  
(* -------------------------------- マウス処理 -------------------------------- *)

(* 世界と、画面からはみ出ないまま離した時のマウス位置を受け取って、処理して新しい世界を返す *)
let on_valid_click (world : world_t) ((x, y) : int * int) : world_t =
  match world.select with
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

(* マウスが何かしたときの処理をする *)
let on_mouse (world : world_t) (x : float) (y : float) (mouse : string)
  : world_t =
  match mouse with
  | "button_down" ->  (* マウス左ボタンが押された時、世界にその位置を記録 *)
    begin
      let pos : (int * int) option = zahyou_to_pos_op x y in
      {world with select = pos}
    end
  | "button_up" ->  (* マウス左ボタンが離された時、必要に応じて処理 *)
    begin
      let pos = zahyou_to_pos_op x y in
      match pos with
      | None -> {world with select = None}
      | Some pos -> 
        let new_world = on_valid_click world pos in
        new_world
    end
  | _ -> world  (* それ以外のマウス操作の時は何もしない *)

(* -------------------------------- 毎時処理 -------------------------------- *)

(* 経過時間をインクリメントする世界変換 *)
let inc_time (world : world_t) : world_t =
  {world with time = world.time + 1}

(* マスを位置が低い順にソートするための比較関数 *)
let compare_y : cell_t -> cell_t -> int =
  fun {pos = (xa, ya)} {pos = (xb, yb)} ->
  if ya < yb
  then 1
  else if ya = yb then 0
  else -1

(* 座標を受け取ってちょっと動かした座標を返す（使ってない） *)
let move (f : float) : float =
  f +. move_speed

(* 小銭が無いマスのリストと全マスのリストを受け取って、１マスずつ落とした全マスを返す *)
let fall (no_cells : cell_t list) (all_cells : cell_t list) : cell_t list =
  let rec fall no_cells all_cells = match no_cells with
    | [] -> all_cells  (* 小銭が無いマスを処理しきったら終了 *)
    | {pos = (xf, yf)} as first :: rest ->
      if yf = 1
      then  (* 一番上の列だったらランダムな小銭を生成 *)
        let new_first =
          {first with
           yen = new_yen ();
           moving = None} in
        let others =
          List.filter
            (fun {pos = (x, y)} -> not (x = xf && y = yf))
            all_cells in
        fall rest (new_first :: others)
      else  (* 一番上の列以外だったら上のマスから小銭を奪う *)
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
  (* sorted_no_cells：位置が低い順に並べられた小銭が無いマス *)
  let sorted_no_cells : cell_t list = List.sort compare_y no_cells in
  fall sorted_no_cells all_cells

(* 時間経過によって更新された世界を返す（具体的には小銭を落とす） *)
let move_on_tick (world : world_t) : world_t =
  let no_cells = List.filter (fun {yen} -> yen = No) world.cells in
  let new_all_cells = fall no_cells world.cells in
  {world with cells = new_all_cells}

(* 毎瞬間呼ばれて、世界を更新して返す *)
let on_tick (world : world_t) : world_t =
  let world = inc_time world in  (* 時間を増やすだけ *)
  let moved_world = move_on_tick world in
  moved_world

(* -------------------------------- 開始・終了 -------------------------------- *)

(* 世界を受け取って、ゲームを終わるかどうかを返す *)
let stop_when ({time} : world_t) : bool = time_limit <= time

(* 世界を受け取って、ゲーム終了後のゲーム画面を返す *)
let to_draw_last (world : world_t) : Image.t =
  let last_world = {world with message = "TIME UP"} in
  draw_with_bg last_world background_end_image

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
    
