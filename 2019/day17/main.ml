type req =
  | Get of (int -> req)
  | Put of int * (unit -> req)
  | Done

let do_get k = Get k
let do_put x k = Put (x, k)
let do_done = Done

module IntMap = Map.Make(struct type t = int let compare = compare end)
let mget i m = match IntMap.find_opt i m with Some x -> x | None -> 0
let mset i v m = IntMap.add i v m

let run mem =
  let rec go mem pc rb =
    let _op = IntMap.find pc mem in
    let opcode = _op mod 100 in
    let modes = [| _op / 100 mod 10; _op / 1000 mod 10; _op / 10000 mod 10 |] in
    let o i = match modes.(i-1) with
      | 0 -> mget (pc + i) mem
      | 1 -> pc + i
      | 2 -> rb + mget (pc + i) mem
      | _ -> failwith "bad parameter mode"
    in
    let set i v = mset (o i) v mem in
    let get i = mget (o i) mem in
    match opcode with
    | 1 -> let mem = set 3 (get 1 + get 2) in go mem (pc + 4) rb
    | 2 -> let mem = set 3 (get 1 * get 2) in go mem (pc + 4) rb
    | 3 -> do_get (fun v -> let mem = set 1 v in go mem (pc + 2) rb)
    | 4 -> do_put (get 1) (fun () -> go mem (pc + 2) rb)
    | 5 -> if get 1 <> 0 then go mem (get 2) rb else go mem (pc + 3) rb
    | 6 -> if get 1 = 0 then go mem (get 2) rb else go mem (pc + 3) rb
    | 7 -> let mem = set 3 (if get 1 < get 2 then 1 else 0) in go mem (pc + 4) rb
    | 8 -> let mem = set 3 (if get 1 = get 2 then 1 else 0) in go mem (pc + 4) rb
    | 9 -> go mem (pc+2) (rb + get 1)
    | 99 -> do_done
    | _ -> failwith "bad op"
  in go mem 0 0

module Grid = Set.Make(struct type t = (int * int) let compare = compare end)
let adjacents (cx, cy) =
  List.map (fun (dx, dy) -> (cx + dx, cy + dy)) [(0, 1); (0, -1); (-1, 0); (1, 0)]

let p1 m =
  let rec handle g x y s d = function
    | Done -> (d, s, g)
    | Put (c, k) ->
      (match Char.chr c with
       | '\n' -> handle g 0 (y + 1) s d @@ k ()
       | '#' -> handle (Grid.add (x, y) g) (x + 1) y s d @@ k ()
       | '.' -> handle g (x + 1) y s d @@ k ()
       | c ->
         let dir = match c with
           | 'v' -> (0, 1)
           | '^' -> (0, -1)
           | '>' -> (1, 0)
           | '<' -> (-1, 0)
           | c -> failwith (Printf.sprintf "bad char '%c'" c)
         in handle (Grid.add (x, y) g) (x + 1) y (x, y) dir @@ k ())
    | _ -> failwith "bad state1"
  in handle Grid.empty 0 0 (0,0) (0,0) (run m)

let move (cx, cy) (dx, dy) = (cx + dx, cy + dy)
type ddir = DF | DL | DR
let move2 (dx, dy) = function
  | DF -> (dx, dy)
  | DL -> (dy, -dx)
  | DR -> (-dy, dx)

let print_grid g t x y =
  for y = 0 to y do
    for x = 0 to x do
      if Grid.mem (x, y) t then
        print_char '*'
      else if Grid.mem (x, y) g then
        print_char '#'
      else
        print_char '.'
    done;
    print_newline ()
  done;
  print_newline ()
  
let p2 m g i s d =
  let results = ref [] in
  let goal = Grid.cardinal g in
  let rec go t p m l d =
    let moves =
      List.filter (fun (p', _) -> not (p = p' || p' = l) && Grid.mem p' g && (Grid.mem p' i || not (Grid.mem p' t))) @@
      List.map (fun dn -> (move p (move2 d dn), dn)) @@
      [DF;DL;DR]
    in
    match moves with
    | [] -> if (Grid.cardinal t) = goal then results := m :: !results
    | moves ->
      List.iter (fun (p', dn') -> go (Grid.add p' t) p' (dn' :: m) p (move2 d dn')) moves
  in
  go (Grid.singleton s) s [] s d;
  !results

let p22 m =
  (* split by hand *)
  let program = List.of_seq @@ String.to_seq @@ String.concat ""
      ["A,B,A,B,C,B,C,A,C,C\n";
       "R,12,L,10,L,10\n";
       "L,6,L,12,R,12,L,4\n";
       "L,12,R,12,L,6\n";
       "n\n"
      ]
  in
  let rec handle p o = function
    | Done -> o
    | Get k ->
      (match p with
       | [] -> failwith "not enough input"
       | f :: r ->  handle r o @@ k (Char.code f))
    | Put (x, k) -> handle p x @@ k ()
  in handle program 0 (run (IntMap.add 0 2 m))

type dir = L | R | F of int
let rec process2 = function
  | [] -> []
  | F a :: F b :: r -> process2 @@ F (a + b) :: r
  | f :: r -> f :: process2 r
let rec process m = function
  | [] -> process2 @@ List.rev m
  | DF :: r -> process (F 1 :: m) r
  | DL :: r -> process (F 1 :: L :: m) r
  | DR :: r -> process (F 1 :: R :: m) r

let str_of_dir = function
  | L -> "L"
  | R -> "R"
  | F i -> string_of_int i

let main =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = ref IntMap.empty in
  List.iteri (fun i x -> mem := IntMap.add i x !mem) p;
  let (d, s, g) = p1 !mem in
  let intersections = Grid.filter (fun pos ->
      let adj = List.filter (fun a -> Grid.mem a g) (adjacents pos) in
      List.length adj = 4) g
  in
  (* let (x, y) = s in *)
  (* Printf.printf "starting point: %d,%d\n" x y; *)
  (* print_grid g Grid.empty 40 60;  *)
  Printf.printf "%d\n" @@
  List.fold_left (+) 0 @@
  List.map (fun (x, y) -> x * y) @@
  Grid.elements intersections;
  (* let r = p2 !mem g intersections s d in
   * print_endline @@ String.concat "," @@ List.map str_of_dir @@ process [] @@  List.rev @@ List.hd @@ List.rev r; *)
  Printf.printf "%d\n" @@ p22 !mem
    
