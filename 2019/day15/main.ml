type req =
  | Get of (int -> req)
  | Put of int * (unit -> req)
  | Done

let do_get k = Get k
let do_put x k = Put (x, k)
let do_done = Done

module IntMap = Map.Make(struct type t = int let compare = compare end)
let run mem =
  let rec go mem pc rb =
    let _op = IntMap.find pc mem in
    let opcode = _op mod 100 in
    let modes = [| _op / 100 mod 10; _op / 1000 mod 10; _op / 10000 mod 10 |] in
    let o i = match modes.(i-1) with
      | 0 -> IntMap.find (pc + i) mem
      | 1 -> pc + i
      | 2 -> rb + IntMap.find (pc + i) mem
      | _ -> failwith "bad parameter mode"
    in
    let set i v = IntMap.add (o i) v mem in
    let get i = IntMap.find (o i) mem in
    match opcode with
    | 1 -> let mem = set 3 (get 1 + get 2) in go mem (pc + 4) rb
    | 2 -> let mem = set 3 (get 1 * get 2) in go mem (pc + 4) rb
    | 3 -> do_get (fun v -> let mem = set 1 v in go mem (pc + 2) rb)
    | 4 -> do_put (get 1) (fun () -> go mem (pc + 2) rb)
    | 5 -> if get 1 != 0 then go mem (get 2) rb else go mem (pc + 3) rb
    | 6 -> if get 1 == 0 then go mem (get 2) rb else go mem (pc + 3) rb
    | 7 -> let mem = set 3 (if get 1 < get 2 then 1 else 0) in go mem (pc + 4) rb
    | 8 -> let mem = set 3 (if get 1 == get 2 then 1 else 0) in go mem (pc + 4) rb
    | 9 -> go mem (pc+2) (rb + get 1)
    | 99 -> do_done
    | _ -> failwith "bad op"
  in go mem 0 0

module Grid = Set.Make(struct type t = (int * int) let compare = compare end)
let dirs = [1;2;3;4]
let get_dir i = [|(0, 1); (0, -1); (-1, 0); (1, 0)|].(i-1)
let move (cx, cy) d = let (dx, dy) = get_dir d in (cx + dx, cy + dy)

let p1 m =
  let best = ref 9999 in
  let oxy = ref (0,0) in
  let map = ref Grid.empty in
  let rec handle_try p n g = function
    | Get k ->
      for d = 1 to 4 do
        let p' = move p d in
        if not (Grid.mem p' g)
        then handle_check p' d n g (k d)
      done
    | _ -> failwith "bad state1"
  and  handle_check p d n g = function
    | Put (0, k) -> ()
    | Put (1, k) -> map := Grid.add p !map;
      handle_try p (n + 1) (Grid.add p g) (k ())
    | Put (2, k) -> best := min !best n; oxy := p
    | _ ->  failwith "bad state2"
  in handle_try (1,1) 1 Grid.empty (run m);
  (!best, !oxy, !map)

let adjacents p = Grid.of_list @@ List.map (move p) dirs
let rec p2 o h i =
  if Grid.cardinal h = 0 then
    i
  else
    let adj = Grid.fold (fun x m -> Grid.union m (adjacents x)) o Grid.empty in
    let o' = (Grid.inter h adj) in
    p2 o' (Grid.diff h o') (i + 1)

let main =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = ref IntMap.empty in
  List.iteri (fun i x -> mem := IntMap.add i x !mem) p;
  let (p1a, o, h) = p1 !mem  in
  Printf.printf "%d\n" p1a;
  Printf.printf "%d\n" @@ p2 (Grid.singleton o) h 0
