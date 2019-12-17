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
let adjacents (cx, cy) =
  List.map (fun (dx, dy) -> (cx + dx, cy + dy)) [(0, 1); (0, -1); (-1, 0); (1, 0)]

let p1 m =
  let rec handle g x y = function
    | Done -> g
    | Put (c, k) ->
      (match Char.chr c with
       | '\n' -> handle g 0 (y + 1) @@ k ()
       | '#' -> handle (Grid.add (x, y) g) (x + 1) y @@ k ()
       | '.' -> handle g (x + 1) y @@ k ()
       | c -> handle (Grid.add (x, y) g) (x + 1) y @@ k ())
    | _ -> failwith "bad state1"
  in handle Grid.empty 0 0 (run m)

let main =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = ref IntMap.empty in
  List.iteri (fun i x -> mem := IntMap.add i x !mem) p;
  let g = p1 !mem in
  Printf.printf "%d\n" @@
  List.fold_left (+) 0 @@
  List.map (fun (x, y) -> x * y) @@
  List.filter (fun pos ->
      let adj = List.filter (fun a -> Grid.mem a g) (adjacents pos) in
      List.length adj = 4) @@
  Grid.elements g
