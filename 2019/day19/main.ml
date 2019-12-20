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

let go m x y =
  let rec handle x y = function
    | Get k ->
      begin match k x with
        | Get k -> handle x y (k y)
        | _ -> failwith "bad state"
      end
    | Put (i, k) -> i
    | _ -> failwith "bad state2"
  in handle x y @@ (run m)

let p1 m =
  let total = ref 0 in
  for y = 0 to 49 do
    for x = 0 to 49 do
      total := !total + go m x y
    done;
  done;
  !total

let p2 m =
  let rec loop x y =
    let tl = go m x y in
    let tr = go m (x+99) y in
    let bl = if y < 100 then 0 else go m x (y-99) in
    let br = if y < 100 then 0 else  go m (x+99) (y-99) in
    match (tl, tr, bl, br) with
    | (1,1,1,1) -> (x*10000+(y-99))
    | _ ->
      (match (go m (x + 1) y, go m x (y + 1)) with
       | (_,1) -> loop x (y+1)
       | (1,0) -> loop (x+1) y
       | _ -> failwith "bad")
  in loop 31 48

let main =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = ref IntMap.empty in
  List.iteri (fun i x -> mem := IntMap.add i x !mem) p;
  Printf.printf "%d\n" @@  p1 !mem;
  Printf.printf "%d\n" @@ p2 !mem

