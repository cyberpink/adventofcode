type req =
  | Get of (int -> req)
  | Put of int * (unit -> req)
  | Done

let do_get k = Get k
let do_put x k = Put (x, k)
let do_done = Done

let run mem =
  let rec go pc rb =
    let _op = mem.(pc) in
    let opcode = _op mod 100 in
    let modes = [| _op / 100 mod 10; _op / 1000 mod 10; _op / 10000 mod 10 |] in
    let o i = match modes.(i-1) with
      | 0 -> mem.(pc + i)
      | 1 -> pc + i
      | 2 -> rb + mem.(pc + i)
      | _ -> failwith "bad parameter mode"
    in
    let set i v = mem.(o i) <- v in
    let get i = mem.(o i) in
    match opcode with
    | 1 -> set 3 (get 1 + get 2); go (pc + 4) rb
    | 2 -> set 3 (get 1 * get 2); go (pc + 4) rb
    | 3 -> do_get (fun v -> set 1 v; go (pc + 2) rb)
    | 4 -> do_put (get 1) (fun () -> go (pc + 2) rb)
    | 5 -> if get 1 != 0 then go (get 2) rb else go (pc + 3) rb
    | 6 -> if get 1 == 0 then go (get 2) rb else go (pc + 3) rb
    | 7 -> set 3 (if get 1 < get 2 then 1 else 0); go (pc + 4) rb
    | 8 -> set 3 (if get 1 == get 2 then 1 else 0); go (pc + 4) rb
    | 9 -> go (pc+2) (rb + get 1)
    | 99 -> do_done
    | _ -> failwith "bad op"
  in go 0 0

let move (cx, cy) (dx, dy) = (cx + dx, cy + dy)
let turn (x, y) = function
  | 0 -> (-y, x)
  | 1 -> (y, -x)
  | _ -> failwith "bad direction"

module Grid = Map.Make(struct type t = (int * int) let compare = compare end)

let rec take n p o =
  if n = 0 then
    (List.rev o, p)
  else
    match p with
    | Put (v, k) ->  take (n - 1) (k ()) (v :: o)
    | _ -> failwith "bad state1"

let p1 p h =
  let rec get h p =
    match take 3 p [] with
    | ([x;y;t], p') -> done_or_next (Grid.add (x,y) t h) p'
    | _ -> failwith "bad state2"
  and done_or_next h= function 
    | Done -> h
    | r -> get h r
  in get h (run p)

let p2 mem h =
  mem.(0) <- 2;
  let rec go h b p s = function
    | Done -> s
    | Get k -> go h b p s @@ k (compare b p)
    | Put (x, k) ->
      begin match k () with
        | Put (y, k) ->
          begin match k () with
            | Put (t, k) ->
              if x = -1 then
                go h b p t @@ k ()
              else
                let b' = if t = 4 then x else b in
                let p' = if t = 3 then x else p in
                go (Grid.add (x,y) t h) b' p' s @@ k ()
            | _ -> failwith "bad state"
          end
        | _ -> failwith "bad state"
      end
  in go h 0 0 0 (run mem)

let main =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = Array.make 20000 0 in
  List.iteri (Array.set mem) p;
  let p1h = p1 (Array.copy mem) Grid.empty in
  Printf.printf "%d\n" @@ Grid.fold (fun _ x m -> if x = 2 then succ m else m) p1h 0;
  Printf.printf "%d\n" @@ p2 (Array.copy mem) Grid.empty
