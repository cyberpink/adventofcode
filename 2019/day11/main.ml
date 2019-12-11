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
let run_program p h =
  let count = ref 0 in
  let dim = ref (0,0,0,0) in
  let rec give_color h c d = function
    | Get k ->
      let color = match Grid.find_opt c h with Some x -> x | None -> 0 in
      get_color h c d @@ k color
    | _ -> failwith "bad state1"
  and get_color h c d = function
    | Put (x, k) ->
      if not (Grid.mem c h) then count := succ !count;
      let (l,r,b, t) = !dim in
      let (cx, cy) = c in
      dim := (min cx l, max cx r, min cy b, max cy t);
      get_dir (Grid.add c x h) c d @@ k ()
    | _ -> failwith "bad state2"
  and get_dir h c d = function
    | Put (x, k) ->
      let d' = turn d x in
      next h (move c d') d' @@ k ()
    | _ -> failwith "bad state3"
  and next h c d = function 
    | Done -> (!count, h, !dim)
    | r -> give_color h c d r
  in give_color h (0,0) (1,0) (run p)

let rec init =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = Array.make 1500 0 in
  List.iteri (Array.set mem) p;
  let (p1, _, _) = run_program (Array.copy mem) Grid.empty in
  let (p2c, p2, (l,r,b,t)) = run_program (Array.copy mem) (Grid.singleton (0,0) 1) in
  Printf.printf "count: %d\n" p1;
  for y = b to t do
    for x = r downto l do
      let d = match Grid.find_opt (x, y) p2 with Some x -> x | _ -> 0 in
      print_string @@ if d =1 then "X" else " "
    done;
    print_newline ()
  done;
