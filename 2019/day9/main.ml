type req =
  | Get of (int -> req)
  | Put of int * (unit -> req)
  | Done

let mset m i v = m.(i) <- v
let mget m i = m.(i)
let mcopy m = Array.copy m

let run mem =
  let rec go pc rb =
    let _op = mget mem pc in
    let opcode = _op mod 100 in
    let modes =
      [| (_op / 100) mod 10;
         (_op / 1000) mod 10;
         (_op / 10000) mod 10 |]
    in
    let set i v = match modes.(i-1) with
      | 0 -> mset mem (mget mem (pc+i)) v
      | 2 -> mset mem (rb + (mget mem (pc+i))) v
      | _ -> failwith "bad write parameter mode"
    in
    let get i = match modes.(i-1) with
      | 0 -> mget mem (mget mem (pc+i))
      | 1 -> mget mem (pc+i)
      | 2 -> mget mem (rb + (mget mem (pc+i)))
      | _ -> failwith "bad read parameter mode"
    in
    match opcode with
    | 1 -> set 3 (get 1 + get 2); go (pc + 4) rb
    | 2 -> set 3 (get 1 * get 2); go (pc + 4) rb
    | 3 -> Get (fun v -> set 1 v; go (pc + 2) rb)
    | 4 -> Put (get 1, fun () -> go (pc + 2) rb)
    | 5 -> if get 1 != 0 then go (get 2) rb else go (pc + 3) rb
    | 6 -> if get 1 == 0 then go (get 2) rb else go (pc + 3) rb
    | 7 -> set 3 (if get 1 < get 2 then 1 else 0); go (pc + 4) rb
    | 8 -> set 3 (if get 1 == get 2 then 1 else 0); go (pc + 4) rb
    | 9 -> go (pc+2) (rb + get 1)
    | 99 -> Done
    | _ -> failwith "bad op"
  in go 0 0

let run_program p input =
  let c = Queue.create () in
  let rec handle = function
    | Done -> Queue.iter (Printf.printf "%d,") c; print_newline ()
    | Get k -> handle @@ k input
    | Put (x, k) ->
      Queue.add x c;
      handle @@ k ()
  in handle @@ run p

let rec init =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = Array.make 1500 0 in
  List.iteri (mset mem) p;
  run_program (mcopy mem) 1;
  run_program (mcopy mem) 2
