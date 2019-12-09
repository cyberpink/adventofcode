type req =
  | Get of (int -> req)
  | Put of int * (unit -> req)
  | Done

let run mem =
  let rec go pc rb =
    let _op = mem.(pc) in
    let opcode = _op mod 100 in
    let modes =
      [| (_op / 100) mod 10;
         (_op / 1000) mod 10;
         (_op / 10000) mod 10 |]
    in
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
  let rec handle o = function
    | Done -> print_endline @@ String.concat "," @@ List.map string_of_int @@ List.rev o
    | Get k -> handle o @@ k input
    | Put (x, k) -> handle (x :: o) @@ k ()
  in handle [] @@ run p

let rec init =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = Array.make 1500 0 in
  List.iteri (Array.set mem) p;
  run_program (Array.copy mem) 1;
  run_program (Array.copy mem) 2
