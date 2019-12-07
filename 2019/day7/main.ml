(* ocamlfind ocamlopt -linkpkg -package bigarray main.ml *)
type req =
  | Get of (int -> req)
  | Put of int * (unit -> req)
  | Done

let mset m i v = Bigarray.Array1.set m i @@ Int32.of_int v
let mget m i = Int32.to_int @@ Bigarray.Array1.get m i
let mcopy m = 
  let m' = Bigarray.Array1.create Int32 C_layout (Bigarray.Array1.dim m) in
  Bigarray.Array1.blit m m';
  m'

let run mem =
  let rec go pc =
    let _op = mget mem pc in
    let opcode = _op mod 100 in
    let modes =
      [| (_op / 100) mod 10;
         (_op / 1000) mod 10;
         (_op / 10000) mod 10 |]
    in
    let set r v = mset mem (mget mem r) v in
    let p i = match modes.(i-1) with
      | 0 -> mget mem (mget mem (pc+i))
      | 1 -> mget mem (pc+i)
      | _ -> failwith "bad mode"
    in
    match opcode with
    | 1 -> set (pc+3) (p 1 + p 2); go (pc + 4)
    | 2 -> set (pc+3) (p 1 * p 2); go (pc + 4)
    | 3 -> Get (fun v -> set (pc+1) v; go (pc + 2))
    | 4 -> Put (p 1, fun () -> go (pc + 2))
    | 5 -> if p 1 != 0 then go (p 2) else go (pc + 3)
    | 6 -> if p 1 == 0 then go (p 2) else go (pc + 3)
    | 7 -> set (pc+3) (if p 1 < p 2 then 1 else 0); go (pc + 4)
    | 8 -> set (pc+3) (if p 1 == p 2 then 1 else 0); go (pc + 4)
    | 99 -> Done
    | _ -> failwith "bad op"
  in go 0

let run_threads ts =
  let last = Array.length ts - 1 in
  let rec run pid c =
    match ts.(pid) with
    | Done -> if pid = last then c else run (pid + 1) c
    | Get k -> ts.(pid) <- k c; run pid c
    | Put (x, k) -> ts.(pid) <- k (); run ((pid + 1) mod 5) x
  in run 0 0

let make_program mem inits =
  let make_thread x = match run (mcopy mem) with
    | Get k -> k x
    | _ -> failwith "bad init state"
  in Array.map make_thread inits

let swap a x y = let tmp = a.(x) in a.(x) <- a.(y); a.(y) <- tmp
let rec permutations yield xs ptr = function
  | i when i >= 0 ->
    swap xs ptr i;
    (match ptr - 1 with
     | 0 -> yield @@ Array.copy xs
     | ptr' -> permutations yield xs ptr' ptr');
    swap xs ptr i;
    permutations yield xs ptr (i - 1)
  | _ -> ()

let run_permutations inputs mem =
  let ps = ref [] in
  permutations (fun x -> ps := x :: !ps) inputs (Array.length inputs - 1) (Array.length inputs - 1);
  List.fold_left max 0 @@ List.map run_threads @@ List.map (make_program mem) !ps

let rec init =
  let p = List.map int_of_string @@ String.split_on_char ',' @@ read_line () in
  let mem = Bigarray.Array1.create Int32 C_layout (List.length p) in
  List.iteri (mset mem) p;
  Printf.printf "%d\n%d\n"
    (run_permutations [| 0; 1; 2; 3; 4 |] mem)
    (run_permutations [| 5; 6; 7; 8; 9 |] mem)
