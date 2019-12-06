
let store = Hashtbl.create 100
let store_c = ref 0
let hash_cons x =
  try Hashtbl.find store x with
  | Not_found ->
    let c' = !store_c in
    Hashtbl.add store x c';
    store_c := succ c';
    c'

let rec build_graph tbl =
  try
    Scanf.scanf "%3s)%3s\n" (fun a b ->
        let a = hash_cons a in
        let b = hash_cons b in
        Hashtbl.add tbl b a;
        build_graph tbl)
  with
  | End_of_file -> ()

let indirect tbl =
  let memo = Array.make !store_c 0 in
  let rec get_count x =
    match memo.(x) with
    | 0 ->
      let children = Hashtbl.find_all tbl x in
      let value = List.fold_left (+) 0 (List.map (fun c -> 1 + get_count c) children) in
      memo.(x) <- value;
      value
    | value -> value
  in Hashtbl.fold (fun k _ m -> m + get_count k) tbl 0

let rec path v p tbl =
  try
    path (Hashtbl.find tbl v) (v :: p) tbl
  with
    Not_found -> p

let rec drop_common a b =
  match (a, b) with
  | ah :: at, bh :: bt ->
    if ah = bh
    then drop_common at bt
    else (a, b)
  | _, _ -> failwith "paths are same"

let () =
  let table = Hashtbl.create 100 in
  build_graph table;
  let you_path = path (hash_cons "YOU") [] table in
  let san_path = path (hash_cons "SAN") [] table in
  let (p1, p2) = drop_common you_path san_path in
  let steps = (List.length p1 + List.length p2) - 2 in
  Printf.printf "%d\n%d\n" (indirect table) steps
