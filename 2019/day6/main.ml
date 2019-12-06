let rec build_graph tbl =
  try
    Scanf.scanf "%3s)%3s\n" (fun a b -> Hashtbl.add tbl b a; build_graph tbl)
  with
  | End_of_file -> ()

let rec indirect vs tbl cnt =
  List.iter
    (fun v ->
       cnt := succ !cnt;          
       indirect (Hashtbl.find_all tbl v) tbl cnt)
    vs

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
  let count = ref 0 in
  build_graph table;
  Hashtbl.iter (fun k _ -> indirect (Hashtbl.find_all table k) table count) table;
  let you_path = path "YOU" [] table in
  let san_path = path "SAN" [] table in
  let (p1, p2) = drop_common you_path san_path in
  let steps = (List.length p1 + List.length p2) - 2 in
  Printf.printf "%d\n%d\n" !count steps
