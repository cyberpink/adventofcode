let rec build_graph tbl rev =
  try
    Scanf.scanf "%3s)%3s\n" (fun a b -> Hashtbl.add tbl b a; Hashtbl.add rev a b; build_graph tbl rev)
  with
  | End_of_file -> ()

(* let indirect tbl =
 *   let cnt = ref 0 in
 *   let rec aux vs =
 *     List.iter
 *       (fun v ->
 *          cnt := succ !cnt;          
 *          aux (Hashtbl.find_all tbl v))
 *       vs
 *   in
 *   Hashtbl.iter (fun k _ -> aux (Hashtbl.find_all tbl k)) tbl;
 *   !cnt *)

(* memoized *)
let indirect tbl =
  let memo = Hashtbl.create 100 in
  let rec get_count x =
    match Hashtbl.find memo x with
    | value -> value
    | exception Not_found ->
      let children = Hashtbl.find_all tbl x in
      let ccount = List.fold_left (+) 0 (List.map (fun c -> 1 + get_count c) children) in
      let value = ccount in
      Hashtbl.add memo x value;
      value
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
  let rev = Hashtbl.create 100 in
  build_graph table rev;
  let you_path = path "YOU" [] table in
  let san_path = path "SAN" [] table in
  let (p1, p2) = drop_common you_path san_path in
  let steps = (List.length p1 + List.length p2) - 2 in
  Printf.printf "%d\n%d\n" (indirect table) steps
