let vec3 a b c = [| a; b; c |]
let grav a b = Array.map2 compare b a
let add a b = Array.map2 (+) a b
let engy a = Array.fold_left (+) 0 @@ Array.map abs a

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = (abs (a * b)) / (gcd a b)

let step (ps, vs) =
  let calc_vs p v = List.fold_left add v @@ List.map (grav p) ps in
  let vs' = List.map2 calc_vs ps vs in
  (List.map2 add ps vs', vs')

let p1 (_ps, _vs) =
  let pvs = ref (_ps, _vs) in
  for i = 1 to 1000 do
    pvs := step !pvs
  done;
  let (ps, vs) = !pvs in
  List.fold_left (+) 0 @@ List.map2 (fun p v -> engy p * engy v) ps vs

let p2 (_ps, _vs) =
  let rec loop pvs i ds m =
    let (ps', vs') = step pvs in
    let i' = succ i in
    let fold_dim (dm, mm) d =
      let ps_eq = List.for_all2 (fun a b -> a.(d) = b.(d)) ps' _ps in
      let vs_eq = List.for_all2 (fun a b -> a.(d) = b.(d)) vs' _vs in 
      if ps_eq && vs_eq then
        (dm, lcm i' mm)
      else
        (d :: dm, mm)
    in
    match List.fold_left fold_dim ([], m) ds with
    | ([], m') -> m'
    | (ds', m') -> loop (ps', vs') i' ds' m'
  in loop (_ps, _vs) 0 [0;1;2] 1

let read_pos () = Scanf.scanf "<x=%d, y=%d, z=%d>\n" vec3
let rec setup ps vs =
  match read_pos () with
  | p -> setup (p :: ps) (vec3 0 0 0 :: vs)
  | exception End_of_file -> (List.rev ps, List.rev vs)

let main =
  let (ps, vs) = setup [] [] in
  Printf.printf "%d\n" @@ p1 (ps, vs);
  Printf.printf "%d\n" @@ p2 (ps, vs)
