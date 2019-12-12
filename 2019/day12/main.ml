let vec3 a b c = [| a; b; c |]
let grav a b = Array.map2 compare b a
let add a b = Array.map2 (+) a b
let sum a = Array.fold_left (+) 0 a
let engy a = sum @@ Array.map abs a

let rec gcd a b =
  if b = 0
  then a
  else gcd b (a mod b)

let lcm a b = (abs (a * b)) / (gcd a b)

type planet = { p : int array; v : int array }
let mk_planet p = { p = p; v = vec3 0 0 0 }

let read_planet () = Scanf.scanf "<x=%d, y=%d, z=%d>\n" vec3
let print_planet p =
  Printf.printf "pos=<x=%d, y=%d, z=%d>, vel=<x=%d, y=%d, z=%d>\n"
    p.p.(0) p.p.(1) p.p.(2) p.v.(0) p.v.(1) p.v.(2)

let step planets =
  let calc p =
    let ds = List.map (fun p2 -> grav p.p p2.p) planets in
    let v' = List.fold_left add p.v ds in
    { p = add p.p v'; v = v' }
  in List.map calc planets

let p1 planets =
  let planets = ref planets in
  for i = 0 to 999 do
    planets := step !planets
  done;
  List.fold_left (+) 0 @@ List.map (fun p -> engy p.p * engy p.v) !planets

let p2 planets = 
  let rec loop d ps i =
    let ps' = step ps in
    let i' = succ i in
    if List.for_all2 (fun a b -> a.v.(d) = b.v.(d) && a.p.(d) = b.p.(d)) ps' planets then
      i'
    else
      loop d ps' i'
  in
  let m = ref 1 in
  for d = 0 to 2 do
    m := lcm !m (loop d planets 0)
  done;
  !m

let main =
  let planets = ref [] in
  try
    while true do
      planets := mk_planet (read_planet ()) :: !planets;
    done
  with
    End_of_file ->
    let planets = List.rev !planets in
    Printf.printf "%d\n" @@ p1 planets;
    Printf.printf "%d\n" @@ p2 planets
