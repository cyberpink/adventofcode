let code = [| 0;1;0;-1 |]
let run input p =
  let out = ref 0 in
  for i = 0 to Array.length input - 1 do
    out:= !out + code.((i+1)/(p+1) mod 4) * input.(i)
  done;
  abs @@ !out mod 10

let main =
  let str = ref "" in
  Scanf.scanf "%s\n" (fun i -> str := i);
  let input = Array.make (String.length !str) 0 in
  String.iteri (fun i c -> input.(i) <- (Char.code c) - (Char.code '0')) !str;
  let len = Array.length input in
  let output = Array.make len 0 in
  for r = 1 to 100 do
    for i = 0 to len - 1 do
      output.(i) <- run input i
    done;
    Array.iteri (fun i x -> input.(i) <- x) output
  done;
  print_endline @@ String.concat "" @@ List.map string_of_int @@ Array.to_list @@ Array.sub output 0 8
