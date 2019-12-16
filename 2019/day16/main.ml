let code = [| 0;1;0;-1 |]
let p1 _input =
  let input = Array.copy _input in
  let len = Array.length input in
  let output = Array.make len 0 in
  for iter = 1 to 100 do
    for d = 0 to len - 1 do
      output.(d) <- 0;
      for i = 0 to len - 1 do
        output.(d) <- output.(d) + code.((i+1)/(d+1) mod 4) * input.(i)
      done;
      output.(d) <- abs @@ output.(d) mod 10
    done;
    Array.iteri (fun i x -> input.(i) <- x) output
  done;
  Array.sub output 0 8

let main =
  let str = Scanf.scanf "%s\n" (fun i -> i) in
  let input = Array.make (String.length str) 0 in
  String.iteri (fun i c -> input.(i) <- (Char.code c) - (Char.code '0')) str;
  print_endline @@ String.concat "" @@
  List.map string_of_int @@ Array.to_list @@ p1 input;
  Printf.printf "%d\n" ((String.length str * 10000) - (int_of_string (String.sub str 0 7)))
    
