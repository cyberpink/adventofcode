let w = 25
let h = 6

let to_int c = char_of_int @@ (int_of_char c) - (int_of_char '0')
let _ =
  let b = Buffer.create 1024 in
  let () = try Buffer.add_channel b stdin 20000 with _ -> () in
  Buffer.truncate b (Buffer.length b - 1);
  let bytes = Bytes.map to_int @@ Buffer.to_bytes b in
  let block_size = w * h in
  let blocks = Bytes.length bytes / block_size in

  let best = ref [| max_int; 0; 0 |] in
  let out = Bytes.map (fun _ -> char_of_int 2) @@ Bytes.create block_size in
  let merge i x =
    if Bytes.get out i = char_of_int 2 then
      Bytes.set out i x
  in
  for i = 0 to blocks - 1 do
    let counts = Array.make 3 0 in
    for j = 0 to block_size - 1 do
      let char = Bytes.get bytes (i * block_size + j) in
      let digit = int_of_char char in
      counts.(digit) <- succ counts.(digit);
      merge j char
    done;
    if counts.(0) < !best.(0) then
      best := counts
  done;
  Printf.printf "%d\n" @@ !best.(1) * !best.(2);
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let d = int_of_char @@ Bytes.get out (y * w + x) in
      print_string @@ if d == 1 then "X" else " "
    done;
    print_newline ()
  done;

