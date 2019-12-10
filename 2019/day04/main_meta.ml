let prompt = Delimcc.new_prompt ()
let reset p = Delimcc.push_prompt prompt p

let pick_num = fun n ->
  Delimcc.shift0 prompt @@ fun k ->
  .< for i = .~n to 9 do
       .~(k .<i>.)
     done >.

let min = 347312
let max = 805915
let make_number count count2 =
  .< let rec loop len (adjc, adjl) last m =
       if len < 6 then
         .~(reset @@ fun () ->
            let num = pick_num .<last>. in
            .< let adj' = if .~num == last then (succ adjc, adjl) else (0, adjc :: adjl) in
               loop (len + 1) adj' .~num (m * 10 + .~num) >.)
       else
       if m >= min && m <= max then
         begin
           if List.exists ((<=) 1) (adjc :: adjl) then
             .~count := succ !(.~count);
           if List.exists ((=) 1) (adjc :: adjl) then
             .~count2 := succ !(.~count2);
         end
     in loop 0 (0, []) 0 0 >.

let main = 
  .< let count = ref 0 in
     let count2 = ref 0 in
     .~(make_number .<count>. .<count2>.);
     Printf.printf "%d\n%d\n" !count !count2 >.

let () = format_code Format.std_formatter (close_code main);
