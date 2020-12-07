module SS = Set.Make(Char);

let options =
  Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day06.txt")
  |> List.fold_left(
       ((accResult, accString), current) => {
         switch (current) {
         | "" => ([accString, ...accResult], "")
         | _ => (accResult, accString ++ current)
         }
       },
       ([], ""),
     )
  |> (((l, s)) => [s, ...l])
  |> List.map(options => {
       let i = ref(0);
       for (ch in int_of_char('a') to int_of_char('z')) {
         let ch = char_of_int(ch);
         String.contains(options, ch) ? i := i^ + 1 : ();
       };
       i^;
     });
let run = () => options |> List.fold_left((c, acc) => c + acc, 0);
