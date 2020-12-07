module SS = Set.Make(Char);

let sol1 =
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
     })
  |> List.fold_left((c, acc) => c + acc, 0);

let sol2 =
  Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day06.txt")
  |> List.fold_left(
       ((accResult, accString), current) => {
         switch (current) {
         | "" => ([accString, ...accResult], [])
         | _ => (accResult, [current, ...accString])
         }
       },
       ([], []),
     )
  |> (((l, s)) => [s, ...l])
  |> List.map(options => {
       let i = ref(0);
       for (ch in int_of_char('a') to int_of_char('z')) {
         let ch = char_of_int(ch);
         List.for_all(str => String.contains(str, ch), options)
           ? i := i^ + 1 : ();
       };
       i^;
     })
  |> List.fold_left((c, acc) => c + acc, 0);

let run = () =>
  "part1: " ++ string_of_int(sol1) ++ " part2: " ++ string_of_int(sol2);
