open Angstrom;
let ids =
  Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day05.txt")
  |> List.map(str => {
       parse_string(
         ~consume=All,
         lift2(
           (row, column) => row * 8 + column,
           take_while(c => c == 'F' || c == 'B')
           >>| (
             seat => {
               "0b"
               ++ String.map(
                    ch => {
                      switch (ch) {
                      | 'F' => '0'
                      | 'B' => '1'
                      | _ => failwith("invalid data")
                      }
                    },
                    seat,
                  )
               |> int_of_string;
             }
           ),
           take_while(c => c == 'L' || c == 'R')
           >>| (
             seat => {
               "0b"
               ++ String.map(
                    ch => {
                      switch (ch) {
                      | 'L' => '0'
                      | 'R' => '1'
                      | _ => failwith("invalid data")
                      }
                    },
                    seat,
                  )
               |> int_of_string;
             }
           ),
         ),
         str,
       )
     })
  |> List.map(res =>
       switch (res) {
       | Ok(res) => res
       | Error(er) => failwith(er)
       }
     );
let sol =
  ids |> List.fold_right((curr, acc) => curr > acc ? curr : acc, _, 0);
let rec getMySeat = (ids, max, mainList) => {
  switch (ids) {
  | [cur, ...rest] =>
    cur == max
      ? getMySeat(rest, max, mainList)
      : mainList |> List.exists(c => c == cur + 1)
          ? getMySeat(rest, max, mainList) : cur + 1
  | [] => failwith("empty seat ids")
  };
};

let sol2 = getMySeat(ids, sol, ids);
let run = () =>
  "Part1:" ++ string_of_int(sol) ++ " Part2:" ++ string_of_int(sol2);
