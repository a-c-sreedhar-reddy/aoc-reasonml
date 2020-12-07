open Angstrom;
let sol =
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
     )
  |> List.fold_right((curr, acc) => curr > acc ? curr : acc, _, 0);

let run = () => sol;
