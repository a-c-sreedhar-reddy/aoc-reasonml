open Angstrom;
type bus = {
  id: int,
  index: int,
};
let rec findCorrectTime = (busIds, time, firstBusId) => {
  time == 1068781
    ? {
      print_newline();
      print_int(time);
      print_newline();
    }
    : ();

  List.for_all(
    ((busId, index)) => {
      (
        switch (time mod busId) {
        | 0 => 0
        | diff => busId - diff
        }
      )
      == index
    },
    busIds,
  )
    ? time : findCorrectTime(busIds, time + firstBusId, firstBusId);
};
let rec gcd = (a, b) =>
  a <= b ? b mod a == 0 ? a : gcd(b mod a, a) : gcd(b, a);
let lcm = (a, b) => a * b / gcd(a, b);
let modinverse = (a, b) => a;
let runPart2 = () => {
  let input =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day13.txt");
  let (time, busIds) =
    switch (input) {
    | [time, busIDs, ..._] =>
      let busIds =
        parse_string(
          ~consume=All,
          sep_by(
            char(','),
            char('x')
            *> return(None)
            <|> (
              take_while(a =>
                switch (a) {
                | '0' .. '9' => true
                | _ => false
                }
              )
              >>= (id => return(Some(int_of_string(id))))
            ),
          ),
          busIDs,
        )
        |> (
          busIds => {
            switch (busIds) {
            | Ok(busIds) => busIds
            | Error(er) => failwith(er)
            };
          }
        )
        |> Array.of_list
        |> Array.mapi((index, busId) => {
             switch (busId) {
             | None => None
             | Some(busId) => Some({id: busId, index: busId - index})
             }
           })
        |> Array.to_list
        |> List.filter(busId => busId != None)
        |> List.map(busId =>
             switch (busId) {
             | Some(busId) => busId
             | None => failwith("unreachable cde")
             }
           );

      (int_of_string(time), busIds);
    | _ => failwith("input parse error")
    };
  let bigM = busIds |> List.fold_left((acc, bus) => {acc * bus.id}, 1);
  let time =
    busIds
    |> List.fold_left(
         (acc, {id, index}) => {
           let z = bigM / id;
           let y = Util.mod_inv(z, id);
           let w = y * z mod bigM;
           acc + index * w;
         },
         0,
       );
  time mod bigM;
};

let run = () => {
  let input =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day13.txt");
  let (time, busIds) =
    switch (input) {
    | [time, busIDs, ..._] =>
      let busIds =
        parse_string(
          ~consume=All,
          sep_by(
            char(','),
            char('x')
            *> return(None)
            <|> (
              take_while(a =>
                switch (a) {
                | '0' .. '9' => true
                | _ => false
                }
              )
              >>= (id => return(Some(int_of_string(id))))
            ),
          ),
          busIDs,
        )
        |> (
          busIds => {
            switch (busIds) {
            | Ok(busIds) => busIds
            | Error(er) => failwith(er)
            };
          }
        )
        |> List.filter(busId => busId != None)
        |> List.map(busId =>
             switch (busId) {
             | Some(busId) => busId
             | None => failwith("unreachable cde")
             }
           );

      (int_of_string(time), busIds);
    | _ => failwith("input parse error")
    };
  let diffs =
    busIds
    |> List.map(x => {
         switch (time mod x) {
         | 0 => (x, 0)
         | diff => (x, x - diff)
         }
       });
  let acc =
    switch (diffs) {
    | [] => failwith("no buses")
    | [(busId, diff), ..._] => (diff, diff * busId)
    };
  let (_, part1) =
    diffs
    |> List.fold_left(
         ((minDiff, result), (busId, diff)) => {
           diff < minDiff ? (diff, diff * busId) : (minDiff, result)
         },
         acc,
       );

  let part2 = runPart2();
  "Part1: " ++ string_of_int(part1) ++ " Part2:" ++ string_of_int(part2);
};
