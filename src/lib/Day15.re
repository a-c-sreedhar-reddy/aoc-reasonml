type num = {
  first: int,
  second: option(int),
};
open Angstrom;
external sum: unit => int = "sum";

let isDigit = a =>
  switch (a) {
  | '0' .. '9' => true
  | _ => false
  };
module Numbers = Map.Make(Int);
let insertInitailNumbers = (~numbers, ~numbersHash) => {
  numbers
  |> Array.of_list
  |> Array.mapi((index, num) => (index + 1, num))
  |> Array.fold_left(
       (numbersHash, (index, num)) => {
         switch (Hashtbl.find_opt(numbersHash, num)) {
         | Some(numberInfo) =>
           numbersHash
           |> Hashtbl.replace(
                _,
                num,
                {first: index, second: numberInfo.second},
              )
         | None =>
           numbersHash |> Hashtbl.add(_, num, {first: index, second: None})
         };
         numbersHash;
       },
       numbersHash,
     );
};
let speak = (number, time, numbersHash) => {
  switch (Hashtbl.find_opt(numbersHash, number)) {
  | Some({first, _}) =>
    numbersHash
    |> Hashtbl.replace(_, number, {first: time, second: Some(first)})
  | None =>
    numbersHash |> Hashtbl.add(_, number, {first: time, second: None})
  };
  numbersHash;
};
let rec play = (~numbersHash, ~lastNumber, ~currentTime, ~till) => {
  let (lastNumber, numbersHash) =
    switch (Hashtbl.find_opt(numbersHash, lastNumber)) {
    | Some(numberInfo) =>
      switch (numberInfo.first, numberInfo.second) {
      | (first, Some(second)) => (
          first - second,
          speak(first - second, currentTime, numbersHash),
        )
      | (first, None) => (0, speak(0, currentTime, numbersHash))
      }
    | None => (0, speak(0, currentTime, numbersHash))
    };
  currentTime == till
    ? lastNumber
    : play(~numbersHash, ~lastNumber, ~currentTime=currentTime + 1, ~till);
};
let run = () => {
  let numbers =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day15.txt")
    |> (
      lines => {
        switch (lines) {
        | [] => failwith("input not given")
        | [numbers, ..._] =>
          parse_string(
            ~consume=All,
            sep_by(
              char(','),
              take_while(isDigit)
              >>| (
                a => {
                  int_of_string(a);
                }
              ),
            ),
            numbers,
          )
        };
      }
    )
    |> (
      res =>
        switch (res) {
        | Ok(res) => res
        | Error(er) => failwith(er)
        }
    );
  let lastNumber = numbers |> List.fold_left((acc, cur) => cur, 0);
  let play = play(~lastNumber, ~currentTime=List.length(numbers) + 1);
  let part1 =
    play(
      ~till=2020,
      ~numbersHash=
        Hashtbl.create(2020)
        |> insertInitailNumbers(~numbers, ~numbersHash=_),
    );
  let time = Sys.time();
  let part2 =
    play(
      ~till=30000000,
      ~numbersHash=
        Hashtbl.create(30000000)
        |> insertInitailNumbers(~numbers, ~numbersHash=_),
    );
  "part1: "
  ++ string_of_int(part1)
  ++ " part2: "
  ++ string_of_int(part2)
  ++ " time"
  ++ string_of_float(Sys.time() -. time);
};
