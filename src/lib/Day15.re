type num = {
  first: int,
  second: option(int),
};
open Angstrom;
let isDigit = a =>
  switch (a) {
  | '0' .. '9' => true
  | _ => false
  };
module Numbers = Map.Make(Int);

let insertInitailNumbers = (~numbers, ~numbersMap) => {
  numbers
  |> Array.of_list
  |> Array.mapi((index, num) => (index + 1, num))
  |> Array.fold_left(
       (numbersMap, (index, num)) => {
         switch (Numbers.find_opt(num, numbersMap)) {
         | Some(numberInfo) =>
           numbersMap
           |> Numbers.add(num, {first: index, second: numberInfo.second})
         | None =>
           numbersMap |> Numbers.add(num, {first: index, second: None})
         }
       },
       numbersMap,
     );
};
let speak = (number, time, numbersMap) => {
  switch (Numbers.find_opt(number, numbersMap)) {
  | Some({first, _}) =>
    numbersMap |> Numbers.add(number, {first: time, second: Some(first)})
  | None => numbersMap |> Numbers.add(number, {first: time, second: None})
  };
};
let rec play = (~numbersMap, ~lastNumber, ~currentTime, ~till) => {
  let (lastNumber, numbersMap) =
    switch (Numbers.find_opt(lastNumber, numbersMap)) {
    | Some(numberInfo) =>
      switch (numberInfo.first, numberInfo.second) {
      | (first, Some(second)) => (
          first - second,
          speak(first - second, currentTime, numbersMap),
        )
      | (first, None) => (0, speak(0, currentTime, numbersMap))
      }
    | None => (0, speak(0, currentTime, numbersMap))
    };
  currentTime == till
    ? lastNumber
    : play(~numbersMap, ~lastNumber, ~currentTime=currentTime + 1, ~till);
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
  let numbersMap = insertInitailNumbers(~numbers, ~numbersMap=Numbers.empty);
  let play =
    play(~numbersMap, ~lastNumber, ~currentTime=List.length(numbers) + 1);
  let part1 = play(~till=2020);
  let part2 = play(~till=30000000);
  "part1: " ++ string_of_int(part1) ++ " part2: " ++ string_of_int(part2);
};
