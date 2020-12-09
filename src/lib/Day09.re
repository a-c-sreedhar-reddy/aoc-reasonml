let isHackNumber = (~trackedInput, ~number) => {
  !(
    trackedInput
    |> List.exists(a => {
         List.exists(b => {number - b == a && a != b}, trackedInput)
       })
  );
};

let rec findHackNumber = (~trackedInput, ~nextInput) => {
  switch (nextInput) {
  | [] => failwith("no hacknumber")
  | [nextNumber, ...rest] =>
    List.length(trackedInput) < 25
      ? findHackNumber(
          ~trackedInput=[nextNumber, ...trackedInput],
          ~nextInput=rest,
        )
      : isHackNumber(~trackedInput, ~number=nextNumber)
          ? nextNumber
          : findHackNumber(
              ~trackedInput=[nextNumber, ...trackedInput],
              ~nextInput=rest,
            )
  };
};
type min = int;
type max = int;
type res =
  | Searching((int, min, max))
  | Failed
  | Found((min, max));
let getNumberFromStart = (~number, ~numberArr) => {
  let ending = ref(0);
  let minNumber =
    switch (numberArr) {
    | [] => failwith("empty array")
    | [min, ..._] => min
    };
  numberArr
  |> List.fold_left(
       (result, num) => {
         let res =
           switch (result) {
           | Searching((sum, min, max)) =>
             sum + num > number
               ? Failed
               : sum + num < number
                   ? Searching((
                       sum + num,
                       num < min ? num : min,
                       num > max ? num : max,
                     ))
                   : Found((num < min ? num : min, num > max ? num : max))
           | _ => result
           };
         ending := ending^ + 1;
         res;
       },
       Searching((0, minNumber, 0)),
     )
  |> (
    res =>
      switch (res) {
      | Found(index) => Ok(index)
      | _ => Error("not found")
      }
  );
};
let rec getContinuousList = (~number, ~numberArr) => {
  switch (numberArr) {
  | [] => failwith("not found")
  | [_a] => failwith("not found")
  | [first, ...rest] =>
    switch (getNumberFromStart(~number, ~numberArr)) {
    | Ok((min, max)) => min + max
    | Error(_) => getContinuousList(~number, ~numberArr=rest)
    }
  };
};
let run = () => {
  let input =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day09.txt")
    |> List.map(num => int_of_string(num));
  let part1 = findHackNumber(~trackedInput=[], ~nextInput=input);
  let part2 = getContinuousList(~number=part1, ~numberArr=input);
  "Part1: " ++ string_of_int(part1) ++ " Part2: " ++ string_of_int(part2);
};
