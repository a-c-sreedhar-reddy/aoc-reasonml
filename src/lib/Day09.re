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

let run = () => {
  let input =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day09.txt")
    |> List.map(num => int_of_string(num));

  findHackNumber(~trackedInput=[], ~nextInput=input);
};
