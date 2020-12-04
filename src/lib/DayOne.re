// open Containers;
// open Libr  ary;
let rec listFind = (~list, ~element) => {
  switch (list) {
  | [] => None
  | [a] => a == element ? Some(element) : None
  | [start, ...rest] =>
    start == element ? Some(element) : listFind(~list=rest, ~element)
  };
};
let rec getTwoNumberswithSum = (~l, ~sum) => {
  switch (l) {
  | [] => None
  | [_start] => None
  | [start, ...list] =>
    switch (listFind(~list, ~element=sum - start)) {
    | Some(second) => Some((start, second))
    | None => getTwoNumberswithSum(~l=list, ~sum)
    }
  };
};
let rec getThreeNumersWithSum2020 = (~l) => {
  switch (l) {
  | [] => None
  | [_one] => None
  | [_one, _two] => None
  | [one, ...rest] =>
    switch (getTwoNumberswithSum(~l=rest, ~sum=2020 - one)) {
    | Some((a, b)) => Some((a, b, one))
    | None => getThreeNumersWithSum2020(~l=rest)
    }
  };
};
let run = () => {
  let inputList =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/DayOne.txt")
    |> List.map(element => int_of_string(element));
  let part1 =
    switch (getTwoNumberswithSum(~l=inputList, ~sum=2020)) {
    | Some((a, b)) => a * b
    | None => 0
    };
  let part2 =
    switch (getThreeNumersWithSum2020(~l=inputList)) {
    | Some((a, b, c)) => a * b * c
    | None => 0
    };
  "part1 " ++ string_of_int(part1) ++ " part2:" ++ string_of_int(part2);
};
