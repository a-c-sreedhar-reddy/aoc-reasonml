let destructureString = rule => {
  let [minMax, char] = String.split_on_char(' ', rule);
  let [min, max] = String.split_on_char('-', minMax);
  (int_of_string(min), int_of_string(max), char.[0]);
};
let rec getCharCount = (char, str, from) => {
  String.length(str) <= from
    ? 0
    : (
      switch (str) {
      | "" => 0
      | _ =>
        let count = getCharCount(char, str, from + 1);
        str.[from] == char ? count + 1 : count;
      }
    );
};
let isEntryCorrect = (~entry, ~validation) => {
  switch (entry) {
  | [rule, input] =>
    let (minCount, maxCount, char) = destructureString(rule);
    validation(minCount, maxCount, char, input);

  | _ => false
  };
};
let run = () => {
  let input =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/DayTwo.txt");
  let part1 =
    input
    |> List.map(String.split_on_char(':'))
    |> List.filter(entry =>
         isEntryCorrect(
           ~entry,
           ~validation=(minCount, maxCount, char, input) => {
             let count = getCharCount(char, input, 0);
             minCount <= count && count <= maxCount;
           },
         )
       )
    |> List.length;
  let part2 =
    input
    |> List.map(String.split_on_char(':'))
    |> List.filter(entry =>
         isEntryCorrect(~entry, ~validation=(first, second, char, input) => {
           Util.(input.[first] == char <<>> (input.[second] == char))
         })
       )
    |> List.length;
  "Part1: " ++ string_of_int(part1) ++ " part2:" ++ string_of_int(part2);
};

let acceptLabeledArg = (~name) => true;

acceptLabeledArg(56);
