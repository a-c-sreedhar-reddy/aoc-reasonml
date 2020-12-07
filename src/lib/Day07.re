type rule = {
  color: string,
  count: int,
};
type bag = {
  color: string,
  rules: list(rule),
};
type bags = list(bag);
open Angstrom;
let parseRules =
  string("no other bags")
  *> return([])
  <|> sep_by(
        string(", "),
        lift2(
          (count, color) => {{count, color}},
          take_while(c =>
            switch (c) {
            | '0' .. '9' => true
            | _ => false
            }
          )
          <* char(' ')
          >>| (
            count => {
              int_of_string(count);
            }
          ),
          lift3(
            (prim, sec, _) => prim ++ " " ++ sec,
            take_while(c => c != ' ') <* char(' '),
            take_while(c => c != ' ') <* char(' '),
            string("bags") <|> string("bag"),
          ),
        ),
      );
let parsed =
  Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day07.txt")
  |> List.map(ruleString => {
       parse_string(
         ~consume=All,
         lift3(
           (color, _contains, rules) => {color, rules},
           lift2(
             (prim, sec) => prim ++ " " ++ sec,
             take_while(c => c != ' ') <* char(' '),
             take_while(c => c != ' ') <* char(' '),
           ),
           string("bags contain "),
           parseRules,
         )
         <* char('.'),
         ruleString,
       )
     })
  |> List.mapi((i, res) =>
       switch (res) {
       | Ok(res) => res
       | Error(er) => failwith(er ++ string_of_int(i))
       }
     );
let getBagOfColor = color => parsed |> List.find(bag => bag.color == color);
let rec canContain = bag => {
  bag.color == "shiny gold"
    ? false
    : List.exists(
        (rule: rule) =>
          rule.color == "shiny gold" || canContain(getBagOfColor(rule.color)),
        bag.rules,
      );
};
let rec getBagsIntheBag = bag => {
  let rules = bag.rules;
  List.map(
    (rule: rule) => rule.count * getBagsIntheBag(getBagOfColor(rule.color)),
    rules,
  )
  |> List.fold_right((acc, cur) => acc + cur, _, 1);
};
let run = () => {
  let part1 = parsed |> List.filter(bag => canContain(bag)) |> List.length;
  let part2 = getBagsIntheBag(getBagOfColor("shiny gold")) - 1;
  "Part1:" ++ string_of_int(part1) ++ " Part2:" ++ string_of_int(part2);
};
