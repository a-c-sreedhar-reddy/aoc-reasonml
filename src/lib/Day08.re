type rule =
  | Acc(int)
  | Jmp(int)
  | Nop(int);
type bootResult =
  | Ended(int)
  | Infinite(int);
type executedRules = list(int);

let executeRule = (~index, ~rules, ~executedRules, ~accumulator) => {
  let accumulator =
    accumulator
    + (
      switch (rules[index]) {
      | Acc(acc) => acc
      | _ => 0
      }
    );
  let executedRules = [index, ...executedRules];
  let nextIndex =
    switch (rules[index]) {
    | Jmp(jmp) => index + jmp
    | _ => index + 1
    };
  (nextIndex, rules, executedRules, accumulator);
};

let rec play = (~index, ~rules, ~executedRules, ~accumulator) => {
  index == Array.length(rules)
    ? Ended(accumulator)
    : executedRules |> List.exists(cur => cur == index)
        ? Infinite(accumulator)
        : {
          let (nextIndex, rules, executedRules, accumulator) =
            executeRule(~index, ~rules, ~executedRules, ~accumulator);
          play(~index=nextIndex, ~rules, ~executedRules, ~accumulator);
        };
};

let rec playByChanging = (~index, ~rules, ~executedRules, ~accumulator) => {
  index == Array.length(rules)
    ? Ended(accumulator)
    : {
      (
        switch (rules[index]) {
        | Nop(nop) => Jmp(nop)
        | Jmp(jmp) => Nop(jmp)
        | a => a
        }
      )
      |> Array.set(rules, index);
      switch (play(~index, ~rules, ~executedRules, ~accumulator)) {
      | Ended(acc) => Ended(acc)
      | Infinite(_) =>
        (
          switch (rules[index]) {
          | Nop(nop) => Jmp(nop)
          | Jmp(jmp) => Nop(jmp)
          | a => a
          }
        )
        |> Array.set(rules, index);
        let (nextIndex, rules, executedRules, accumulator) =
          executeRule(~index, ~rules, ~executedRules, ~accumulator);
        playByChanging(
          ~index=nextIndex,
          ~rules,
          ~executedRules,
          ~accumulator,
        );
      };
    };
};

open Angstrom;
let acc =
  string("acc ")
  *> take_while(c =>
       switch (c) {
       | '-' => true
       | '+' => true
       | '0' .. '9' => true
       | _ => false
       }
     )
  >>| (acc => Acc(int_of_string(acc)));
let jmp =
  string("jmp ")
  *> take_while(c =>
       switch (c) {
       | '-' => true
       | '+' => true
       | '0' .. '9' => true
       | _ => false
       }
     )
  >>| (jmp => Jmp(int_of_string(jmp)));
let nop =
  string("nop ")
  *> take_while(c =>
       switch (c) {
       | '-' => true
       | '+' => true
       | '0' .. '9' => true
       | _ => false
       }
     )
  >>| (nop => Nop(int_of_string(nop)));
let run = () => {
  let rules =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day08.txt")
    |> List.mapi((indexx, ruleString) =>
         parse_string(~consume=All, acc <|> jmp <|> nop, ruleString)
       )
    |> List.mapi((i, rule) => {
         switch (rule) {
         | Ok(rule) => rule
         | Error(er) =>
           print_endline(er);
           failwith(er);
         }
       })
    |> Array.of_list;
  let part1 =
    switch (play(~index=0, ~rules, ~executedRules=[], ~accumulator=0)) {
    | Ended(acc) => acc
    | Infinite(acc) => acc
    };
  let part2 =
    switch (
      playByChanging(~index=0, ~rules, ~executedRules=[], ~accumulator=0)
    ) {
    | Ended(acc) => acc
    | Infinite(acc) => failwith("infinite loop")
    };
  "Part1:" ++ string_of_int(part1) ++ " part2: " ++ string_of_int(part2);
};
