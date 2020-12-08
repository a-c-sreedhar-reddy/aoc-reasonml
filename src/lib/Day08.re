type rule =
  | Acc(int)
  | Jmp(int)
  | Nop;
type executedRules = list(int);

let rec executeRule = (~index, ~rules, ~executedRules, ~accumulator) => {
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
  executedRules |> List.exists(cur => cur == nextIndex)
    ? accumulator
    : executeRule(~index=nextIndex, ~rules, ~executedRules, ~accumulator);
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
  *> return(Nop);
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
  executeRule(~index=0, ~rules, ~executedRules=[], ~accumulator=0);
};
