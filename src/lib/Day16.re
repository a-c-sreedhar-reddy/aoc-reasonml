open Angstrom;
type rule = {
  name: string,
  rule1: (int, int),
  rule2: (int, int),
};
let isDigit = c => {
  switch (c) {
  | '0' .. '9' => true
  | _ => false
  };
};
let parseNumber = take_while(isDigit) >>| int_of_string;
let parserange =
  lift2((s, e) => (s, e), parseNumber <* char('-'), parseNumber);
let parseRule = rule =>
  parse_string(
    ~consume=All,
    lift3(
      (name, rule1, rule2) => {name, rule1, rule2},
      take_while(c => c != ':') <* string(": "),
      parserange <* string(" or "),
      parserange,
    ),
    rule,
  )
  |> (
    rule =>
      switch (rule) {
      | Ok(rule) => rule
      | Error(er) => failwith(er)
      }
  );
let parseTicket = ticket => {
  parse_string(~consume=All, sep_by(char(','), parseNumber), ticket)
  |> (
    rule =>
      switch (rule) {
      | Ok(rule) => rule
      | Error(er) => failwith(er)
      }
  );
};
let isNumberInRule = (~number, ~rule) => {
  let {rule1: (s, e), rule2: (s1, e1)} = rule;
  s <= number && number <= e || s1 <= number && number <= e1;
};
let isNumberinRules = (~number, ~rules) => {
  rules
  |> List.exists(({rule1: (s, e), rule2: (s1, e1)}) =>
       s <= number && number <= e || s1 <= number && number <= e1
     );
};
let isValidTicket = (~ticket, ~rules) => {
  ticket |> List.for_all(number => isNumberinRules(~number, ~rules));
};
let getInValidNumbersInTicket = (~ticket, ~rules) => {
  ticket |> List.filter(number => !isNumberinRules(~number, ~rules));
};
let addElements = l => {
  List.fold_left((sum, num) => sum + num, 0, l);
};
let allTicketsInRule = (~tickets, ~rule) => true;
let isNumberRuleIndexValid =
    (~number, ~ruleIndex, ~rule, ~validTickets, ~hashTable) => {
  switch (
    Hashtbl.find_opt(
      hashTable,
      (number |> string_of_int) ++ " " ++ (ruleIndex |> string_of_int),
    )
  ) {
  | Some(value) => value
  | None =>
    let res =
      isNumberInRule(~number, ~rule)
      && validTickets
      |> List.map(ticket => {
           switch (ticket) {
           | [] => failwith("ticket is finished")
           | [number, ..._] => number
           }
         })
      |> List.for_all(isNumberInRule(~rule, ~number=_));
    Hashtbl.add(
      hashTable,
      (number |> string_of_int) ++ " " ++ (ruleIndex |> string_of_int),
      res,
    );
    res;
  };
};
// let rec getValidMatch =
//         (
//           ~rules: list((int, rule)),
//           ~ticket,
//           ~validTickets,
//           ~match,
//           ~hashTable,
//         ) => {
//   let (status, li) = match;
//   switch (ticket, rules) {
//   | ([], []) => (Some(true), li)

//   | ([], _) => (Some(false), li)
//   | (_, []) => (Some(false), li)
//   | ([number, ...restTicket], rules) =>
//     rules
//     |> List.fold_left(
//          (acc, (ruleIndex, rule)) => {
//            acc |> (((status, li)) => status == Some(true))
//              ? acc
//              : isNumberRuleIndexValid(
//                  ~number,
//                  ~ruleIndex,
//                  ~rule,
//                  ~validTickets,
//                  ~hashTable,
//                )
//                  ? {
//                    getValidMatch(
//                      ~rules=
//                        rules |> List.filter(((_, crule)) => crule != rule),
//                      ~ticket=restTicket,
//                      ~validTickets=
//                        validTickets
//                        |> List.map(ticket => {
//                             switch (ticket) {
//                             | [] => failwith("ticket is finished")
//                             | [number, ...rest] => rest
//                             }
//                           }),
//                      ~match=(status, [(number, rule), ...li]),
//                      ~hashTable,
//                    );
//                  }
//                  : match
//          },
//          match,
//        )
//   };
// };

let rec getValidMatch = (~rules, ~validTickets, ~match, ~columns) => {
  // print_int(List.length(rules));
  // print_endline("rules*******************");
  // rules
  // |> List.map(rule => rule.name)
  // |> List.fold_left((ac, valu) => ac ++ valu, "")
  // |> print_endline;
  // print_newline();
  // print_endline("matches*******************");
  // match
  // |> List.map(((rule, _)) => rule.name)
  // |> List.fold_left((ac, valu) => ac ++ valu, "")
  // |> print_endline;
  // print_newline();
  // match
  // |> List.map(((rule, l)) =>
  //      rule.name ++ " " ++ (List.length(l) |> string_of_int) |> print_endline
  //    );
  print_int(List.length(match));
  print_newline();
  switch (rules, columns) {
  | ([], []) =>
    print_endline("found");
    Some(match);
  | ([], _) =>
    print_endline("colsnotempty");
    None;
  | (_, []) =>
    print_endline("RULEsnotempty");

    None;
  | (rules, [column, ...columns]) =>
    let matchedRules =
      rules
      |> List.filter(rule =>
           column |> List.for_all(number => isNumberInRule(~number, ~rule))
         );

    switch (matchedRules) {
    | [] =>
      print_endline("un matched rule");
      None;
    | [matchedRule] =>
      let match = [(matchedRule, column), ...match];
      getValidMatch(
        ~rules=rules |> List.filter((rule: rule) => rule != matchedRule),
        ~validTickets,
        ~match,
        ~columns,
      );
    | matchedRules =>
      matchedRules
      |> List.fold_left(
           (res, matchedRule) => {
             res == None
               ? getValidMatch(
                   ~rules=
                     rules |> List.filter((rule: rule) => rule != matchedRule),
                   ~validTickets,
                   ~match=[(matchedRule, column), ...match],
                   ~columns,
                 )
               : res
           },
           None,
         )
    };
  };
};
let rec getColums = (~tickets, ~columns) => {
  tickets |> List.for_all(ticket => List.length(ticket) == 0)
    ? columns
    : {
      let column =
        tickets
        |> List.map(ticket =>
             switch (ticket) {
             | [number, ..._rest] => number
             }
           );

      let rest =
        tickets
        |> List.map(ticket =>
             switch (ticket) {
             | [_, ...rest] => rest
             }
           );
      getColums(~tickets=rest, ~columns=columns @ [column]);
    };
};
let run = () => {
  let rules =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day16-rules.txt")
    |> List.fold_right((rule, rules) => [parseRule(rule), ...rules], _, []);
  let tickets =
    Util.Fs.readLines(
      "/home/a-c-sreedhar-reddy/aoc/src/lib/Day16-tickets.txt",
    )
    |> List.fold_left(
         (tickets, ticket) => {[parseTicket(ticket), ...tickets]},
         [],
       );
  //   let part1 =
  //     tickets
  //     |> List.map(ticket =>
  //          getInNumbersInTicket(~ticket, ~rules) |> addElements
  //        )
  //     |> addElements;
  let validTickets =
    tickets |> List.filter(isValidTicket(~rules, ~ticket=_));
  let ticket =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day16-ticket.txt")
    |> (
      tickets =>
        switch (tickets) {
        | [ticket, ..._] => parseTicket(ticket)
        | _ => failwith("more")
        }
    );

  getValidMatch(
    ~rules,
    ~validTickets,
    ~match=[],
    ~columns=getColums(~tickets=validTickets, ~columns=[]),
  );
};
