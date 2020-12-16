type scanMode =
  | Start
  | Rules
  | Tickets
  | NearbyTickets;
open Angstrom;
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
    take_while(c => c != ':')
    *> string(": ")
    *> lift2(
         (rule1, rule2) => (rule1, rule2),
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
let isNumberinRules = (~number, ~rules) => {
  rules
  |> List.exists((((s, e), (s1, e1))) =>
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
  tickets
  |> List.map(ticket =>
       getInValidNumbersInTicket(~ticket, ~rules) |> addElements
     )
  |> addElements;
};
