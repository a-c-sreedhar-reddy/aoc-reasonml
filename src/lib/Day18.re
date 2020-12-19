let line = "1 + 2 * 3 + 4 * 5 + 6";

open Angstrom;

type operator =
  | Add
  | Multiply;

let parseAdd = char('+') *> return(Add);
let parseMultiply = char('*') *> return(Multiply);
let parseOperator = parseAdd <|> parseMultiply;

type expression =
  | OperatorExpression(expression, operator, expression)
  | Number(int)
  | BracketExpression(expression);

let isDigit = c => {
  print_endline("isDigit");
  switch (c) {
  | '0' .. '9' => true
  | _ => false
  };
};

let numberParser =
  take_while1(isDigit) >>| (number => Number(int_of_string(number)));
let parseWhiteSpace = take_while(c => c == ' ');

let parse =
  fix(parse => {
    let parseOperatorExpression =
      lift3(
        (expr1, operator, expr2) => {
          OperatorExpression(expr1, operator, expr2)
        },
        parse,
        parseOperator,
        parse,
      );
    let parseBracketExpression = char('(') *> parse <* char(')');
    parseWhiteSpace
    *> (parseBracketExpression <|> parseOperatorExpression <|> numberParser)
    <* parseWhiteSpace;
  });

let run = () =>
  parse_string(~consume=All, parse, line)
  |> (
    res => {
      switch (res) {
      | Ok(res) => res
      | Error(er) => failwith(er)
      };
    }
  );
