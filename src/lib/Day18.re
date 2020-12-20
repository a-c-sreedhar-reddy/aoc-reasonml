open Angstrom;
let line = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2";

type tokens =
  | Number(int)
  | Add
  | Multiply
  | Bra
  | Ket;
let isDigit = c => {
  switch (c) {
  | '0' .. '9' => true
  | _ => false
  };
};
let numberParser =
  take_while1(isDigit) >>| (number => Number(int_of_string(number)));
let parseWhiteSpace = take_while(c => c == ' ');
let parseAdd = char('+') *> return(Add);
let parseMultiply = char('*') *> return(Multiply);
let parseBra = char('(') *> return(Bra);
let parseKet = char(')') *> return(Ket);

let parseTokens =
  many(
    parseWhiteSpace
    *> (numberParser <|> parseAdd <|> parseMultiply <|> parseBra <|> parseKet)
    <* parseWhiteSpace,
  );
let getTokensInFirstBracket = tokens => {
  let (_, tokensInBracket, tokens, _) =
    switch (tokens) {
    | [Bra, ...tokens] =>
      tokens
      |> List.fold_left(
           (
             (openingBracketCount, tokensInBracket, restTokens, foundClosing),
             token,
           ) => {
             let openingBracketCount =
               switch (token) {
               | Bra => openingBracketCount + 1
               | Ket => openingBracketCount - 1
               | _ => openingBracketCount
               };
             foundClosing
               ? (
                 openingBracketCount,
                 tokensInBracket,
                 restTokens @ [token],
                 foundClosing,
               )
               : openingBracketCount == 0
                   ? (openingBracketCount, tokensInBracket, restTokens, true)
                   : (
                     openingBracketCount,
                     tokensInBracket @ [token],
                     restTokens,
                     false,
                   );
           },
           (1, [], [], false),
         )
    | _ => failwith("no bracket")
    };
  (tokensInBracket, tokens);
};

let rec evaluate = tokens => {
  //   tokens
  //   |> List.map(token =>
  //        switch (token) {
  //        | Add => "+"
  //        | Multiply => "*"
  //        | Bra => "("
  //        | Ket => ")"
  //        | Number(int) => int |> string_of_int
  //        }
  //      )
  //   |> print_endline;
  switch (tokens) {
  //   |> List.fold_left((acc, cur) => acc ++ cur, "")

  | [] => 0
  | [Number(_), Add] => failwith("no tokens1")
  | [Number(_), Add, Add, ..._] => failwith("no tokens2")
  | [Number(_), Add, Multiply, ..._] => failwith("no tokens3")
  | [Number(_), Multiply, Multiply, ..._] => failwith("no tokens4")
  | [Number(_), Multiply, Add, ..._] => failwith("no tokens5")
  | [Number(_), Add, Ket, ..._] => failwith("no tokens6")
  | [Number(_), Multiply, Ket, ..._] => failwith("no tokens7")
  | [Add, ..._] => failwith("no tokens8")
  | [Multiply, ..._] => failwith("no tokens9")
  | [Number(_), Multiply] => failwith("no tokens 10")
  | [Number(_), Bra | Ket | Number(_), ..._] => failwith("no tokens 11")
  | [Bra, ...rest] =>
    let (a, rest) = evaluateFirstBracket([Bra, ...rest]);
    evaluate([Number(a), ...rest]);
  | [Ket, ..._] => failwith("no tokens 12")
  | [Number(int)] => int
  | [Number(int), Add, Number(int2), ...rest] =>
    evaluate([Number(int + int2), ...rest])
  | [Number(int), Multiply, Number(int2), ...rest] =>
    evaluate([Number(int * int2), ...rest])
  | [Number(int), Add, Bra, ...rest] =>
    let (bracketNumber, rest) = evaluateFirstBracket([Bra, ...rest]);
    evaluate([Number(bracketNumber + int), ...rest]);
  | [Number(int), Multiply, Bra, ...rest] =>
    let (bracketNumber, rest) = evaluateFirstBracket([Bra, ...rest]);
    evaluate([Number(bracketNumber * int), ...rest]);
  };
}
and evaluateFirstBracket = tokens => {
  let (tokensInBracket, tokens) = getTokensInFirstBracket(tokens);
  (evaluate(tokensInBracket), tokens);
};
let run = () => {
  Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day18.txt")
  |> List.map(str =>
       parse_string(~consume=All, parseTokens, str)
       |> (
         res => {
           switch (res) {
           | Ok(a) => a
           | Error(er) => failwith(er)
           };
         }
       )
     )
  |> List.map(evaluate)
  |> List.fold_left((acc, cur) => acc + cur, 0);
};
