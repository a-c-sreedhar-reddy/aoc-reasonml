open Angstrom;
type maskNumber =
  | And(int)
  | Or(int);
type mask = list(maskNumber);
type address = int;
type value = int;
type instruction = {
  address,
  mask,
  value,
};

type parseResult =
  | Mask(list(char))
  | Instruction((address, value));
let returnChar = c => char(c) *> return(c);
let parseMask =
  string("mask = ")
  *> many(returnChar('0') <|> returnChar('1') <|> returnChar('X'))
  >>| (a => Mask(a));
type acc = {
  instructions: list(instruction),
  mask: option(mask),
};
let parseInstruction =
  lift2(
    (address, value) => Instruction((address, value)),
    string("mem[")
    *> take_while(a =>
         switch (a) {
         | '0' .. '9' => true
         | _ => false
         }
       )
    >>= (
      a => {
        return(int_of_string(a));
      }
    )
    <* string("] = "),
    take_while(a =>
      switch (a) {
      | '0' .. '9' => true
      | _ => false
      }
    )
    >>= (
      a => {
        return(int_of_string(a));
      }
    ),
  );
type memory = {
  address: int,
  value,
};
module Address = Map.Make(Int);

type part2acc('a) = {
  baseMask: int,
  xorMask: list(int),
  result: Address.t('a),
};
let part2 = () => {
  let instructions =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day14.txt")
    |> List.map(line => {
         parse_string(~consume=All, parseMask <|> parseInstruction, line)
       })
    |> List.map(ins =>
         switch (ins) {
         | Ok(ins) => ins
         | Error(er) => failwith(er)
         }
       );
  let result = Address.empty;
  instructions
  |> List.fold_left(
       (acc, inst) => {
         switch (inst) {
         | Mask(chars) =>
           let baseMask =
             chars
             |> List.fold_left(
                  (acc, char) => char == '1' ? acc ++ "1" : acc ++ "0",
                  "0b",
                )
             |> int_of_string;
           let xorMask =
             chars
             |> List.fold_left(
                  (acc, char) => {
                    char == 'X'
                      ? List.map(ad => ad ++ "0", acc)
                        @ List.map(ad => ad ++ "1", acc)
                      : List.map(ad => ad ++ "0", acc)
                  },
                  ["0b"],
                )
             |> List.map(int_of_string);
           {...acc, baseMask, xorMask};
         | Instruction((address, value)) =>
           let maskedAddress = address lor acc.baseMask;
           let possibleAddress =
             acc.xorMask |> List.map(xormask => xormask lxor maskedAddress);
           let result =
             possibleAddress
             |> List.fold_left(
                  (result, address) => {
                    result |> Address.add(address, value)
                  },
                  acc.result,
                );
           {...acc, result};
         }
       },
       {baseMask: 0, xorMask: [], result},
     )
  |> (acc => acc.result)
  |> Address.fold((key, data, acc) => acc + data, _, 0);
};
let run = () => {
  let instructions =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day14.txt")
    |> List.map(line => {
         parse_string(~consume=All, parseMask <|> parseInstruction, line)
       })
    |> List.map(ins =>
         switch (ins) {
         | Ok(ins) => ins
         | Error(er) => failwith(er)
         }
       )
    |> List.fold_left(
         (acc, inst) => {
           switch (inst) {
           | Mask(chars) =>
             let mask =
               chars
               |> Array.of_list
               |> Array.mapi((index, c) => {(c, 35 - index)})
               |> Array.to_list
               |> List.filter(((c, index)) => c == '0' || c == '1')
               |> List.map(((c, index)) => {
                    switch (c) {
                    | '1' => Or(2. ** float_of_int(index) |> int_of_float)
                    | '0' =>
                      And(
                        2.
                        ** 36.
                        -. 2.
                        ** float_of_int(index)
                        -. 1.
                        |> int_of_float,
                      )
                    | _ => failwith("unreachabl")
                    }
                  });
             {...acc, mask: Some(mask)};
           | Instruction((address, value)) =>
             switch (acc.mask) {
             | Some(mask) => {
                 ...acc,
                 instructions:
                   acc.instructions
                   |> List.filter((instruction: instruction) => {
                        instruction.address != address
                      })
                   |> (
                     instructions => {
                       [{address, mask, value}, ...instructions];
                     }
                   ),
               }
             | _ => failwith("no mask")
             }
           }
         },
         {instructions: [], mask: None},
       )
    |> (({instructions}) => instructions);
  let part1 =
    instructions
    |> List.fold_left(
         (sum, {address, mask, value}) => {
           sum
           + List.fold_left(
               (res, op) => {
                 switch (op) {
                 | And(op) => res land op
                 | Or(op) => res lor op
                 }
               },
               value,
               mask,
             )
         },
         0,
       );
  let part2 = part2();
  "Part1: " ++ string_of_int(part1) ++ " Part2: " ++ string_of_int(part2);
};
