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
                   |> List.filter(instruction => {
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
};
