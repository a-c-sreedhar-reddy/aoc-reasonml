type direction =
  | North
  | South
  | East
  | West
  | Left
  | Right
  | Forword;
type instruction = {
  direction,
  value: int,
};

type cordinates = {
  x: int,
  y: int,
};
type facing =
  | NorthFacing
  | SouthFacing
  | EastFacing
  | WestFacing;
type shipPosition = {
  facing,
  cordinates,
};
type shipPostion2 = {
  shipPosition: cordinates,
  wayPointPosition: cordinates,
};
open Angstrom;
let north = string("N") *> return(North);
let south = string("S") *> return(South);
let east = string("E") *> return(East);
let west = string("W") *> return(West);
let left = string("L") *> return(Left);
let right = string("R") *> return(Right);
let forword = string("F") *> return(Forword);
let isDigit = c => {
  switch (c) {
  | '0' .. '9' => true
  | _ => false
  };
};
let run = () => {
  let instructions =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day12.txt")
    |> List.map(instruction =>
         parse_string(
           ~consume=All,
           lift2(
             (direction, value) => {{direction, value}},
             north <|> south <|> east <|> west <|> left <|> right <|> forword,
             take_while(isDigit) >>| (s => int_of_string(s)),
           ),
           instruction,
         )
       )
    |> List.map(inst =>
         switch (inst) {
         | Ok(inst) => inst
         | Error(er) => failwith(er)
         }
       );
  let part1 =
    instructions
    |> List.fold_left(
         (acc, inst) => {
           switch (inst.direction) {
           | East => {
               ...acc,
               cordinates: {
                 ...acc.cordinates,
                 x: acc.cordinates.x + inst.value,
               },
             }
           | North => {
               ...acc,
               cordinates: {
                 ...acc.cordinates,
                 y: acc.cordinates.y + inst.value,
               },
             }
           | South => {
               ...acc,
               cordinates: {
                 ...acc.cordinates,
                 y: acc.cordinates.y - inst.value,
               },
             }
           | West => {
               ...acc,
               cordinates: {
                 ...acc.cordinates,
                 x: acc.cordinates.x - inst.value,
               },
             }
           | Left =>
             let times = inst.value / 90;
             {
               ...acc,
               facing: {
                 switch (acc.facing, times) {
                 | (EastFacing, 1) => NorthFacing
                 | (EastFacing, 2) => WestFacing
                 | (EastFacing, 3) => SouthFacing
                 | (SouthFacing, 1) => EastFacing
                 | (SouthFacing, 2) => NorthFacing
                 | (SouthFacing, 3) => WestFacing
                 | (WestFacing, 1) => SouthFacing
                 | (WestFacing, 2) => EastFacing
                 | (WestFacing, 3) => NorthFacing
                 | (NorthFacing, 1) => WestFacing
                 | (NorthFacing, 2) => SouthFacing
                 | (NorthFacing, 3) => EastFacing
                 | _ => acc.facing
                 };
               },
             };
           | Right =>
             let times = inst.value / 90;
             {
               ...acc,
               facing: {
                 switch (acc.facing, times) {
                 | (EastFacing, 1) => SouthFacing
                 | (EastFacing, 2) => WestFacing
                 | (EastFacing, 3) => NorthFacing
                 | (SouthFacing, 1) => WestFacing
                 | (SouthFacing, 2) => NorthFacing
                 | (SouthFacing, 3) => EastFacing
                 | (WestFacing, 1) => NorthFacing
                 | (WestFacing, 2) => EastFacing
                 | (WestFacing, 3) => SouthFacing
                 | (NorthFacing, 1) => EastFacing
                 | (NorthFacing, 2) => SouthFacing
                 | (NorthFacing, 3) => WestFacing
                 | _ => acc.facing
                 };
               },
             };
           | Forword => {
               ...acc,
               cordinates: {
                 switch (acc.facing) {
                 | EastFacing => {
                     ...acc.cordinates,
                     x: acc.cordinates.x + inst.value,
                   }
                 | WestFacing => {
                     ...acc.cordinates,
                     x: acc.cordinates.x - inst.value,
                   }
                 | NorthFacing => {
                     ...acc.cordinates,
                     y: acc.cordinates.y + inst.value,
                   }
                 | SouthFacing => {
                     ...acc.cordinates,
                     y: acc.cordinates.y - inst.value,
                   }
                 };
               },
             }
           }
         },
         {
           facing: EastFacing,
           cordinates: {
             x: 0,
             y: 0,
           },
         },
       )
    |> (({cordinates: {x, y}}) => abs(x) + abs(y));
  let part2 =
    instructions
    |> List.fold_left(
         (acc, inst) => {
           switch (inst.direction) {
           | East => {
               ...acc,
               wayPointPosition: {
                 ...acc.wayPointPosition,
                 x: acc.wayPointPosition.x + inst.value,
               },
             }
           | North => {
               ...acc,
               wayPointPosition: {
                 ...acc.wayPointPosition,
                 y: acc.wayPointPosition.y + inst.value,
               },
             }
           | South => {
               ...acc,
               wayPointPosition: {
                 ...acc.wayPointPosition,
                 y: acc.wayPointPosition.y - inst.value,
               },
             }
           | West => {
               ...acc,
               wayPointPosition: {
                 ...acc.wayPointPosition,
                 x: acc.wayPointPosition.x - inst.value,
               },
             }
           | Left =>
             let times = inst.value / 90;
             {
               ...acc,

               wayPointPosition: {
                 switch (times) {
                 | 1 => {
                     x: (-1) * acc.wayPointPosition.y,
                     y: acc.wayPointPosition.x,
                   }
                 | 2 => {
                     x: (-1) * acc.wayPointPosition.x,
                     y: (-1) * acc.wayPointPosition.y,
                   }
                 | 3 => {
                     x: acc.wayPointPosition.y,
                     y: (-1) * acc.wayPointPosition.x,
                   }
                 | _ => acc.wayPointPosition
                 };
               },
             };
           | Right =>
             let times = inst.value / 90;
             {
               ...acc,

               wayPointPosition: {
                 switch (times) {
                 | 1 => {
                     x: acc.wayPointPosition.y,
                     y: (-1) * acc.wayPointPosition.x,
                   }
                 | 2 => {
                     x: (-1) * acc.wayPointPosition.x,
                     y: (-1) * acc.wayPointPosition.y,
                   }
                 | 3 => {
                     x: (-1) * acc.wayPointPosition.y,
                     y: acc.wayPointPosition.x,
                   }
                 | _ => acc.wayPointPosition
                 };
               },
             };
           | Forword => {
               ...acc,
               shipPosition: {
                 x: acc.shipPosition.x + inst.value * acc.wayPointPosition.x,
                 y: acc.shipPosition.y + inst.value * acc.wayPointPosition.y,
               },
             }
           }
         },
         {
           shipPosition: {
             x: 0,
             y: 0,
           },
           wayPointPosition: {
             x: 10,
             y: 1,
           },
         },
       )
    |> (({shipPosition: {x, y}}) => abs(x) + abs(y));
  "Part1:" ++ string_of_int(part1) ++ " Part2:" ++ string_of_int(part2);
};
