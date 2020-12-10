type previousAdapter =
  | Some(int)
  | None;
type acc = {
  previousAdapter: int,
  oneVoltDiffAdapterCount: int,
  threeVoltDiffAdapterCount: int,
  maxVolt: int,
};
let rec allPossible = (~inputJolt, ~adapters, ~maxOutput, ~memoed) => {
  switch (adapters) {
  | [] => 0
  | [oneAdapter] =>
    let res =
      maxOutput - oneAdapter <= 3 && oneAdapter - inputJolt <= 3 ? 1 : 0;
    Hashtbl.add(memoed, oneAdapter, res);
    res;
  | [firstAdapter, ...restAdapters] =>
    firstAdapter - inputJolt <= 3
      ? {
        let included =
          switch (memoed |> Hashtbl.find_opt(_, firstAdapter)) {
          | Some(included) => included
          | None =>
            let included =
              allPossible(
                ~inputJolt=firstAdapter,
                ~adapters=restAdapters,
                ~maxOutput,
                ~memoed,
              );
            Hashtbl.add(memoed, firstAdapter, included);
            included;
          };
        let excluded =
          allPossible(
            ~inputJolt,
            ~adapters=restAdapters,
            ~maxOutput,
            ~memoed,
          );
        included + excluded;
      }
      : 0
  };
};
let run = () => {
  let adapters =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day10.txt")
    |> List.map(int_of_string)
    |> List.sort((a, b) => a - b);
  let part1 =
    adapters
    |> List.fold_left(
         (acc, cAdapter) => {
           {
             previousAdapter: cAdapter,
             oneVoltDiffAdapterCount:
               cAdapter - acc.previousAdapter == 1
                 ? acc.oneVoltDiffAdapterCount + 1
                 : acc.oneVoltDiffAdapterCount,
             threeVoltDiffAdapterCount:
               cAdapter - acc.previousAdapter == 3
                 ? acc.threeVoltDiffAdapterCount + 1
                 : acc.threeVoltDiffAdapterCount,
             maxVolt: cAdapter,
           }
         },
         {
           previousAdapter: 0,
           oneVoltDiffAdapterCount: 0,
           threeVoltDiffAdapterCount: 0,
           maxVolt: 0,
         },
       )
    |> (
      acc => acc.oneVoltDiffAdapterCount * (acc.threeVoltDiffAdapterCount + 1)
    );
  let memoed =
    Hashtbl.create((adapters |> List.length) * (adapters |> List.length));

  let maxOutput =
    (adapters |> List.fold_left((_, adapter) => adapter, 0)) + 3;
  let part2 = allPossible(~inputJolt=0, ~adapters, ~maxOutput, ~memoed);
  "Part1: " ++ string_of_int(part1) ++ " part2: " ++ string_of_int(part2);
};
