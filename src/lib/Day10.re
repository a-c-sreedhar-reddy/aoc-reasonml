type previousAdapter =
  | Some(int)
  | None;
type acc = {
  previousAdapter: int,
  oneVoltDiffAdapterCount: int,
  threeVoltDiffAdapterCount: int,
};
let run = () => {
  let adapters =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day10.txt")
    |> List.map(int_of_string)
    |> List.sort((a, b) => a - b);
  adapters
  |> List.fold_left(
       (acc, cAdapter) => {
         {
           previousAdapter: cAdapter,
           oneVoltDiffAdapterCount:
             cAdapter - acc.previousAdapter == 1
               ? acc.oneVoltDiffAdapterCount + 1 : acc.oneVoltDiffAdapterCount,
           threeVoltDiffAdapterCount:
             cAdapter - acc.previousAdapter == 3
               ? acc.threeVoltDiffAdapterCount + 1
               : acc.threeVoltDiffAdapterCount,
         }
       },
       {
         previousAdapter: 0,
         oneVoltDiffAdapterCount: 0,
         threeVoltDiffAdapterCount: 0,
       },
     )
  |> (acc => acc.oneVoltDiffAdapterCount * (acc.threeVoltDiffAdapterCount + 1));
};
