let canMove = (map, currentPosition, slope) => {
  let (_, y) = currentPosition;
  let (_, slopy) = slope;
  y + slopy < Array.length(map);
};
let move = (map, currentPosition, slope) => {
  let (x, y) = currentPosition;
  let (slopex, slopeY) = slope;
  let newX = (x + slopex) mod (map[y] |> String.length);
  (newX, y + slopeY);
};
let treeAtPosition = (map, (x, y)) => {
  map[y].[x] == '#';
};
let rec getTrees = (map, currentPosition, slope) => {
  canMove(map, currentPosition, slope)
    ? {
      let newPosition = move(map, currentPosition, slope);
      treeAtPosition(map, newPosition)
        ? 1 + getTrees(map, newPosition, slope)
        : getTrees(map, newPosition, slope);
    }
    : 0;
};
let run = () => {
  let slope = (3, 1);
  let map =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/DayThree.txt")
    |> Array.of_list;
  let part1 = getTrees(map, (0, 0), slope);
  let getTreesForSlope = getTrees(map, (0, 0));
  let part2 =
    getTreesForSlope((1, 1))
    * getTreesForSlope((3, 1))
    * getTreesForSlope((5, 1))
    * getTreesForSlope((7, 1))
    * getTreesForSlope((1, 2));
  "Part1:" ++ string_of_int(part1) ++ " Part2:" ++ string_of_int(part2);
};
