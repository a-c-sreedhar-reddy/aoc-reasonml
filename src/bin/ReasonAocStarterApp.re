open Containers;
open Library;

CLI.(
  program("aoc")
  |> version("1.0.0")
  |> description("Solve Advent of Code")
  |> argument("day", "Day of the AoC month", String)
  |> action(({log, args}) => {
       switch (args.string("day")) {
       | "day-15" => Console.log(Day15.run())
       | _ => Console.log("Not there yet")
       }
     })
  |> parseAndRun(Sys.argv)
);
