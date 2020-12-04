open Containers;
open Library;

CLI.(
  program("aoc")
  |> version("1.0.0")
  |> description("Solve Advent of Code")
  |> argument("day", "Day of the AoC month", String)
  |> action(({log, args}) => {
       switch (args.string("day")) {
       | "day-one" => Console.log(DayOne.run())
       | "day-two" => Console.log(DayTwo.run())
       | "day-three" => Console.log(DayThree.run())
       | t => Console.log("Not there yet")
       }
     })
  |> parseAndRun(Sys.argv)
);
