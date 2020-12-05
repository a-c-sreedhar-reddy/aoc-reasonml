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
       | "day-four" => Console.log(DayFour.run())
       | t => Console.log("Not there yet")
       }
     })
  |> parseAndRun(Sys.argv)
);
