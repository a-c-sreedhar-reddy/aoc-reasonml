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
       | "day-five" => Console.log(Day05.run())
       | "day-six" => Console.log(Day06.run())
       | "day-seven" => Console.log(Day07.run())
       | "day-eight" => Console.log(Day08.run())
       | t => Console.log("Not there yet")
       }
     })
  |> parseAndRun(Sys.argv)
);
