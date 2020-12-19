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
       | "day-nine" => Console.log(Day09.run())
       | "day-ten" => Console.log(Day10.run())
       | "day-11" => Console.log(Day11.run())
       | "day-12" => Console.log(Day12.run())
       | "day-13" => Console.log(Day13.run())
       | "day-14" => Console.log(Day14.run())
       | "day-15" => Console.log(Day15.run())
       | "day-16" => Console.log(Day16.run())
       | "day-18" => Console.log(Day18.run())

       | t => Console.log("Not there yet")
       }
     })
  |> parseAndRun(Sys.argv)
);
