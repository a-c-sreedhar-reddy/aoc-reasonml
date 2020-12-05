open Angstrom;
type line =
  | NewLine
  | PassPort(string);
let newLine = char('\n') *> return(NewLine);
let passPort =
  take_while(char => char != '\n')
  <* char('\n')
  >>= (s => return(PassPort(s)));
let parse =
  fix(parse => {
    fix(parse => {
      peek_char_fail
      >>= (
        c =>
          switch (c) {
          | '\n' => newLine
          | _ => passPort
          }
      )
    })
  });
let string = "eyr:2021 hgt:168cm hcl:#fffffd pid:180778832 byr:1923 ecl:amb iyr:2019 cid:241

hcl:#341e13 ecl:lzr eyr:2024 iyr:2014 pid:161cm byr:1991 cid:59 hgt:150cm

iyr:2018 eyr:2027
hgt:153cm
pid:642977294 ecl:gry hcl:#c0946f byr:1999

pid:#534f2e eyr:2022
ecl:amb cid:268
iyr:2028 hcl:2b079f
byr:2008
hgt:185cm
";
let run = () => {
  // Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/DayFour.txt");
  parse_string(
    ~consume=All,
    parse,
    string,
  );
};
