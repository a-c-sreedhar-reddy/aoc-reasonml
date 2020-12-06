open Angstrom;
type key =
  | Eyr
  | Pid
  | Ecl
  | Hcl
  | Byr
  | Iyr
  | Cid
  | Hgt;
let eyr = string("eyr") *> return(Eyr);
let cid = string("cid") *> return(Cid);
let hcl = string("hcl") *> return(Hcl);
let iyr = string("iyr") *> return(Iyr);
let byr = string("byr") *> return(Byr);
let ecl = string("ecl") *> return(Ecl);
let pid = string("pid") *> return(Pid);
let hgt = string("hgt") *> return(Hgt);
let parseKey = eyr <|> hgt <|> pid <|> ecl <|> hcl <|> byr <|> iyr <|> cid;
let passportKeys =
  Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/DayFour.txt")
  |> List.fold_left(
       ((accResult, accString), current) => {
         switch (current) {
         | "" => ([accString, ...accResult], "")
         | _ => (accResult, accString ++ " " ++ current)
         }
       },
       ([], ""),
     )
  |> (((l, s)) => [s, ...l])
  |> List.map(passPort => {
       parse_string(
         ~consume=All,
         sep_by(
           char(' '),
           take_while(c => c == ' ') *> parseKey <* take_while(c => c != ' '),
         ),
         passPort,
       )
     });
let run = () => {
  passportKeys
  |> List.filter(passport =>
       switch (passport) {
       | Ok(result) => true
       | Error(_) => false
       }
     )
  |> List.map(passPort =>
       switch (passPort) {
       | Ok(result) => result
       | Error(_) => failwith("faie")
       }
     )
  |> List.filter(keys => {
       [Eyr, Pid, Ecl, Hcl, Byr, Iyr, Hgt]
       |> List.for_all(key => keys |> List.exists(ckey => ckey == key))
     })
  |> List.length;
};
