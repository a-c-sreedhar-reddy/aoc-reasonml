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
           take_while(c => c == ' ')
           *> lift3(
                (key, colon, value) => (key, value),
                parseKey,
                char(':'),
                take_while(c => c != ' '),
              )
           <* take_while(c => c != ' '),
         ),
         passPort,
       )
     });
let run = () => {
  let part1 =
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
    |> List.filter(keyvalues => {
         [Eyr, Pid, Ecl, Hcl, Byr, Iyr, Hgt]
         |> List.for_all(key =>
              keyvalues |> List.exists(((ckey, value)) => ckey == key)
            )
       })
    |> List.length;
  let part2 =
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
    |> List.filter(keyvalues => {
         [Eyr, Pid, Ecl, Hcl, Byr, Iyr, Hgt]
         |> List.for_all(key =>
              keyvalues |> List.exists(((ckey, value)) => ckey == key)
            )
       })
    |> List.filter(keyvalues => {
         keyvalues
         |> List.for_all(((key, value)) =>
              switch (key) {
              | Byr =>
                switch (
                  parse_string(
                    ~consume=All,
                    take_while(c => {
                      switch (c) {
                      | '0' .. '9' => true
                      | _ => false
                      }
                    }),
                    value,
                  )
                ) {
                | Ok(byr) =>
                  byr
                  |> String.length == 4
                  && int_of_string(byr) >= 1920
                  && int_of_string(byr) <= 2002
                | _ => false
                }
              | Eyr =>
                switch (
                  parse_string(
                    ~consume=All,
                    take_while(c => {
                      switch (c) {
                      | '0' .. '9' => true
                      | _ => false
                      }
                    }),
                    value,
                  )
                ) {
                | Ok(byr) =>
                  byr
                  |> String.length == 4
                  && int_of_string(byr) >= 2020
                  && int_of_string(byr) <= 2030
                | _ => false
                }
              | Iyr =>
                switch (
                  parse_string(
                    ~consume=All,
                    take_while(c => {
                      switch (c) {
                      | '0' .. '9' => true
                      | _ => false
                      }
                    }),
                    value,
                  )
                ) {
                | Ok(byr) =>
                  byr
                  |> String.length == 4
                  && int_of_string(byr) >= 2010
                  && int_of_string(byr) <= 2020
                | _ => false
                }
              | Hcl =>
                switch (
                  parse_string(
                    ~consume=All,
                    lift2(
                      (hash, value) => value,
                      char('#'),
                      take_while(c => {
                        switch (c) {
                        | '0' .. '9' => true
                        | 'a' .. 'f' => true
                        | _ => false
                        }
                      }),
                    ),
                    value,
                  )
                ) {
                | Ok(hcl) => hcl |> String.length == 6
                | _ => false
                }
              | Pid =>
                switch (
                  parse_string(
                    ~consume=All,
                    take_while(c => {
                      switch (c) {
                      | '0' .. '9' => true
                      | _ => false
                      }
                    }),
                    value,
                  )
                ) {
                | Ok(hcl) => hcl |> String.length == 9
                | _ => false
                }
              | Ecl =>
                switch (
                  parse_string(
                    ~consume=All,
                    string("amb")
                    <|> string("blu")
                    <|> string("brn")
                    <|> string("gry")
                    <|> string("grn")
                    <|> string("hzl")
                    <|> string("oth"),
                    value,
                  )
                ) {
                | Ok(_) => true
                | _ => false
                }
              | Hgt =>
                switch (
                  parse_string(
                    ~consume=All,
                    lift2(
                      (measurement, unit) => (measurement, unit),
                      take_while(c =>
                        switch (c) {
                        | '0' .. '9' => true
                        | _ => false
                        }
                      )
                      >>| (x => int_of_string(x)),
                      string("cm") <|> string("in"),
                    ),
                    value,
                  )
                ) {
                | Ok((measurement, unit)) =>
                  switch (unit) {
                  | "cm" => 150 <= measurement && measurement <= 193
                  | "in" => 59 <= measurement && measurement <= 76
                  | _ => false
                  }
                | _ => false
                }
              | Cid => true
              }
            )
       })
    |> List.length;
  "part1:" ++ string_of_int(part1) ++ " part2:" ++ string_of_int(part2);
};
