open Angstrom;
type status =
  | Floor
  | Occupied
  | UnOccupied;
type fill =
  | Sit
  | Leave;
type direction =
  | Top
  | Bottom
  | Left
  | Right
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight;
let getEmptyseatsInRow = row => {
  Array.fold_left(
    (count, seat) => {
      switch (seat) {
      | Occupied => count + 1
      | _ => count
      }
    },
    0,
    row,
  );
};
let getEmptySeats = (~seats) => {
  Array.fold_left(
    (count, row) => count + getEmptyseatsInRow(row),
    0,
    seats,
  );
};
let safeGet = (array, x, y) => {
  switch (array[y][x]) {
  | a => Some(a)
  | exception _ => None
  };
};
let rec getSeatInDirection = (seats, x, y, direction) => {
  switch (safeGet(seats, x, y)) {
  | Some(seat) =>
    seat != Floor
      ? Some(seat)
      : getSeatInDirection(
          seats,
          switch (direction) {
          | Top => x
          | Bottom => x
          | Left => x - 1
          | Right => x + 1
          | TopLeft => x - 1
          | TopRight => x + 1
          | BottomLeft => x - 1
          | BottomRight => x + 1
          },
          switch (direction) {
          | Left => y
          | Right => y
          | Top => y + 1
          | Bottom => y - 1
          | TopLeft => y + 1
          | TopRight => y + 1
          | BottomLeft => y - 1
          | BottomRight => y - 1
          },
          direction,
        )
  | None => None
  };
};
let getType2AdjacentList = (seats, x, y) => {
  let getSeatInDirection = getSeatInDirection(seats);
  [
    getSeatInDirection(x, y + 1, Top),
    getSeatInDirection(x + 1, y + 1, TopRight),
    getSeatInDirection(x + 1, y, Right),
    getSeatInDirection(x + 1, y - 1, BottomRight),
    getSeatInDirection(x, y - 1, Bottom),
    getSeatInDirection(x - 1, y - 1, BottomLeft),
    getSeatInDirection(x - 1, y, Left),
    getSeatInDirection(x - 1, y + 1, TopLeft),
  ];
};
let canSit = (seats, x, y) => {
  switch (seats[y][x]) {
  | UnOccupied =>
    let adjacent_seats = [
      safeGet(seats, x - 1, y + 1),
      safeGet(seats, x, y + 1),
      safeGet(seats, x + 1, y + 1),
      safeGet(seats, x + 1, y),
      safeGet(seats, x + 1, y - 1),
      safeGet(seats, x, y - 1),
      safeGet(seats, x - 1, y - 1),
      safeGet(seats, x - 1, y),
    ];
    List.for_all(
      seat =>
        switch (seat) {
        | Some(seat) => seat != Occupied
        | None => true
        },
      adjacent_seats,
    );

  | _ => false
  };
};
let canSit2 = (seats, x, y) => {
  switch (seats[y][x]) {
  | UnOccupied =>
    let adjacent_seats = getType2AdjacentList(seats, x, y);
    List.for_all(
      seat =>
        switch (seat) {
        | Some(seat) => seat != Occupied
        | None => true
        },
      adjacent_seats,
    );

  | _ => false
  };
};
let sit = (~seats) => {
  let changed = ref(false);
  let seats =
    Array.mapi(
      (y, row) => {
        Array.mapi(
          (x, seat) =>
            switch (canSit(seats, x, y)) {
            | true =>
              changed := seats[y][x] != Occupied;
              Occupied;
            | _ => seat
            },
          row,
        )
      },
      seats,
    );
  (seats, changed^);
};
let sit2 = (~seats) => {
  let changed = ref(false);
  let seats =
    Array.mapi(
      (y, row) => {
        Array.mapi(
          (x, seat) =>
            switch (canSit2(seats, x, y)) {
            | true =>
              changed := seats[y][x] != Occupied;
              Occupied;
            | _ => seat
            },
          row,
        )
      },
      seats,
    );
  (seats, changed^);
};
let canLeave = (seats, x, y) => {
  switch (seats[y][x]) {
  | Occupied =>
    [
      safeGet(seats, x - 1, y + 1),
      safeGet(seats, x, y + 1),
      safeGet(seats, x + 1, y + 1),
      safeGet(seats, x + 1, y),
      safeGet(seats, x + 1, y - 1),
      safeGet(seats, x, y - 1),
      safeGet(seats, x - 1, y - 1),
      safeGet(seats, x - 1, y),
    ]
    |> List.filter(a => a != None)
    |> List.map(a =>
         switch (a) {
         | Some(a) => a
         | None => failwith("can not be done")
         }
       )
    |> List.filter(seat => seat == Occupied)
    |> List.length >= 4
  | _ => false
  };
};
let canLeave2 = (seats, x, y) => {
  switch (seats[y][x]) {
  | Occupied =>
    getType2AdjacentList(seats, x, y)
    |> List.filter(a => a != None)
    |> List.map(a =>
         switch (a) {
         | Some(a) => a
         | None => failwith("can not be done")
         }
       )
    |> List.filter(seat => seat == Occupied)
    |> List.length >= 5
  | _ => false
  };
};
let leave2 = (~seats) => {
  let changed = ref(false);
  let seats =
    Array.mapi(
      (y, row) => {
        Array.mapi(
          (x, seat) =>
            switch (canLeave2(seats, x, y)) {
            | true =>
              changed := seats[y][x] != UnOccupied;
              UnOccupied;
            | _ => seat
            },
          row,
        )
      },
      seats,
    );
  (seats, changed^);
};
let leave = (~seats) => {
  let changed = ref(false);
  let seats =
    Array.mapi(
      (y, row) => {
        Array.mapi(
          (x, seat) =>
            switch (canLeave(seats, x, y)) {
            | true =>
              changed := seats[y][x] != UnOccupied;

              UnOccupied;
            | _ => seat
            },
          row,
        )
      },
      seats,
    );
  (seats, changed^);
};
let rec calmDown = (~seats, ~fill) => {
  let (seats, changed) =
    switch (fill) {
    | Sit => sit(~seats)
    | Leave => leave(~seats)
    };
  changed ? calmDown(~seats, ~fill=fill == Sit ? Leave : Sit) : seats;
};
let rec calmDown2 = (~seats, ~fill) => {
  let (seats, changed) =
    switch (fill) {
    | Sit => sit2(~seats)
    | Leave => leave2(~seats)
    };
  changed ? calmDown2(~seats, ~fill=fill == Sit ? Leave : Sit) : seats;
};
let run = () => {
  let parse =
    fix(parse => {
      let floorLift =
        lift2((a, b) => [a, ...b], char('.') *> return(Floor), parse);
      let unOccupiedLift =
        lift2(
          (a, b) => [a, ...b],
          char('L') *> return(UnOccupied),
          parse,
        );
      peek_char
      >>= (
        char => {
          switch (char) {
          | Some('L') => unOccupiedLift
          | Some('.') => floorLift
          | _ => return([])
          };
        }
      );
    });
  let seats =
    Util.Fs.readLines("/home/a-c-sreedhar-reddy/aoc/src/lib/Day11.txt")
    |> List.map(row => parse_string(~consume=All, parse, row))
    |> List.map(row =>
         switch (row) {
         | Ok(row) => row |> Array.of_list
         | Error(e) => failwith(e)
         }
       )
    |> Array.of_list;
  let part1 = getEmptySeats(calmDown(~seats, ~fill=Sit));
  let part2 = getEmptySeats(calmDown2(~seats, ~fill=Sit));
  "Part1 : " ++ string_of_int(part1) ++ " Part2:" ++ string_of_int(part2);
};
