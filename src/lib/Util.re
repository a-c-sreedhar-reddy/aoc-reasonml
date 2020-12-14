module Fs = {
  let readLines = path => Fs.readTextExn(Fp.absoluteExn(path));
};

let logicalXOR = (x, y) =>
  switch (x, y) {
  | (true, true) => false
  | (true, false) => true
  | (false, true) => true
  | (false, false) => false
  };

/* Logical XOR */
let (<<>>) = logicalXOR;

let rec gcd_ext = (a, b) =>
  switch (b) {
  | 0 => (1, 0, a)
  | b =>
    let (s, t, g) = gcd_ext(b, a mod b);
    (t, s - a / b * t, g);
  };
let mod_inv = (a, m) => {
  let mk_pos = x => x < 0 ? x + m : x;
  switch (gcd_ext(a, m)) {
  | (i, _, 1) => mk_pos(i)
  | _ => failwith("mod_inv")
  };
};

let isDigit = a =>
  switch (a) {
  | '0' .. '9' => true
  | _ => false
  };
