open Data;
open! Helpers;

let rec drop: (list(char), int) => list(char) =
  (lst, k) =>
    switch (k) {
    | 0 => lst
    | n when n > 0 => drop(List.tl(lst), n - 1)
    | _ => failwith("unexpected error")
    };

let rec startsWith: (list(char), list(char)) => bool =
  (lst, comp) =>
    switch (lst, comp) {
    | (_, []) => true
    | ([hd, ...tl], [hd2, ...tl2]) => hd == hd2 && startsWith(tl, tl2)
    | _ => false
    };

let rec getNumber: (list(char), int) => (int, list(char)) =
  (lst, prev) =>
    switch (lst) {
    | [] => (0, [])
    | [c, ...tl] =>
      if (0 <= Char.code(c) - 48 && Char.code(c) - 48 < 10) {
        getNumber(tl, prev * 10 + Char.code(c) - 48);
      } else {
        (prev, lst);
      }
    };

/* Part 1 */
let rec compute: (list(char), int, int) => int =
  (lst, state, tot) =>
    switch (state) {
    | 0 =>
      if (List.length(lst) < 8) {
        tot;
      } else if (startsWith(lst, ['m', 'u', 'l', '('])) {
        compute(drop(lst, 4), 1, tot);
      } else {
        compute(drop(lst, 1), 0, tot);
      }
    | 1 =>
      let (int, next) = getNumber(lst, 0);
      if (List.length(lst) < 4) {
        tot;
      } else if (int != 0 && startsWith(next, [','])) {
        let (int2, next2) = getNumber(drop(next, 1), 0);
        if (int2 != 0 && startsWith(next2, [')'])) {
          compute(drop(next2, 1), 0, tot + int * int2);
        } else {
          compute(drop(next, 1), 0, tot);
        };
      } else {
        compute(next, 0, tot);
      };
    | _ => failwith("unexpected error")
    };

println(compute(charListOfString(data), 0, 0));

/* Part 2 */
let rec compute2: (list(char), int, int) => int =
  (lst, state, tot) =>
    switch (state) {
    | 0 =>
      if (List.length(lst) < 8) {
        tot;
      } else if (startsWith(
                   lst,
                   ['d', 'o', 'n', Char.chr(39), 't', '(', ')'],
                 )) {
        compute2(drop(lst, 7), 2, tot);
      } else if (startsWith(lst, ['m', 'u', 'l', '('])) {
        compute2(drop(lst, 4), 1, tot);
      } else {
        compute2(drop(lst, 1), 0, tot);
      }
    | 1 =>
      let (int, next) = getNumber(lst, 0);
      if (List.length(lst) < 4) {
        tot;
      } else if (int != 0 && startsWith(next, [','])) {
        let (int2, next2) = getNumber(drop(next, 1), 0);
        if (int2 != 0 && startsWith(next2, [')'])) {
          compute2(drop(next2, 1), 0, tot + int * int2);
        } else {
          compute2(drop(next, 1), 0, tot);
        };
      } else {
        compute2(next, 0, tot);
      };
    | 2 =>
      if (List.length(lst) < 4) {
        tot;
      } else if (startsWith(lst, ['d', 'o', '(', ')'])) {
        compute2(drop(lst, 4), 0, tot);
      } else {
        compute2(drop(lst, 1), 2, tot);
      }
    | _ => failwith("unexpected error")
    };

println(compute2(charListOfString(data), 0, 0));
