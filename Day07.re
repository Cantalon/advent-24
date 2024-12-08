open Data;
open Helpers;

open Int64;

let processed =
  List.map(
    str => {
      let lst = String.split_on_char(':', str);
      [
        of_string(List.nth(lst, 0)),
        ...List.map(of_int, parseInts(List.nth(lst, 1))),
      ];
    },
    parseInput(data),
  );

/* Part 1 */
let rec possible: (int64, list(int64), int64) => bool =
  (target, l, cur) =>
    switch (l) {
    | [] => cur == target
    | [hd, ...tl] =>
      if (cur > target) {
        false;
      } else {
        possible(target, tl, add(cur, hd))
        || possible(target, tl, mul(cur, hd));
      }
    };

let ans1 =
  List.fold_left(
    (tot, l) =>
      add(
        tot,
        possible(List.hd(l), drop(2, l), List.nth(l, 1))
          ? List.hd(l) : zero,
      ),
    zero,
    processed,
  );

println(to_string(ans1));

/* Part 2 */
let rec possible3: (int64, list(int64), int64) => bool =
  (target, l, cur) =>
    switch (l) {
    | [] => cur == target
    | [hd, ...tl] =>
      if (cur > target) {
        false;
      } else {
        possible3(target, tl, add(cur, hd))
        || possible3(target, tl, mul(cur, hd))
        || possible3(
             target,
             tl,
             of_string(to_string(cur) ++ to_string(hd)),
           );
      }
    };

let filtered =
  List.filter(
    l => !possible(List.hd(l), drop(2, l), List.nth(l, 1)),
    processed,
  );

let ans2 =
  List.fold_left(
    (tot, l) =>
      add(
        tot,
        possible3(List.hd(l), drop(2, l), List.nth(l, 1))
          ? List.hd(l) : zero,
      ),
    zero,
    filtered,
  );

println(to_string(add(ans1, ans2)));
