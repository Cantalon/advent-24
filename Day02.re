open Data;
open! Helpers;

let pd = List.map(parseInts, parseInput(data));

/* Part 1 */
let rec safe: (list(int), bool) => bool =
  (lst, inc) =>
    switch (lst) {
    | []
    | [_] => true
    | [hd1, hd2, ...tl] =>
      (inc && hd2 - hd1 >= 1 && hd2 - hd1 <= 3 || !inc && hd1 - hd2 >= 1 && hd1 - hd2 <= 3)
      && safe([hd2, ...tl], inc)
    };

println(
  List.length(List.filter(x => safe(x, true) || safe(x, false), pd)),
);

/* Part 2 */
let rec safeDamp: (list(int), list(int)) => bool =
  (lst, prev) =>
    switch (lst) {
    | [] => false
    | [hd, ...tl] =>
      safe(prev @ tl, true)
      || safe(prev @ tl, false)
      || safeDamp(tl, prev @ [hd])
    };

println(List.length(List.filter(x => safeDamp(x, []), pd)));
