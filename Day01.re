open Data;
open! Helpers;

let (ld, rd) =
  List.fold_left(
    (cur, str) => {
      let (lcur, rcur) = cur;
      (
        [List.nth(parseInts(str), 0), ...lcur],
        [List.nth(parseInts(str), 1), ...rcur],
      );
    },
    ([], []),
    parseInput(data),
  );

/* Part 1 */
println(
  List.fold_left2(
    (tot, a, b) => tot + abs(a - b),
    0,
    List.sort(Pervasives.compare, ld),
    List.sort(Pervasives.compare, rd),
  ),
);

/* Part 2 */
let (lmap, rmap) = (
  List.fold_left(
    (prev, elem) =>
      IntMap.update(
        elem,
        fun
        | None => Some(1)
        | Some(y) => Some(y + 1),
        prev,
      ),
    IntMap.empty,
    ld,
  ),
  List.fold_left(
    (prev, elem) =>
      IntMap.update(
        elem,
        fun
        | None => Some(1)
        | Some(y) => Some(y + 1),
        prev,
      ),
    IntMap.empty,
    rd,
  ),
);

let ans = ref(0);
IntMap.iter(
  (k, _) => {
    ans :=
      ans^
      + k
      * (
        switch (IntMap.find_opt(k, rmap)) {
        | None => 0
        | Some(x) => x
        }
      );
    ();
  },
  lmap,
);
println(ans^);
