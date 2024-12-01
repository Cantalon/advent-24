open Data;

let (ld, rd) =
  List.fold_left(
    (cur, str) => {
      let (lcur, rcur) = cur;
      (
        [int_of_string(String.sub(str, 0, 5)), ...lcur],
        [int_of_string(String.sub(str, 8, 5)), ...rcur],
      );
    },
    ([], []),
    String.split_on_char('\n', data),
  );

/* Part 1 */
Js.log(
  List.fold_left2(
    (tot, a, b) => tot + abs(a - b),
    0,
    List.sort(Pervasives.compare, ld),
    List.sort(Pervasives.compare, rd),
  ),
);

/* Part 2 */
module IntMap =
  Map.Make({
    type t = int;
    let compare: (t, t) => int = (-);
  });

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
Js.log(ans);
