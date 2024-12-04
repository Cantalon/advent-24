open Data;
open! Helpers;

let grid = List.map(charListOfString, parseInput(data));

let rec take: (list('a), int) => list('a) =
  (lst, k) =>
    switch (k) {
    | 0 => []
    | _ => [List.hd(lst), ...take(List.tl(lst), k - 1)]
    };

/* Part 1 */
let rec checkLst: (list(char), int, int) => int =
  (lst, state, tot) =>
    switch (state, lst) {
    | (_, []) => tot
    | (0, [hd, ...tl]) =>
      hd == 'X' ? checkLst(tl, 1, tot) : checkLst(tl, 0, tot)
    | (1, [hd, ...tl]) =>
      hd == 'M' ? checkLst(tl, 2, tot) : checkLst(lst, 0, tot)
    | (2, [hd, ...tl]) =>
      hd == 'A' ? checkLst(tl, 3, tot) : checkLst(lst, 0, tot)
    | (3, [hd, ...tl]) =>
      hd == 'S' ? checkLst(tl, 0, tot + 1) : checkLst(lst, 0, tot)
    | _ => failwith("unexpected error")
    };

let rec getRows: (list(list(char)), list(list(char))) => list(list(char)) =
  (mat, cur) =>
    switch (mat) {
    | [] => cur
    | [row, ...tl] =>
      getRows(tl, List.map2((elem, l) => [elem, ...l], row, cur))
    };

let rec getDias:
  (
    list(list(char)),
    list(list(char)),
    list(list(char)),
    list(list(char)),
    int
  ) =>
  list(list(char)) =
  (grid, up_dia, down_dia, cur, height) =>
    switch (grid) {
    | [] => cur @ up_dia @ down_dia
    | [row, ...tl] =>
      getDias(
        tl,
        [
          [List.hd(row)],
          ...List.map2(
               (elem, l) => [elem, ...l],
               List.tl(row),
               take(up_dia, height - 1),
             ),
        ],
        List.map2(
          (elem, l) => [elem, ...l],
          take(row, height - 1),
          List.tl(down_dia),
        )
        @ [[List.nth(row, height - 1)]],
        [List.nth(up_dia, height - 1), List.hd(down_dia), ...cur],
        height,
      )
    };

let emptyHeight = List.map(_ => [], List.hd(grid));

let allLsts =
  grid
  @ getRows(grid, emptyHeight)
  @ getDias(grid, emptyHeight, emptyHeight, [], List.length(emptyHeight));

println(
  List.fold_left(
    (tot, l) => tot + checkLst(l, 0, 0),
    0,
    allLsts @ List.map(List.rev, allLsts),
  ),
);

/* Part 2 */
let check3x3: (char, char, char, char, char) => bool =
  (c, e1, e2, e3, e4) => {
    let s =
      String.make(1, e1)
      ++ String.make(1, e2)
      ++ String.make(1, e3)
      ++ String.make(1, e4);
    c == 'A' && (s == "MMSS" || s == "SSMM" || s == "SMSM" || s == "MSMS");
  };

let rec checkRow: (list(char), list(char), list(char), int) => int =
  (r1, r2, r3, tot) =>
    switch (r1, r2, r3) {
    | ([a1, a2, a3, ...tla], [_, b2, b3, ...tlb], [c1, c2, c3, ...tlc]) =>
      checkRow(
        [a2, a3, ...tla],
        [b2, b3, ...tlb],
        [c2, c3, ...tlc],
        tot + (check3x3(b2, a1, a3, c1, c3) ? 1 : 0),
      )
    | _ => tot
    };

let rec checkGrid: (list(list(char)), int) => int =
  (mat, tot) =>
    switch (mat) {
    | [r1, r2, r3, ...tl] =>
      checkGrid([r2, r3, ...tl], checkRow(r1, r2, r3, tot))
    | _ => tot
    };

println(checkGrid(grid, 0));
