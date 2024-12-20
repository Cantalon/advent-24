open Data;
open Helpers;

let grid = List.map(charListOfString, parseInput(data));

let g_height = List.length(grid);
let g_width = List.length(List.hd(grid));

/* use IntPairsSet to store the list of node positions, then filter */
/* use a map: char => list((int, int)) to store all positions with the same node */

/* Part 1 */
let rec scan_lst:
  (list(char), int, int, CharMap.t(list((int, int)))) =>
  CharMap.t(list((int, int))) =
  (lst, row, col, nodes) => {
    switch (lst) {
    | [] => nodes
    | [hd, ...tl] =>
      scan_lst(
        tl,
        row,
        col + 1,
        if (hd == '.') {
          nodes;
        } else {
          switch (CharMap.find_opt(hd, nodes)) {
          | None => CharMap.add(hd, [(row, col)], nodes)
          | Some(x) => CharMap.add(hd, [(row, col), ...x], nodes)
          };
        },
      )
    };
  };

let rec scan_grid:
  (list(list(char)), int, CharMap.t(list((int, int)))) =>
  CharMap.t(list((int, int))) =
  (mat, row, nodes) =>
    switch (mat) {
    | [] => nodes
    | [hd, ...tl] => scan_grid(tl, row + 1, scan_lst(hd, row, 0, nodes))
    };

let nodes =
  List.map(
    x => {
      let (_, p) = x;
      p;
    },
    CharMap.bindings(scan_grid(grid, 0, CharMap.empty)),
  );

let rec antinodes_from_single:
  ((int, int), list((int, int)), IntPairsSet.t) => IntPairsSet.t =
  (pnt, lst, antinodes) =>
    switch (pnt, lst) {
    | (_, []) => antinodes
    | ((x, y), [(x2, y2), ...tl]) =>
      antinodes_from_single(
        pnt,
        tl,
        IntPairsSet.add(
          (2 * x2 - x, 2 * y2 - y),
          IntPairsSet.add((2 * x - x2, 2 * y - y2), antinodes),
        ),
      )
    };

let rec antinodes_from_list:
  (list((int, int)), IntPairsSet.t) => IntPairsSet.t =
  (lst, antinodes) =>
    switch (lst) {
    | [] => antinodes
    | [hd, ...tl] =>
      antinodes_from_list(tl, antinodes_from_single(hd, tl, antinodes))
    };

let antinodes =
  List.fold_left(
    (set, lst) => antinodes_from_list(lst, set),
    IntPairsSet.empty,
    nodes,
  );

let in_bounds: (int, int) => bool =
  (row, col) => 0 <= row && row < g_height && 0 <= col && col < g_width;

println(
  IntPairsSet.fold(
    (pnt, tot) => {
      let (a, b) = pnt;
      tot + (in_bounds(a, b) ? 1 : 0);
    },
    antinodes,
    0,
  ),
);

let rec traverse: (int, int, int, int, IntPairsSet.t) => IntPairsSet.t =
  (row, col, delrow, delcol, set) =>
    in_bounds(row, col)
      ? traverse(
          row + delrow,
          col + delcol,
          delrow,
          delcol,
          IntPairsSet.add((row, col), set),
        )
      : set;

/* Part 2 */
let rec antinodes_from_single2:
  ((int, int), list((int, int)), IntPairsSet.t) => IntPairsSet.t =
  (pnt, lst, antinodes) =>
    switch (pnt, lst) {
    | (_, []) => antinodes
    | ((x, y), [(x2, y2), ...tl]) =>
      let del_x = (x2 - x) / gcd(x2 - x, y2 - y);
      let del_y = (y2 - y) / gcd(x2 - x, y2 - y);
      antinodes_from_single2(
        pnt,
        tl,
        traverse(
          x,
          y,
          - del_x,
          - del_y,
          traverse(x, y, del_x, del_y, antinodes),
        ),
      );
    };

let rec antinodes_from_list2:
  (list((int, int)), IntPairsSet.t) => IntPairsSet.t =
  (lst, antinodes) =>
    switch (lst) {
    | [] => antinodes
    | [hd, ...tl] =>
      antinodes_from_list2(tl, antinodes_from_single2(hd, tl, antinodes))
    };

let antinodes2 =
  List.fold_left(
    (set, lst) => antinodes_from_list2(lst, set),
    IntPairsSet.empty,
    nodes,
  );

println(IntPairsSet.fold((_, tot) => tot + 1, antinodes2, 0));
