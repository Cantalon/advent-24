open Data;
open Helpers;

let grid = List.map(charListOfString, parseInput(data));

let boardWidth = List.length(List.hd(grid));
let boardHeight = List.length(grid);

let get: (list(list(char)), int, int) => char =
  (cur_grid, row, col) =>
    if (row < 0 || boardWidth <= row || col < 0 || boardHeight <= col) {
      ' ';
    } else {
      List.nth(List.nth(cur_grid, row), col);
    };

/* Part 1 */
// right, up, left, down
let del_row = [0, (-1), 0, 1];
let del_col = [1, 0, (-1), 0];

let rec next_state:
  (int, int, int, Test.Helpers.IntPairsSet.t) => Test.Helpers.IntPairsSet.t =
  (state, row, col, seen) =>
    if (row < 0 || boardWidth <= row || col < 0 || boardHeight <= col) {
      //exited the map
      seen;
    } else {
      let next_row = row + List.nth(del_row, state);
      let next_col = col + List.nth(del_col, state);
      if (get(grid, next_row, next_col) == '#') {
        next_state((state + 3) mod 4, row, col, seen);
      } else {
        next_state(
          state,
          next_row,
          next_col,
          IntPairsSet.add((row, col), seen),
        );
      };
    };

let rec find_col: (list(list(char)), int) => (int, int) =
  (mat, index) =>
    switch (mat) {
    | [] => ((-1), (-1))
    | [hd, ...tl] =>
      if (List.mem('^', hd)) {
        (index, getIndex('^', hd));
      } else {
        find_col(tl, index + 1);
      }
    };

let (a, b) = find_col(grid, 0);
let computed = next_state(1, a, b, IntPairsSet.empty);

println(IntPairsSet.fold((_, tot) => tot + 1, computed, 0));

/* Part 2 */
module StateSet =
  Set.Make({
    // state, row, col
    type t = (int, int, int);
    let compare = ((s0, x0, y0), (s1, x1, y1)) =>
      compare(s0 + 4 * x0 + 40000 * y0, s1 + 4 * x1 + 40000 * y1);
  });

let replace: (list(list(char)), int, int, char) => list(list(char)) =
  (mat, row, col, ch) => {
    let cur_row = List.nth(mat, row);
    take(row, mat)
    @ [take(col, cur_row) @ [ch] @ drop(col + 1, cur_row)]
    @ drop(row + 1, mat);
  };

let rec loopsP: (list(list(char)), int, int, int, StateSet.t) => bool =
  (g, state, row, col, seen) =>
    if (row < 0 || boardWidth <= row || col < 0 || boardHeight <= col) {
      false;
    } else if (StateSet.mem((state, row, col), seen)) {
      true;
    } else {
      let next_row = row + List.nth(del_row, state);
      let next_col = col + List.nth(del_col, state);
      if (get(g, next_row, next_col) == '#') {
        loopsP(g, (state + 3) mod 4, row, col, seen);
      } else {
        loopsP(
          g,
          state,
          next_row,
          next_col,
          StateSet.add((state, row, col), seen),
        );
      };
    };

let ans = ref(0);

for (r in 0 to boardHeight - 1) {
  for (c in 0 to boardWidth - 1) {
    ans :=
      loopsP(replace(grid, r, c, '#'), 1, a, b, StateSet.empty)
        ? ans^ + 1 : ans^;
  };
};
println(ans^);
