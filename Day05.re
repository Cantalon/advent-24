open Data;
open Helpers;

let raw = parseInput(data);
let (rules, updates) = List.partition(s => String.contains(s, '|'), raw);
// get rid of extra newline
let updates =
  List.map(
    x => List.map(int_of_string, String.split_on_char(',', x)),
    List.tl(updates),
  );

//stores rules in a tree map, int => list of successors
let rules_graph =
  List.fold_left(
    (im, str) =>
      switch (String.split_on_char('|', str)) {
      | [l, r] =>
        let (li, ri) = (int_of_string(l), int_of_string(r));
        switch (IntMap.find_opt(li, im)) {
        | None => IntMap.add(li, [ri], im)
        | Some(x) => IntMap.add(li, [ri, ...x], im)
        };
      | _ => failwith("unexpected error")
      },
    IntMap.empty,
    rules,
  );

/* Part 1 */
let rec process_print: (list(int), list(int)) => bool =
  (lst, prev) =>
    switch (lst) {
    | [] => true
    | [hd, ...tl] =>
      List.for_all(
        i =>
          switch (IntMap.find_opt(hd, rules_graph)) {
          | None => true
          | Some(x) => !List.mem(i, x)
          },
        prev,
      )
      && process_print(tl, [hd, ...prev])
    };

println(
  List.fold_left(
    (tot, lst) => tot + List.nth(lst, List.length(lst) / 2),
    0,
    List.filter(u => process_print(u, []), updates),
  ),
);

/* Part 2 */
let rec find_first: (int, list(int), list(int)) => list(int) =
  (hd, tl, prev) =>
    switch (tl) {
    | [] => [hd, ...prev]
    | [f, ...r] =>
      if (try(List.mem(hd, IntMap.find(f, rules_graph))) {
          | _ => false
          }) {
        // f comes before hd
        find_first(f, r, [hd, ...prev]);
      } else {
        find_first(hd, r, [f, ...prev]);
      }
    };

let rec sort: list(int) => list(int) =
  fun
  | [] => []
  | [hd, ...tl] => {
      let lst = find_first(hd, tl, []);
      [List.hd(lst), ...sort(List.tl(lst))];
    };

println(
  List.fold_left(
    (tot, lst) => tot + List.nth(sort(lst), List.length(lst) / 2),
    0,
    List.filter(u => !process_print(u, []), updates),
  ),
);
