/* I/O nicer function names */

let parseInput: string => list(string) = String.split_on_char('\n');

let parseLine: string => list(string) =
  str => List.filter(sub => sub != "", String.split_on_char(' ', str));

let parseInts: string => list(int) =
  str => List.map(int_of_string, parseLine(str));

let println: 'a => unit = Js.log;

/* remedial string helpers */

let charListOfString: string => list(char) =
  str => {
    let rec getChars: (bytes, int, list(char)) => list(char) =
      (b, depth, l) =>
        if (depth == (-1)) {
          l;
        } else {
          getChars(b, depth - 1, [Bytes.get(b, depth), ...l]);
        };
    getChars(Bytes.of_string(str), String.length(str) - 1, []);
  };

/* list helpers */
let rec take: (int, list('a)) => list('a) =
  (k, l) =>
    switch (k, l) {
    | (0, _) => []
    | (_, [hd, ...tl]) => [hd, ...take(k - 1, tl)]
    | _ => failwith("take: improper input")
    };

let rec drop: (int, list('a)) => list('a) =
  (k, l) =>
    switch (k, l) {
    | (0, _) => l
    | (_, [_hd, ...tl]) => drop(k - 1, tl)
    | _ => failwith("drop: improper input")
    };

let getIndex: ('a, list('a)) => int =
  (i, lst) => {
    let rec getIndexHelper: ('a, list('a), int) => int =
      (elem, l, index) =>
        switch (l) {
        | [] => failwith("getIndex: not found")
        | [hd, ...tl] =>
          if (hd == elem) {
            index;
          } else {
            getIndexHelper(elem, tl, index + 1);
          }
        };
    getIndexHelper(i, lst, 0);
  };

/* data structures */

module IntMap =
  Map.Make({
    type t = int;
    let compare: (t, t) => int = (-);
  });

module IntPairsSet =
  Set.Make({
    type t = (int, int);
    let compare = ((x0, y0), (x1, y1)) =>
      compare(10000 * x0 + y0, 10000 * x1 + y1);
  });
