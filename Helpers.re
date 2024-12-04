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
    getChars(Bytes.of_string(str), String.length(str), []);
  };

/* data structures */

module IntMap =
  Map.Make({
    type t = int;
    let compare: (t, t) => int = (-);
  });
