let read_file = filename => {
  let chan = open_in(filename);
  let try_read = () =>
    try(Some(input_line(chan))) {
    | End_of_file => None
    };
  let rec loop = lines =>
    switch (try_read()) {
    | Some(s) => loop([s, ...lines])
    | None =>
      close_in(chan);
      List.rev(lines);
    };

  loop([]);
};

let explode = s => {
  let rec exp = (i, l) =>
    if (i < 0) {
      l;
    } else {
      exp(i - 1, [s.[i], ...l]);
    };
  exp(String.length(s) - 1, []);
};

let string_of_chars = chars => {
  let buf = Buffer.create(16);
  List.iter(Buffer.add_char(buf), chars);
  Buffer.contents(buf);
};

let cons_uniq = (tl, hd) =>
  if (List.mem(hd, tl)) {
    tl;
  } else {
    [hd, ...tl];
  };

let remove_duplicates = xs => List.rev(List.fold_left(cons_uniq, [], xs));
