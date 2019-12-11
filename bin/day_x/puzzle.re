open Lib;

// Boilerplate
Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Info));
Logs.set_reporter(Logs_fmt.reporter());

// Parse input
let parsed_input =
  Util.read_file("./bin/day_x/input") |> List.map(int_of_string);

// Puzzle 1
let puzzle_1 = 1;

Logs.info(m => m("Solution to day_xa: %d", puzzle_1));

// Puzzle 2
let puzzle_2 = 2;

Logs.info(m => m("Solution to day_xb: %d", puzzle_2));
