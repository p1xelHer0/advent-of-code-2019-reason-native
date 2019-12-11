open Lib;

// Boilerplate
Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Info));
Logs.set_reporter(Logs_fmt.reporter());

// Parse input
let parsed_input =
  Util.read_file("./bin/day_1/input") |> List.map(int_of_string);

// Puzzle 1
let fuel_required = mass => mass / 3 - 2;

let puzzle_1 =
  parsed_input |> List.map(fuel_required) |> List.fold_left((+), 0);

Logs.info(m => m("Solution to day1a: %d", puzzle_1));

// Puzzle 2
let rec added_fuel_required = (mass, fuel) =>
  switch (fuel_required(mass)) {
  | next_fuel when next_fuel <= 0 => fuel
  | next_fuel => added_fuel_required(next_fuel, fuel + next_fuel)
  };

let solve_puzzle_2 = mass => added_fuel_required(mass, 0);

let puzzle_2 =
  parsed_input |> List.map(solve_puzzle_2) |> List.fold_left((+), 0);

Logs.info(m => m("Solution to day1b: %d", puzzle_2));
