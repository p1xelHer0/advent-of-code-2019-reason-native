open Lib;

// Boilerplate
Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Info));
Logs.set_reporter(Logs_fmt.reporter());

// Parse input
let parsed_input =
  Util.read_file("./bin/day_2/input")
  |> List.hd
  |> Str.split(Str.regexp(","))
  |> List.filter(s => s != ",")
  |> List.map(int_of_string)
  |> Array.of_list;

// Puzzle 1
type opcode =
  | Addition
  | Product
  | Halt
  | Error;

let read_opcode = x =>
  switch (x) {
  | 1 => Addition
  | 2 => Product
  | 99 => Halt
  | _ => Error
  };

let computer_thing = (noun, verb, program) => {
  /* [..] adds together numbers read from two positions
     and stores the result in a third position. */
  let make_instructions = (pointer, array) => {
    /* [..] the first two indicate the positions from .. */
    let position_to_read_1 = array[pointer + 1];
    let position_to_read_2 = array[pointer + 2];
    /* ... which you should read the input values */
    let value_1 = array[position_to_read_1];
    let value_2 = array[position_to_read_2];

    /* [..] and the third indicates the position at
       which the output should be stored */
    let position = array[pointer + 3];
    (value_1, value_2, position);
  };

  let process_opcode = (pointer, operation, array) => {
    let (value_1, value_2, new_position) = make_instructions(pointer, array);
    let value = operation(value_1, value_2);

    let new_array = Array.copy(array);
    new_array[new_position] = value;
    new_array;
  };

  let rec run = (pointer, array) =>
    switch (read_opcode(array[pointer])) {
    | Addition => run(pointer + 4, process_opcode(pointer, Int.add, array))
    | Product => run(pointer + 4, process_opcode(pointer, Int.mul, array))
    | Halt => array
    | Error => raise(Not_found)
    };

  /* [..] To do this, before running the program,
     replace position 1 with the value 12 and replace position 2 with the value 2. */
  let program_to_run = Array.copy(program);
  program_to_run[1] = noun;
  program_to_run[2] = verb;

  run(0, program_to_run)[0];
};

let solve_puzzle_1 = computer_thing(12, 2);

let puzzle_1 = parsed_input |> solve_puzzle_1;

Logs.info(m => m("Solution to day_2a: %d", puzzle_1));

// Puzzle 2
let verbs = List.init(99, x => x + 1);
let nouns = List.init(99, x => x + 1);

/* Find the input noun and verb that cause
   the program to produce the output 19690720 */
let solve_puzzle_2 = program =>
  List.map(
    noun =>
      List.map(
        verb =>
          switch (computer_thing(noun, verb, program)) {
          /* What is 100 * noun + verb? */
          | 19690720 => Some(100 * noun + verb)
          | _ => None
          },
        verbs,
      ),
    nouns,
  )
  |> List.concat
  |> List.filter(Option.is_some)
  |> List.hd;

let puzzle_2 =
  switch (solve_puzzle_2(parsed_input)) {
  | Some(value) => value
  | None => raise(Not_found)
  };

Logs.info(m => m("Solution to day_2b: %d", puzzle_2));
