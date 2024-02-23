let test = "example_code/ack/project.json"

open Scratch_compiler

let program = Front_end.front_end test

let result = Interp_0.interp_program program
let _ = print_string "\n"; Interp_0.print_state result
