let test = "example_code/turing/project.json"

open Scratch_compiler

let program = Front_end.front_end test
let _ = print_endline @@ Typed_ast.show_program program
