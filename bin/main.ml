let test = "example_code/project.json"

open Scratch_compiler

let () = Printexc.record_backtrace true

let program = Front_end.front_end test
let () = print_endline @@ Typed_ast.show_program program
let (_, result) = Interp.interp program
let () = print_endline @@ Interp.show_state result
