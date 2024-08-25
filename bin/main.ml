let test = "example_code/project.json"

open Scratch_compiler

let () = Printexc.record_backtrace true

let program = Parse.parse test |> Past_to_blocks.convert
let () = print_endline @@ Blocks.show_program program
(*let program = Front_end.front_end test
let () = print_endline @@ Typed_ast.show_program program
let _ = Typed_ast_to_llvm.convert program

let () = print_endline @@ Llvm.string_of_llmodule Typed_ast_to_llvm.llmodule

let () = Typed_ast_to_llvm.aot_compile ()*)
