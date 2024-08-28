let test = "example_code/project.json"

open Scratch_compiler

let () = Printexc.record_backtrace true

let () = Parse.parse test
    |> Past_to_blocks.convert
    |> Blocks_to_untyped_ast.convert
    |> Untyped_ast_to_typed_ast.convert
    |> Typed_ast_to_llvm.convert
let () = print_endline @@ Llvm.string_of_llmodule Typed_ast_to_llvm.llmodule
let () = Typed_ast_to_llvm.aot_compile ()
(*let program = Front_end.front_end test
let () = print_endline @@ Typed_ast.show_program program
let _ = Typed_ast_to_llvm.convert program

let () = print_endline @@ Llvm.string_of_llmodule Typed_ast_to_llvm.llmodule

let () = Typed_ast_to_llvm.aot_compile ()*)
