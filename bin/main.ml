let test = "example_code/project.json"

open Scratch_compiler

let program = Front_end.front_end test
let () = print_endline @@ Typed_ast.show_program program
let _ = Typed_ast_to_llvm.convert program

let () = print_endline @@ Llvm.string_of_llmodule Typed_ast_to_llvm.llmodule

let () = Llvm_analysis.assert_valid_module Typed_ast_to_llvm.llmodule
let () = Llvm.dump_module Typed_ast_to_llvm.llmodule
let () = Llvm.dispose_module Typed_ast_to_llvm.llmodule

