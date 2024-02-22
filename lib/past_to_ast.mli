type variable
module VariableExtractor: sig
  val extract : Past.value -> variable list
end

type general_block
module BlockExtractor: sig
  val extract : Past.value -> general_block list
end

type input

type block

val print_variable: variable -> unit
val print_general_block: general_block -> unit
val print_blocks: block list -> unit

val general_block_to_block: general_block -> block
val create_program: block list -> variable list -> Ast.program
val translate_past: Past.value -> Ast.program
