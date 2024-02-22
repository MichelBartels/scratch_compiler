type state

val print_state: state -> unit
val interp_program: Ast.program -> state
