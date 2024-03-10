type variable = {
    id: string;
    name: string;
    value: Untyped_ast.scratch_value;
}
[@@ deriving show]

type input =
    | Id of string
    | Variable of string
    | Value of Untyped_ast.scratch_value
[@@ deriving show]

type block =
    | Argument of {
        id: string;
        name: string;
    }
    | ProceduresDefinition of {
        id: string;
        next: string option;
        prototype: string;
    }
    | ProceduresPrototype of {
        id: string;
        parameters: (string * string) list;
        proccode: string;
    }
    | BinaryOperator of {
        id: string;
        operator: Untyped_ast.binary_operator;
        arg1: input;
        arg2: input;
    }
    | Not of {
        id: string;
        arg: input;
    }
    | ProceduresCall of {
        id: string;
        next: string option;
        inputs: (string * input) list;
        proccode: string
    }
    | Start of {
        id: string;
        next: string option;
    }
    | IfThenElse of {
        id: string;
        next: string option;
        condition: string;
        then_branch: string option;
        else_branch: string option;
    }
    | SetVariable of {
        id: string;
        next: string option;
        variable: string;
        value: input;
    }
    | AddToList of {
        id: string;
        next: string option;
        list: string;
        item: input;
    }
    | DeleteAllOfList of {
        id: string;
        next: string option;
        list: string;
    }
    | NumOfList of {
        id: string;
        list: string;
        index: input;
    }
    | ChangeVariableBy of {
        id: string;
        next: string option;
        value: input;
        variable: string;
    }
    | ItemOfList of {
        id: string;
        list: string;
        item: input
    }
    | ReplaceItemOfList of {
        id: string;
        next: string option;
        list: string;
        index: input;
        item: input
    }
    | LengthOfList of {
        id: string;
        list: string;
    }
    | RepeatUntil of {
        id: string;
        next: string option;
        condition: input;
        body: string option;
    }
    | Repeat of {
        id: string;
        next: string option;
        count: input;
        body: string option;
    }
    | Say of {
        id: string;
        next: string option;
        message: input;
    }
    | Ask of {
        id: string;
        next: string option;
        question: input;
    }
    | Answer of {
        id: string;
    }
[@@ deriving show]

type program = {
    variables: variable list;
    lists: variable list;
    blocks: block list
}
[@@ deriving show]
