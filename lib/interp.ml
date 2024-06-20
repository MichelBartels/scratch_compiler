open Typed_ast

module type Monad = sig
    type 'a t
    val return: 'a -> 'a t
    val (let+): 'a t -> ('a -> 'b t) -> 'b t
    val (and+): 'a t -> 'b t -> ('a * 'b) t
    val (let*): 'a t list -> ('a list -> 'b t) -> 'b t
end
type state = {
    variables: (string * Scratch_value.t) list;
    lists: (string * Scratch_value.t) list;
    answer: string;
}
[@@ deriving show]
module State: Monad with type 'a t = state -> 'a * state = struct
    type 'a t = state -> 'a * state
    let return a = fun state -> (a, state)
    let (let+) t f = fun state ->
        let (v, state) = t state in
        f v state
    let (and+) t1 t2 = fun state ->
        let (v1, state) = t1 state in
        let (v2, state) = t2 state in
        ((v1, v2), state)
    let (let*) ts f = fun state ->
        let (vs, state) = List.fold_left (fun (vs, state) t ->
            let (v, state) = t state in
            (v::vs, state)) ([], state) ts in
        f (List.rev vs) state
end

let assert_some = function | Some s -> s | None -> failwith "unexpected statement; this should have been detected during type inference"


let rec interp_expr program args expr =
    let interp_expr = interp_expr program in
    let interp_expr_local = interp_expr args in
    State.(match expr with 
    | Argument (arg, _) -> fun state -> (match Assoc_list.search arg args with
        | Some v -> (Some v, state)
        | None -> failwith @@ "could not find argument: " ^ arg)
    | Variable (var, _) -> fun state -> (match Assoc_list.search var state.variables with
        | Some v -> (Some v, state)
        | None -> failwith "could not find variable")
    | Literal v -> return @@ Some v
    | BinaryOperator (op, e1, e2) ->
            let+ v1 = interp_expr_local e1
            and+ v2 = interp_expr_local e2
            in (return @@ match (v1, v2, op) with
                | (Some (Primitive Float v1), Some (Primitive Float v2), Gt) -> Some (Scratch_value.Primitive (Boolean (v1 > v2)))
                | (Some (Primitive Float v1), Some (Primitive Float v2), Lt) -> Some (Primitive (Boolean (v1 < v2)))
                | (Some v1, Some v2, Equals) -> Some (Primitive (Boolean (v1 = v2)))
                | (Some (Primitive Float v1), Some (Primitive Float v2), Subtract) -> Some (Primitive (Float (v1 -. v2)))
                | (Some (Primitive Float v1), Some (Primitive Float v2), Add) -> Some (Primitive (Float (v1 +. v2)))
                | (Some (Primitive Boolean v1), Some (Primitive Boolean v2), Or) -> Some (Primitive (Boolean (v1 || v2)))
                | (Some (Primitive String v1), Some (Primitive String v2), Join) -> Some (Primitive (String (v1 ^ v2)))
                | (Some (Primitive Float v1), Some (Primitive String v2), LetterOf) -> Some (Primitive (String (String.make 1 v2.[int_of_float v1])))
                | (_, _, op) -> failwith @@ "invalid input for binary op: " ^ Untyped_ast.show_binary_operator op
            )
    | Not e ->
            let+ v = interp_expr_local e in
            let v = v |> assert_some |> Scratch_value.assert_bool in
            return @@ Some (Scratch_value.Primitive (Scratch_value.Boolean (not v)))
    | FuncCall (f, args) -> 
        let f = (match Assoc_list.search f program.functions with
            | Some f ->  f
            | None -> failwith @@ "could not find function: " ^ f) in
        let* vs = List.map (fun (_, v) -> interp_expr_local v) args in
        let vs = List.map assert_some vs in
        let args = List.map2 (fun (k, _) v -> (k, v)) args vs in
        let* _ = List.map (interp_expr args) f.statements in
        return None
    | Branch (cond, then_branch, else_branch) ->
        let+ cond = interp_expr_local cond in 
        let cond = cond = Some (Primitive (Boolean true)) in
        if cond then
            let* _ = List.map interp_expr_local then_branch in
            return None
        else
            let* _ = List.map interp_expr_local else_branch in
            return None
    | SetVariable (var, v) ->
        let+ v = interp_expr_local v in
        let v = assert_some v in
        (fun state -> (None, {
            variables = Assoc_list.update var v state.variables;
            lists = state.lists;
            answer = state.answer;
        }))
    | AddToList (k, v) ->
        let+ v = interp_expr_local v in
        let v = assert_some v |> Scratch_value.assert_primitive in 
        (fun state -> (None,
            let list = Assoc_list.search k state.lists |> assert_some |> Scratch_value.assert_list in
            {
                variables = state.variables;
                lists = Assoc_list.update k (Scratch_value.List (list @ [v])) state.lists;
                answer = state.answer;
            }
        ))
    | DeleteAllOfList l ->
        (fun state -> (None, {
            variables = state.variables;
            lists = Assoc_list.update l (Scratch_value.List []) state.lists;
            answer = state.answer;
        }))
    | Index (k, i, t) ->
        let+ i = interp_expr_local i in
        let i = i |> assert_some |> Scratch_value.assert_number in
        let i = int_of_float i - 1 in
        (fun state ->
            let list = Assoc_list.search k state.lists |> assert_some |> Scratch_value.assert_list in
            ((match List.nth_opt list i with Some v -> Some (Scratch_value.Primitive v) | None -> Some (Scratch_value.Primitive (Scratch_value.default_primitive (Scratch_type.assert_list t)))), state)
        )
    | IncrVariable (k, v) ->
        let+ v = interp_expr_local v in
        let v = v |> assert_some |> Scratch_value.assert_number in
        (fun state ->
            let var = Assoc_list.search k state.variables |> assert_some |> Scratch_value.assert_number in
            (None, {
                variables = Assoc_list.update k (Scratch_value.Primitive (Scratch_value.Float (v +. var))) state.variables;
                lists = state.lists;
                answer = state.answer;
            })
        )
    | IndexOf (k, v) ->
        let+ v = interp_expr_local v in
        let v = assert_some v in
        let v = Scratch_value.assert_primitive v in
        (fun state ->
            let list = Assoc_list.search k state.lists |> assert_some in
            let list = Scratch_value.assert_list list in
            let i = List.find_index ((=) v) list in
            let i = match i with Some i -> float_of_int (i + 1) | None -> 0. in
            (Some (Scratch_value.Primitive (Scratch_value.Float i)), state)
        )
    | SetIndex (l, i, v) ->
        let+ i = interp_expr_local i in
        let i = i |> assert_some |> Scratch_value.assert_number in
        let+ v = interp_expr_local v in
        let v = v |> assert_some |> Scratch_value.assert_primitive in
        (fun state ->
            let list = Assoc_list.search l state.lists |> assert_some |> Scratch_value.assert_list in
            let list = List.mapi (fun i' x -> if (float_of_int i') = (i -. 1.) then v else x) list in 
            (None, {
                variables = state.variables;
                lists = Assoc_list.update l (Scratch_value.List list) state.lists;
                answer = state.answer;
            })
        )
    | Length l ->
        (fun state ->
            let list = Assoc_list.search l state.lists |> assert_some |> Scratch_value.assert_list in
            (Some (Scratch_value.Primitive (Scratch_value.Float (float_of_int @@ List.length list))), state)
        )
    | WhileNot (cond, body) ->
        let rec loop state =
            (let+ cond = interp_expr_local cond in
            let cond = cond |> assert_some |> Scratch_value.assert_bool in
            if cond then return None
            else
                let* _ = List.map interp_expr_local body in
                loop) state
        in loop
    | Repeat (times, body) ->
        let+ times = interp_expr_local times in
        let times = times |> assert_some |> Scratch_value.assert_number |> int_of_float in
        let rec loop i state =
            (if (i = times) then return None
            else
                let* _ = List.map interp_expr_local body in
                loop (i + 1)) state
        in loop 0
    | Say v ->
        let+ v = interp_expr_local v in
        let v = v |> assert_some |> Scratch_value.assert_string in
        print_endline v;
        return None
    | Ask v ->
        let+ v = interp_expr_local v in
        let v = v |> assert_some |> Scratch_value.assert_string in
        print_endline v;
        let answer = read_line () in
        (fun state -> (None, {
            variables = state.variables;
            lists = state.lists;
            answer = answer;
        }))
    | Answer ->
        (fun state -> (Some (Scratch_value.Primitive (Scratch_value.String state.answer)), state))
    | Cast (v, t) ->
        let+ v = interp_expr_local v in
        let v = assert_some v in
        return @@ Some (Scratch_value.Primitive (Scratch_value.cast (Scratch_type.assert_primitive t) v))
    )

let interp program =
    State.(let* _ = List.map (interp_expr program []) program.main in
    return None) {
        variables = program.variables;
        lists = program.lists;
        answer = "";
    }
