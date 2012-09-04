module Less.Interpreter

open Less.Ast

open System


let eval less = 
    let replace body env =
        List.map (function 
            | Property(name, VariableRef var) -> Property(name, Literal <| Map.find var env)
            | p -> p) body
    let eval less env = 
        match less with
        | Variable(name, value) -> [], Map.add name value env
        | Rule(selectors, body) ->
            [Rule(selectors, replace body env)], env

    List.fold (fun (result, env) stmt ->
       let eres, newEnv = eval stmt env
       (eres :: result, newEnv)
    )  ([], Map.empty) less 
    |> fst 
    |> List.concat
    |> List.rev

