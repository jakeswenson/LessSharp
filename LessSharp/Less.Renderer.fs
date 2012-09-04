module Less.Renderer

open Less.Ast

type LessRenderer() = 
    member this.Bind(x, f) = f x
    member this.Yield(x) = x |> ignore
    member this.Combine(a, b) = ()
    member this.Delay(m) = (fun () -> m()) ()
    member this.For(sequence, f) =
        Seq.iter f sequence
    member this.Zero() = ()

    
let render = LessRenderer()

render {
    yield 3
    for x in [1;2] do
        yield x
} 



let renderCss output ruleSet =
    let dumpSelectors w list = 
        List.iteri (fun i selectors -> 
            if i > 0 then fprintf w ", "
            List.iter (fprintf w "%O") selectors) list
    let dumpBody w properties = 
        List.iter (function 
            | Property(name, Literal value) -> 
                fprintf w "%s: %O" name value) properties
    List.iter (function 
        | Rule(s, b) -> 
            fprintfn output "%a {" dumpSelectors s
            fprintfn output "\t%a" dumpBody b
            fprintfn output "}"
    ) ruleSet

