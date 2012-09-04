module Less.Rewriter

open Less.Ast

let writeCss output ruleSet =
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

