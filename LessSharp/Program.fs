open System

open Less.Parser
open Less.Interpruter

[<EntryPoint>]
let main argv =

    let finish (args: ConsoleCancelEventArgs) = 
        args.Cancel <- true
        exit 0

    Event.add finish Console.CancelKeyPress
 
    while true do
        let s = Console.ReadLine()
        parseStr s
        |> fun a -> 
            printfn "Parsed %A" a
            eval a 
            |> printfn "Evaled %A"

    0 // return an integer exit code
