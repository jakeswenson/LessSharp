open System

open Less.Parser
open Less.Interpreter

open Less.Helpers
open Less.Rewriter

[<EntryPoint>]
let main argv =

    let finish (args: ConsoleCancelEventArgs) = 
        args.Cancel <- true
        exit 1

    Event.add finish Console.CancelKeyPress
 
    let megenta = Color.Create(0.750, 0.250, 0.750)
    printfn "%O -> HSL%A" megenta megenta.Hsl
    printfn "%O -> HSV%A" megenta megenta.Hsv

    let megenta = Color.Create(0.211, 0.149, 0.597)
    printfn "%O -> HSL%A" megenta megenta.Hsl
    printfn "%O -> HSV%A" megenta megenta.Hsv


    hsv (248.3, 0.750, 0.597)
    |> printfn "%O"

    hsv (248.3035714, 0.7504187605, 0.597)
    |> printfn "%O"

    while true do
        let s = Console.ReadLine()
        parseStr s
        |> fun a -> 
            printfn "Parsed %A" a
            eval a 
            |> fun b ->
                printfn "Evaled %A" b
                writeCss Console.Out b
                Console.Out.Flush()

    0
