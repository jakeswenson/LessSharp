module Less.Parser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Less.Ast
open Less.Helpers

type Parser<'t> = Parser<'t, unit>

let selectorChar = asciiLetter <|> digit <|> anyOf [ '-'; '_' ] <??> "Selector character"
let selectorString = many1Chars selectorChar

let elementSelector = selectorString |>> Element 
let idSelector = pchar '#' >>. selectorString |>> Id
let classSelector = pchar '.' >>. selectorString |>> Class
let pseudoSelector = pchar ':' >>. selectorString |>> Pseudo

let selectorParser = 
    choiceL [
        classSelector;
        idSelector;
        pseudoSelector;
        elementSelector;
    ] "Selector" .>> spaces

let ruleSelector = many1 selectorParser

let hexRgb : Parser<Color> =
    let parse b = System.Byte.Parse(b, System.Globalization.NumberStyles.HexNumber)  
    let byte = pipe2 (hex |>> string) (hex |>> string) (+) |>> parse
    pchar '#' >>. tuple3 byte byte byte |>> (fun (r, g, b) -> Color.Create(r, g, b))

let rgbaColor =
    let intArg = pint32 .>> spaces .>> pchar ',' .>> spaces
    pstring "rgba" >>. between (pchar '(' >>. spaces) (pchar ')' .>> spaces) (tuple4 intArg intArg intArg (pfloat .>> spaces)) 
    |>> Color.Force

let rgbColor =
    let intArg = pint32 .>> spaces .>> pchar ',' .>> spaces
    pstring "rgb" >>. between (pchar '(' >>. spaces) (pchar ')' .>> spaces) (tuple3 intArg intArg (pint32 .>> spaces)) 
    |>> Color.Force

let hslColor =
    let intArg = pfloat .>> spaces .>> pchar ',' .>> spaces
    pstring "hsl" >>. between (pchar '(' >>. spaces) (pchar ')' .>> spaces) (tuple3 intArg intArg (pfloat .>> spaces)) 
    |>> hsl

let parseColor = 
    choiceL [
        hexRgb;
        rgbaColor;
        rgbColor;
        hslColor
    ] "Color"


let literalValue =  (parseColor .>> spaces .>> pchar ';' |>> Color) <|> (many1CharsTill anyChar (pchar ';') .>> spaces |>> Str) 
let literal = literalValue |>> Literal
let variableRef = pchar '@' >>. selectorString .>> spaces .>> pchar ';' .>> spaces |>> VariableRef

let propertyValue = 
    choiceL [
        variableRef;
        literal
    ] "Property Value"

let ruleProperty = selectorString .>> spaces .>> pchar ':' .>> spaces .>>. propertyValue |>> Property

let ruleProperties = many ruleProperty

let ruleBody = between (pchar '{' >>. spaces) (pchar '}' >>. spaces) ruleProperties

let manyRuleSelectors = sepBy1 ruleSelector (pchar ',' >>. spaces)

let rule = manyRuleSelectors .>> spaces .>>. ruleBody |>> Rule

let variable = pchar '@' >>. selectorString .>> spaces .>> pchar ':' .>> spaces .>>. literalValue .>> spaces |>> Variable

let lesscss = 
    choiceL [
        variable;
        rule
    ] "less css"

let lessRuleSet =  many1 lesscss 

let lessCss = lessRuleSet .>> eof

let parseFile file = runParserOnFile lessCss () file System.Text.Encoding.UTF8 
let parseStr str = 
    match str with
    | null -> []
    | str -> 
        match runParserOnString lessCss () "input" str with
        | Success(result, state, pos) -> result
        | Failure(msg, err, state) -> 
            printfn "Error parsing: %s" msg
            []
