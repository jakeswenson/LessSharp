module Less.Parser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Less.Ast

type Parser<'t> = Parser<'t, unit>

let selectorChar = asciiLetter <|> digit <|> anyOf [ '-'; '_' ] <??> "Selector character"
let selectorString = many1Chars selectorChar

let elementSelector = selectorString |>> Element 
let idSelector = pchar '#' >>. selectorString |>> Id
let classSelector = pchar '.' >>. selectorString |>> Class

let selectorParser = 
    choiceL [
        classSelector;
        idSelector
        elementSelector;
    ] "Selector" .>> spaces

let ruleSelector = many1 selectorParser

let literalValue =  many1CharsTill anyChar (pchar ';') .>> spaces |>> Literal
let variableRef = pchar '@' >>. selectorString .>> spaces .>> pchar ';' .>> spaces |>> VariableRef

let propertyValue = 
    choiceL [
        variableRef;
        literalValue
    ] "Property Value"

let ruleProperty = selectorString .>> spaces .>> pchar ':' .>> spaces .>>. propertyValue |>> Property

let ruleProperties = many1 ruleProperty

let ruleBody = between (pchar '{' >>. spaces) (pchar '}' >>. spaces) ruleProperties

let manyRuleSelectors = sepBy1 ruleSelector (pchar ',' >>. spaces)

let rule = manyRuleSelectors .>> spaces .>>. ruleBody |>> Rule

let variable = pchar '@' >>. selectorString .>> spaces .>> pchar ':' .>> spaces .>>. many1CharsTill anyChar (pchar ';') .>> spaces |>> Variable

let lesscss = 
    choiceL [
        variable;
        rule
    ] "less css"

let manyRules =  many1 lesscss 

let parseFile file = runParserOnFile manyRules () file System.Text.Encoding.UTF8 
let parseStr str = 
    match str with
    | null -> []
    | str -> 
        match runParserOnString manyRules () "input" str with
        | Success(result, state, pos) -> result
        | Failure(msg, err, state) -> 
            printfn "Error parsing: %s" msg
            []
