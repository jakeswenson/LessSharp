module Less.Ast
open Less.Helpers

type Selector = 
    | Id of string
    | Class of string
    | Element of string
    | Pseudo of string
    override this.ToString() = 
        match this with
        | Id id -> "#" + id
        | Class cls -> "." + cls
        | Pseudo cls -> ":" + cls
        | Element element -> element

type Parameter = string

type Unit = Em | Pixel | Percent | Ew

type LiteralValue =
    | Str of string
    | Color of Color
    | Unit of Unit * double
    | Long of int64
    override this.ToString() = 
        match this with 
        | Str(s) -> s
        | Color(c) -> c.ToString()
        | a -> sprintf "%A" a

type PropertyValue =
    | Literal of LiteralValue
    | VariableRef of Parameter

type RuleBody = 
    | Property of string * PropertyValue
    | MixinApply of string * Parameter list

type LessStatement = 
    | Rule of Selector list list * RuleBody list
    | ParametricMixin of Selector list * Parameter list * RuleBody list
    | Variable of Parameter * LiteralValue

type LessRuleSet = LessStatement list
