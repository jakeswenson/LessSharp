module Less.Ast

type Selector = 
    | Id of string
    | Class of string
    | Element of string

type Parameter = string

type PropertyValue =
    | Literal of string
    | VariableRef of Parameter

type RuleBody = 
    | Property of string * PropertyValue
    | MixinApply of string * Parameter list

type LessStatement = 
    | Rule of Selector list list * RuleBody list
    | ParametricMixin of Selector list * Parameter list * RuleBody list
    | Variable of Parameter * string

type LessRuleSet = LessStatement list
