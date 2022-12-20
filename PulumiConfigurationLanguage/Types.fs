module rec PulumiConfigurationLanguage.Types

[<RequireQualifiedAccess>]
type SyntaxValue = 
    | Bool of bool
    | String of string 
    | Int of int
    | Number of float
    | Null 

[<RequireQualifiedAccess>]
type TraversalKind = 
    | TraverseAttr of name:string
    | TraverseRoot of name:string
    | TraverseIndex of index:int
    | TraverseSplat of each:TraversalKind list

type ObjectConsExpression = { properties : Map<string, SyntaxExpr> }
type TupleConsExpression = { items : SyntaxExpr list }
type FunctionCallExpression = { name : string; args : SyntaxExpr list }
type ScopeTraversalExpression = { rootName : string; traversal : TraversalKind list }
type RelativeTraversalExpression = { source: SyntaxExpr; traversal : TraversalKind list }

[<RequireQualifiedAccess>]
type SyntaxExpr = 
    | LiteralValueExpression of SyntaxValue
    | TemplateExpression of parts:SyntaxExpr list
    | ObjectConsExpression of ObjectConsExpression
    | TupleConsExpression of TupleConsExpression
    | FunctionCallExpression of FunctionCallExpression
    | ScopeTraversalExpression of ScopeTraversalExpression
    | RelativeTraversalExpression of RelativeTraversalExpression

type Resource = {
    name: string 
    logicalName: string
    token: string
    inputs: Map<string, SyntaxExpr>
}

type OutputVariable = {
    name: string
    logicalName: string
    value: SyntaxExpr
}

type LocalVariable = {
    name: string
    value: SyntaxExpr
}

type ConfigVariable = {
    name: string
    configType: string
    logicalName: string
    value: SyntaxExpr
}

[<RequireQualifiedAccess>]
type Node = 
    | Resource of Resource
    | OutputVariable of OutputVariable
    | LocalVariable of LocalVariable
    | ConfigVariable of ConfigVariable

type Program = {
    nodes : Node list
}