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
type FunctionCallExpression = { name : string; args: SyntaxExpr list }
type ScopeTraversalExpression = { rootName : string; traversal : TraversalKind list }
type RelativeTraversalExpression = { source: SyntaxExpr; traversal : TraversalKind list }
type IndexExpression = { collection: SyntaxExpr; key: SyntaxExpr }
type ConditionalExpression = { condition: SyntaxExpr; trueExpr: SyntaxExpr; falseExpr: SyntaxExpr }
type BinaryOpExpression = { left: SyntaxExpr; right: SyntaxExpr; operation: string }
type UnaryOpExpression = { operand: SyntaxExpr; operation: string }
type AnonymousFunctionExpression = { parameters: string list; body: SyntaxExpr }

[<RequireQualifiedAccess>]
type SyntaxExpr = 
    | LiteralValueExpression of SyntaxValue
    | TemplateExpression of parts:SyntaxExpr list
    | ObjectConsExpression of ObjectConsExpression
    | TupleConsExpression of TupleConsExpression
    | FunctionCallExpression of FunctionCallExpression
    | ScopeTraversalExpression of ScopeTraversalExpression
    | RelativeTraversalExpression of RelativeTraversalExpression
    | IndexExpression of IndexExpression
    | ConditionalExpression of ConditionalExpression
    | BinaryOpExpression of BinaryOpExpression
    | UnaryOpExpression of UnaryOpExpression
    | AnonymousFunctionExpression of AnonymousFunctionExpression


type ResourceOptions = {
    dependsOn: SyntaxExpr option
    protect: SyntaxExpr option
    provider: SyntaxExpr option
    version: SyntaxExpr option
    ignoreChanges: SyntaxExpr option
    parent: SyntaxExpr option
}

type Resource = {
    name: string 
    logicalName: string
    token: string
    inputs: Map<string, SyntaxExpr>
    options: ResourceOptions
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
    defaultValue: SyntaxExpr option
    logicalName: string
    configType: string
}

[<RequireQualifiedAccess>]
type Node = 
    | Resource of Resource
    | OutputVariable of OutputVariable
    | LocalVariable of LocalVariable
    | ConfigVariable of ConfigVariable

type PluginReference = {
    name: string
    version: string
}

type Program = {
    nodes : Node list
    plugins: PluginReference list
}