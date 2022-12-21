module PulumiConfigurationLanguage.Converter

open System
open System.Text

open Types

let rec writeExpression 
    (expr: SyntaxExpr) 
    (append: string -> unit)
    (beginScope: (unit -> unit) -> unit)
    (indent: unit -> unit) = 

    let newline() = append "\n"
    match expr with
    | SyntaxExpr.LiteralValueExpression literalValue -> 
        match literalValue with
        | SyntaxValue.String value -> append $"\"{value}\""
        | SyntaxValue.Int value -> append (value.ToString())
        | SyntaxValue.Number value -> append (value.ToString())
        | SyntaxValue.Bool value -> append (value.ToString())
        | SyntaxValue.Null -> append "null"

    | SyntaxExpr.RelativeTraversalExpression relativeTraversal ->
        writeExpression relativeTraversal.source append beginScope indent
        append "."
        for part in relativeTraversal.traversal do
            match part with
            | TraversalKind.TraverseAttr name -> append name
            | TraversalKind.TraverseIndex index -> 
                let literalIndex = SyntaxExpr.LiteralValueExpression(SyntaxValue.Int index)
                append "["
                writeExpression literalIndex append beginScope indent
                append "]"
            | _ -> ()

    | SyntaxExpr.ScopeTraversalExpression scopeTraversal ->
        append scopeTraversal.rootName
        append "."
        for part in scopeTraversal.traversal do
            match part with
            | TraversalKind.TraverseAttr name -> append name
            | TraversalKind.TraverseIndex index -> 
                let literalIndex = SyntaxExpr.LiteralValueExpression(SyntaxValue.Int index)
                append "["
                writeExpression literalIndex append beginScope indent
                append "]"
            | _ -> ()

    | SyntaxExpr.FunctionCallExpression functionCall ->
        append functionCall.name
        append "("
        for (i, arg) in List.indexed functionCall.args do
            writeExpression arg append beginScope indent
            if i < functionCall.args.Length - 1 then
                append ", "
        append ")"

    | SyntaxExpr.ObjectConsExpression objectExpression ->
        append "new"
        newline()
        indent()
        append "{"
        newline()
        beginScope (fun _ -> 
        for (i, pair) in Seq.indexed objectExpression.properties do
            indent()
            append $"{pair.Key} = "
            writeExpression pair.Value append beginScope indent
            if i < objectExpression.properties.Count - 1 then
                append ","
                newline()
            else 
                newline()
        )
        indent()
        append "}"

    | SyntaxExpr.TupleConsExpression tupleExpression ->
        append "new []"
        newline()
        indent()
        append "{"
        newline()
        beginScope (fun _ -> 
        for (i, expr) in List.indexed tupleExpression.items do
            indent()
            writeExpression expr append beginScope indent
            if i < tupleExpression.items.Length - 1 then
                append ","
            newline()
        )
        indent()
        append "}"

    | _ -> ()

let convertToCSharp (program: Program) : string =
    let builder = StringBuilder()
    let mutable indentSize = 0
    
    let indent() = 
        let indentText = String.init indentSize (fun _ -> " ")
        builder.Append(indentText)
        |> ignore

    let append (text: string) =
        builder.Append(text)
        |> ignore

    let newline() = append "\n"

    let beginScope f =
        indentSize <- indentSize + 4
        f()
        indentSize <- indentSize - 4


    let writeConfigVariable (config: ConfigVariable) =
        ()

    let writeLocalVariable (localVariable: LocalVariable) =
        indent()
        append $"var {localVariable.name} = "
        writeExpression localVariable.value append beginScope indent
        append ";"
        newline()

    let writeResource (resource: Resource) =
        ()
    
    append "using System;"; newline()
    append "using Pulumi;"; newline()
    append "using System.Collections.Generic;"; newline()
    append "using System.Collections.Immutable;"; newline()
    append "using System.Threading.Tasks;"; newline()
    newline()
    append "return await Deployment.RunAsync(() => "; newline()
    append "{"; newline()
    beginScope (fun _ ->
        for node in program.nodes do 
            match node with
            | Node.ConfigVariable configDeclaration -> writeConfigVariable configDeclaration
            | Node.LocalVariable variableDeclaration -> writeLocalVariable variableDeclaration
            | Node.Resource resourceDeclaration -> writeResource resourceDeclaration
            | Node.OutputVariable _ -> ()
    )
    append "});"


    builder.ToString()