module PulumiConfigurationLanguage.Converter

open System
open System.Text

open Types

type Printer = 
    abstract append : string -> unit
    abstract beginScope : (unit -> unit) -> unit
    abstract beginLexicalScope : string -> (unit -> unit) -> unit
    abstract indent : unit -> unit
    abstract newline: unit -> unit

let capitalize (input: string) =
    if String.IsNullOrWhiteSpace input then 
        ""
    else 
        input[0].ToString().ToUpper() + input.Substring(1)

let tokenToTypeName(token: string) = 
    match token.Split(':') with
    | [| pkg; "index"; typeName |] -> $"{capitalize pkg}.{capitalize typeName}"
    | [| pkg; moduleName; typeName |] -> $"{capitalize pkg}.{capitalize moduleName}.{capitalize typeName}"
    | _ -> failwithf "invalid token: %s" token

let packageFromToken (token: string) = 
    match token.Split(':') with
    | [| pkg; _; _ |] -> pkg
    | _ -> failwithf "invalid token: %s" token

let rec writeExpression (expr: SyntaxExpr) (printer: Printer) = 
    match expr with
    | SyntaxExpr.LiteralValueExpression literalValue -> 
        match literalValue with
        | SyntaxValue.String value -> printer.append $"\"{value}\""
        | SyntaxValue.Int value -> printer.append (value.ToString())
        | SyntaxValue.Number value -> printer.append (value.ToString())
        | SyntaxValue.Bool value -> printer.append (value.ToString())
        | SyntaxValue.Null -> printer.append "null"

    | SyntaxExpr.RelativeTraversalExpression relativeTraversal ->
        writeExpression relativeTraversal.source printer
        printer.append "."
        for part in relativeTraversal.traversal do
            match part with
            | TraversalKind.TraverseAttr name -> printer.append name
            | TraversalKind.TraverseIndex index -> 
                let literalIndex = SyntaxExpr.LiteralValueExpression(SyntaxValue.Int index)
                printer.append "["
                writeExpression literalIndex printer
                printer.append "]"
            | _ -> ()

    | SyntaxExpr.ScopeTraversalExpression scopeTraversal ->
        printer.append scopeTraversal.rootName
        if scopeTraversal.traversal.Length > 1 then
            printer.append "."

        for part in scopeTraversal.traversal do
            match part with
            | TraversalKind.TraverseAttr name -> printer.append name
            | TraversalKind.TraverseIndex index -> 
                let literalIndex = SyntaxExpr.LiteralValueExpression(SyntaxValue.Int index)
                printer.append "["
                writeExpression literalIndex printer
                printer.append "]"
            | _ -> ()

    | SyntaxExpr.FunctionCallExpression functionCall ->
        printer.append functionCall.name
        printer.append "("
        for (i, arg) in List.indexed functionCall.args do
            writeExpression arg printer
            if i < functionCall.args.Length - 1 then
                printer.append ", "
        printer.append ")"

    | SyntaxExpr.FunctionInvokeExpression invokeExpr -> 
        printer.append $"{tokenToTypeName invokeExpr.token}.Invoke("
        let inputsObjectExpression = SyntaxExpr.ObjectConsExpression invokeExpr.args
        writeExpression inputsObjectExpression printer
        printer.append ")"

    | SyntaxExpr.ObjectConsExpression objectExpression ->
        printer.beginLexicalScope "new " (fun () -> 
            for (i, pair) in Seq.indexed objectExpression.properties do
                printer.indent()
                printer.append $"{capitalize pair.Key} = "
                writeExpression pair.Value printer
                if i < objectExpression.properties.Count - 1 then
                    printer.append ","
                    printer.newline()
                else 
                    printer.newline()
        )

    | SyntaxExpr.TupleConsExpression tupleExpression ->
        printer.beginLexicalScope "new []" (fun () -> 
            for (i, expr) in List.indexed tupleExpression.items do
                printer.indent()
                writeExpression expr printer
                if i < tupleExpression.items.Length - 1 then
                    printer.append ","
                printer.newline()
        )

    | SyntaxExpr.TemplateExpression templateParts -> 
        printer.append "Output.Join("
        for (i, part) in List.indexed templateParts do
            printer.append "@"
            writeExpression part printer
            if i < templateParts.Length - 1 then
                printer.append ", "
        printer.append ")"

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

    let loadedSchemas = 
        program.plugins
        |> List.choose (fun plugin -> 
            match PulumiSchema.SchemaLoader.FromPulumi(plugin.name, plugin.version) with
            | Ok schema -> Some (plugin.name, schema)
            | Error msg -> 
                printfn "Failed to load schema for %s v%s: %s" plugin.name plugin.version msg
                None)
        |> Map.ofList

    let printer = {
        new Printer with
            member __.append text = append text
            member __.beginScope f = beginScope f
            member __.indent() = indent()
            member __.newline() = newline()
            member __.beginLexicalScope prefix f =
                append prefix
                newline()
                indent()
                append "{"
                newline()
                beginScope f
                indent()
                append "}"
    }

    let writeConfigVariable (config: ConfigVariable) =
        ()

    let writeLocalVariable (localVariable: LocalVariable) =
        indent()
        append $"var {localVariable.name} = "
        writeExpression localVariable.value printer
        append ";"
        newline()

    let writeResource (resource: Resource) =
        indent()
        if resource.inputs.IsEmpty then 
            append $"var {resource.name} = new {tokenToTypeName(resource.token)}(\"{resource.logicalName}\");"
        else
            append $"var {resource.name} = new {tokenToTypeName(resource.token)}(\"{resource.logicalName}\", "
            printer.beginLexicalScope "new ()" (fun () -> 
                for (i, pair) in Seq.indexed resource.inputs do
                    printer.indent()
                    printer.append $"{capitalize pair.Key} = "
                    writeExpression pair.Value printer
                    if i < resource.inputs.Count - 1 then
                        printer.append ","
                        printer.newline()
                    else 
                        printer.newline()
            )
            append ");"
            newline()

    append "using System.Collections.Generic;"; newline()
    append "using Pulumi;"; newline()
    for plugin in program.plugins do
        append $"using {capitalize plugin.name} = Pulumi.{capitalize plugin.name};";
        newline()
    newline()
    append "return await Deployment.RunAsync(() => "; newline()
    append "{";
    newline()

    beginScope (fun _ ->
        for node in program.nodes do 
            match node with
            | Node.ConfigVariable configDeclaration -> 
                writeConfigVariable configDeclaration
                newline()
            | Node.LocalVariable variableDeclaration -> 
                writeLocalVariable variableDeclaration
                newline()
            | Node.Resource resourceDeclaration -> 
                writeResource resourceDeclaration
                newline()
            | Node.OutputVariable _ -> ()

        let outputVariables = 
            program.nodes
            |> List.choose (function 
                | Node.OutputVariable output -> Some output
                | _ -> None)
        
        if outputVariables.Length > 0 then
            newline()
            indent()
            printer.beginLexicalScope "return new Dictionary<string, object?>" (fun () -> 
                for (i, output) in List.indexed outputVariables do
                    indent()
                    append $"[\"{output.logicalName}\"] = "
                    writeExpression output.value printer
                    if i < outputVariables.Length - 1 then
                        append ","
                    newline()
            )
            append ";"
            newline()
    )
    append "});"

    builder.ToString()