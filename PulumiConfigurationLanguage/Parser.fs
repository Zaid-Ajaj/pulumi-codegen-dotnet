module rec PulumiConfigurationLanguage.Parser

open Newtonsoft.Json.Linq
open Types

let concanicalToken (token: string) = PulumiSchema.Parser.canonicalToken token

let private text (json: JObject) (key: string) : string =
    match json.[key] with
    | :? JValue as value when value.Type = JTokenType.String -> value.Value.ToString()
    | _ -> failwithf $"Expected %s{key} to be a string"


let parseLiteralValueExpression (json: JObject) : SyntaxExpr = 
    let value = 
        if json.ContainsKey "value" then
            let valueJson = json["value"]
            match valueJson.Type with
            | JTokenType.String -> 
                SyntaxValue.String (valueJson.ToObject<string>())
            | JTokenType.Integer ->
                SyntaxValue.Int (valueJson.ToObject<int>())
            | JTokenType.Float ->
                SyntaxValue.Number (valueJson.ToObject<float>())
            | JTokenType.Boolean ->
                SyntaxValue.Bool (valueJson.ToObject<bool>())
            | anythingElse -> 
                SyntaxValue.Null
        else 
            SyntaxValue.Null
    SyntaxExpr.LiteralValueExpression value

let parseTraversalKind (json: JObject) : TraversalKind =
    match string json["type"] with
    | "TraverseAttr" -> 
        TraversalKind.TraverseAttr (name = string json["name"])
    | "TraverseRoot" ->
        TraversalKind.TraverseRoot (name = string json["name"])
    | "TraverseIndex" ->
        TraversalKind.TraverseIndex (index = int json["index"])
    | "TraverseSplat" ->
        let each = 
            if json.ContainsKey "each" && json["each"].Type = JTokenType.Array then
                json["each"] :?> JArray
                |> Seq.filter (fun each -> each.Type = JTokenType.Object)
                |> Seq.cast<JObject>
                |> Seq.map parseTraversalKind
                |> Seq.toList
            else
                []
        TraversalKind.TraverseSplat (each = each)
    | anythingElse ->
        failwithf $"Unknown traversal type %s{anythingElse}"

let parseExpression (json: JObject) : SyntaxExpr option = 
    if json.ContainsKey "type" && json["type"].Type = JTokenType.String then
        match json["type"].ToString() with
        | "LiteralValueExpression" -> 
            Some (parseLiteralValueExpression json)
        | "TemplateExpression" ->
            let parts = 
                if json.ContainsKey "parts" && json["parts"].Type = JTokenType.Array then
                    json["parts"] :?> JArray
                    |> Seq.filter (fun part -> part.Type = JTokenType.Object)
                    |> Seq.cast<JObject>
                    |> Seq.choose parseExpression
                    |> Seq.toList
                else
                    []
            Some (SyntaxExpr.TemplateExpression parts)

        | "ObjectConsExpression" ->
            let properties = 
                if json.ContainsKey "properties" && json["properties"].Type = JTokenType.Object then
                    let propertiesObject = json["properties"] :?> JObject
                    propertiesObject.Properties()
                    |> Seq.filter (fun property -> property.Value.Type = JTokenType.Object)
                    |> Seq.map (fun property -> property.Name, property.Value :?> JObject)
                    |> Seq.choose (fun (name, value) -> 
                        match parseExpression value with
                        | Some expression -> Some(name, expression)
                        | None -> None    
                    )
                    |> Map.ofSeq
                else
                    Map.empty

            Some (SyntaxExpr.ObjectConsExpression { properties = properties })

        | "TupleConsExpression" ->
            let items = 
                if json.ContainsKey "items" && json["items"].Type = JTokenType.Array then
                    json["items"] :?> JArray
                    |> Seq.filter (fun item -> item.Type = JTokenType.Object)
                    |> Seq.cast<JObject>
                    |> Seq.choose parseExpression
                    |> Seq.toList
                else
                    []
            Some (SyntaxExpr.TupleConsExpression { items = items })

        | "FunctionCallExpression" when text json "name" = "invoke" ->
            let args = 
                if json.ContainsKey "args" && json["args"].Type = JTokenType.Array then
                    json["args"] :?> JArray
                    |> Seq.filter (fun arg -> arg.Type = JTokenType.Object)
                    |> Seq.cast<JObject>
                    |> Seq.choose parseExpression
                    |> Seq.toList
                else
                    []

            match args with 
            | [ SyntaxExpr.LiteralValueExpression (SyntaxValue.String token) ] ->
                let emptyArgs = { properties = Map.empty }
                Some (SyntaxExpr.FunctionInvokeExpression { token = concanicalToken token; args = emptyArgs })
            | [ 
                SyntaxExpr.LiteralValueExpression (SyntaxValue.String token); 
                SyntaxExpr.ObjectConsExpression objectConsExpression
              ] ->
                Some (SyntaxExpr.FunctionInvokeExpression { token = concanicalToken token; args = objectConsExpression })

            | _ -> 
                Some (SyntaxExpr.FunctionCallExpression { name = "invoke"; args = args })

        | "FunctionCallExpression" -> 
            let name = text json "name"
            let args = 
                if json.ContainsKey "args" && json["args"].Type = JTokenType.Array then
                    json["args"] :?> JArray
                    |> Seq.filter (fun arg -> arg.Type = JTokenType.Object)
                    |> Seq.cast<JObject>
                    |> Seq.choose parseExpression
                    |> Seq.toList
                else
                    []
            Some (SyntaxExpr.FunctionCallExpression { name = name; args = args })

        | "ScopeTraversalExpression" ->
            let traversalJson = json["traversal"] :?> JArray
            let traversal = 
                traversalJson
                |> Seq.filter (fun traversal -> traversal.Type = JTokenType.Object)
                |> Seq.cast<JObject>
                |> Seq.map parseTraversalKind
                |> Seq.toList
            let rootName= text json "rootName"

            Some (SyntaxExpr.ScopeTraversalExpression { rootName = rootName; traversal = traversal })

        | "RelativeTraversalExpression" ->
            let traversalJson = json["traversal"] :?> JArray
            let traversal = 
                traversalJson
                |> Seq.filter (fun traversal -> traversal.Type = JTokenType.Object)
                |> Seq.cast<JObject>
                |> Seq.map parseTraversalKind
                |> Seq.toList

            let sourceJson = json["source"] :?> JObject
            match parseExpression sourceJson with
            | Some source ->
                Some (SyntaxExpr.RelativeTraversalExpression { traversal = traversal; source = source })
            | None -> 
                None

        | "IndexExpression" ->
            if not (json.ContainsKey "key") || json["key"].Type <> JTokenType.Object then
                None
            elif not (json.ContainsKey "collection") || json["collection"].Type <> JTokenType.Object then
                None
            else
                let key = json["key"] :?> JObject
                let collection = json["collection"] :?> JObject
                match parseExpression key, parseExpression collection with
                | Some key, Some collection ->
                    Some (SyntaxExpr.IndexExpression { key = key; collection = collection })
                | _ ->
                    None

        | "ConditionalExpression" -> 
            if not (json.ContainsKey "condition") || json["condition"].Type <> JTokenType.Object then
                None
            elif not (json.ContainsKey "trueExpr") || json["trueExpr"].Type <> JTokenType.Object then
                None
            elif not (json.ContainsKey "falseExpr") || json["falseExpr"].Type <> JTokenType.Object then
                None
            else
                let condition = json["condition"] :?> JObject
                let trueExpr = json["trueExpr"] :?> JObject
                let falseExpr = json["falseExpr"] :?> JObject
                match parseExpression condition, parseExpression trueExpr, parseExpression falseExpr with
                | Some condition, Some trueExpr, Some falseExpr ->
                    Some (SyntaxExpr.ConditionalExpression { condition = condition; trueExpr = trueExpr; falseExpr = falseExpr })
                | _ -> 
                    None

        | "BinaryOpExpression" ->
            if not (json.ContainsKey "left") || json["left"].Type <> JTokenType.Object then
                None
            elif not (json.ContainsKey "right") || json["right"].Type <> JTokenType.Object then
                None
            elif not (json.ContainsKey "operation") || json["operation"].Type <> JTokenType.String then
                None
            else
                let left = json["left"] :?> JObject
                let right = json["right"] :?> JObject
                let operation = text json "operation"
                match parseExpression left, parseExpression right with
                | Some left, Some right ->
                    Some (SyntaxExpr.BinaryOpExpression { left = left; right = right; operation = operation })
                | _ -> 
                    None

        | "UnaryOpExpression" ->
            if not (json.ContainsKey "operand") || json["operand"].Type <> JTokenType.Object then
                None
            elif not (json.ContainsKey "operation") || json["operation"].Type <> JTokenType.String then
                None
            else
                let operand = json["operand"] :?> JObject
                let operation = text json "operation"
                match parseExpression operand with
                | Some operand ->
                    Some (SyntaxExpr.UnaryOpExpression { operand = operand; operation = operation })
                | _ -> 
                    None

        | "AnonymousFunctionExpression" -> 
            if not (json.ContainsKey "parameters") || json["parameters"].Type <> JTokenType.Array then
                None
            elif not (json.ContainsKey "body") || json["body"].Type <> JTokenType.Object then
                None
            else
                let parameters = json["parameters"].ToObject<string array>() |> Array.toList
                let body = json["body"] :?> JObject
                match parseExpression body with
                | Some body ->
                    Some (SyntaxExpr.AnonymousFunctionExpression { parameters = parameters; body = body })
                | _ -> 
                    None
        | _ -> 
            None
    else 
        None

let optionalObject (json: JObject) (key: string) : JObject option = 
    if json.ContainsKey key && json[key].Type = JTokenType.Object then
        Some (json[key] :?> JObject)
    else
        None

let parseResourceOptions (json: JObject) : ResourceOptions = 
    {
        dependsOn = optionalObject json "dependsOn"  |> Option.bind parseExpression
        parent = optionalObject json "parent" |> Option.bind parseExpression
        protect = optionalObject json "protect" |> Option.bind parseExpression
        ignoreChanges = optionalObject json "ignoreChanges" |> Option.bind parseExpression
        provider = optionalObject json "provider" |> Option.bind parseExpression
        version = optionalObject json "version" |> Option.bind parseExpression
    }

let parseResource (json: JObject) : Resource = 
    let name = text json "name"
    let logicalName = text json "logicalName"
    let token = text json "token"
    let inputs = 
        if json.ContainsKey "inputs" && json["inputs"].Type = JTokenType.Object then
            let inputsObject = json["inputs"] :?> JObject
            inputsObject.Properties()
            |> Seq.filter (fun property -> property.Value.Type = JTokenType.Object)
            |> Seq.map (fun property -> property.Name, property.Value :?> JObject)
            |> Seq.choose (fun (name, value) -> 
                match parseExpression value with
                | Some expression -> Some(name, expression)
                | None -> None    
            )

        else
            Seq.empty

    let options = 
        if json.ContainsKey "options" && json["options"].Type = JTokenType.Object then
            let optionsObject = json["options"] :?> JObject
            parseResourceOptions optionsObject
        else
            parseResourceOptions (JObject.Parse "{}")

    { name = name; logicalName = logicalName; token = concanicalToken token; inputs = Map.ofSeq inputs; options = options }

let parseOutputVariable (json: JObject) : OutputVariable option = 
    let name = text json "name"
    let logicalName = text json "logicalName"
    let value = json["value"] :?> JObject
    match parseExpression value with
    | Some value -> Some { name = name; logicalName = logicalName; value = value }
    | None -> None

let parseNode (json: JObject) : Node option = 
    if json.ContainsKey "type" && json["type"].Type = JTokenType.String then
        match json["type"].ToString() with
        | "Resource" -> 
            let resource = Node.Resource (parseResource json)
            Some resource

        | "OutputVariable" ->
            match parseOutputVariable json with
            | Some outputVariable -> Some (Node.OutputVariable outputVariable)
            | None -> None
        
        | "LocalVariable" ->
            let name = text json "name"
            let value = json["value"] :?> JObject
            match parseExpression value with
            | Some value -> Some (Node.LocalVariable { name = name; value = value })
            | None -> None

        | "ConfigVariable" ->
            let name = text json "name"
            let logicalName = text json "logicalName"
            let configType = text json "configType"
            let configNode = Node.ConfigVariable { 
                name = name
                logicalName = logicalName
                configType = configType
                defaultValue = optionalObject json "defaultValue" |> Option.bind parseExpression
            }

            Some configNode

        | _ -> 
            None
    else
        None

let parsePluginReference (json: JObject) : PluginReference =
    let name = text json "name"
    let version = text json "version"
    { name = name; version = version }

let parseProgram (json: string) : Program = 
    let programJson = JObject.Parse(json)
    let nodes = 
        if programJson.ContainsKey "nodes" && programJson["nodes"].Type = JTokenType.Array then
            programJson["nodes"] :?> JArray
            |> Seq.filter (fun node -> node.Type = JTokenType.Object)
            |> Seq.cast<JObject>
            |> Seq.choose parseNode
            |> Seq.sortBy (function
                | Node.ConfigVariable _ -> 0
                | Node.LocalVariable _ -> 1
                | Node.Resource _ -> 2
                | Node.OutputVariable _ -> 3)
            |> Seq.toList
        else   
            []

    let pluginReferences = 
        if programJson.ContainsKey "plugins" && programJson["plugins"].Type = JTokenType.Array then
            programJson["plugins"] :?> JArray
            |> Seq.filter (fun pluginReference -> pluginReference.Type = JTokenType.Object)
            |> Seq.cast<JObject>
            |> Seq.map parsePluginReference
            |> Seq.toList
        else
            []

    { plugins = pluginReferences; nodes = nodes }
