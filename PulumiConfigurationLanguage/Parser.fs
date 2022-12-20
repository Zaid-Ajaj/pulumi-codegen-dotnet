module rec PulumiConfigurationLanguage.Parser

open Newtonsoft.Json.Linq
open Types

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
        | _ -> 
            None
    else 
        None

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

    { name = name; logicalName = logicalName; token = token; inputs = Map.ofSeq inputs }

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
            None

        | "ConfigVariable" ->
            None
        | _ -> 
            None
    else
        None

let parseProgram (json: string) : Program = 
    let programJson = JObject.Parse(json)
    let nodes = 
        if programJson.ContainsKey "nodes" && programJson["nodes"].Type = JTokenType.Array then
            programJson["nodes"] :?> JArray
            |> Seq.filter (fun node -> node.Type = JTokenType.Object)
            |> Seq.cast<JObject>
            |> Seq.choose parseNode
            |> Seq.toList
        else   
            []

    { nodes = nodes }
