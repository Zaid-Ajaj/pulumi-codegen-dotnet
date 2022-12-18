module PulumiSchema.Parser

open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Types

let private optionalText (json: JObject) (key: string) : string option =
    match json.[key] with
    | :? JValue as value when value.Type = JTokenType.String -> Some(value.Value.ToString())
    | _ -> None

let private text (json: JObject) (key: string) : string =
    match json.[key] with
    | :? JValue as value when value.Type = JTokenType.String -> value.Value.ToString()
    | _ -> failwithf "Expected %s to be a string" key

let private readBoolean (json: JObject) (key: string) : bool =
    match json.[key] with
    | :? JValue as value when value.Type = JTokenType.Boolean -> unbox<bool> value.Value
    | _ -> false

let rec private parseTypeDefinition (typeDefJson: JObject) : TypeDefinition =
    let description = optionalText typeDefJson "description"
    let deprecationMessage = optionalText typeDefJson "deprecationMessage"
    let secret = readBoolean typeDefJson "secret"
    let isPlain = readBoolean typeDefJson "plain"
    let schemaType = if not isPlain then SchemaType.Output(parseType typeDefJson) else parseType typeDefJson

    { schemaType = schemaType
      description = description |> Option.map (fun desc -> desc.TrimEnd())
      deprecationMessage = deprecationMessage
      secret = secret }

and private parseType (typeJson: JObject) : SchemaType =
    if typeJson.ContainsKey "type" then 
        match string typeJson.["type"] with
        | "string" -> SchemaType.String
        | "number" -> SchemaType.Number
        | "integer" -> SchemaType.Integer
        | "boolean" -> SchemaType.Boolean
        | "pulumi:pulumi:asset" -> SchemaType.Asset
        | "pulumi:pulumi:archive" -> SchemaType.Archive
        | "array" ->
            let elementType = parseType (typeJson.["items"] :?> JObject)
            SchemaType.Array elementType
        | "object" when not (typeJson.ContainsKey "additionalProperties") ->
            let properties = typeJson.["properties"] :?> JObject
            let propertiesMap = 
                properties.Properties()
                |> Seq.map (fun prop -> prop.Name, parseTypeDefinition (prop.Value :?> JObject))
                |> Map.ofSeq
            SchemaType.Object propertiesMap
        | "object" ->
            // assume additionalProperties is a type
            let additionalProperties = parseType (typeJson.["additionalProperties"] :?> JObject)
            SchemaType.Map additionalProperties

        | otherwise ->
            failwithf "Invalid type definition: %s" otherwise

    elif typeJson.ContainsKey "$ref" then
        let ref = string typeJson.["$ref"]
        SchemaType.Ref ref
    else 
        failwith "Invalid type definition"


let private parseResource (token: string) (resourceJson: JObject) : Resource =
    let parseProperties (from: string) : Map<string, TypeDefinition> = Map.ofList [
        if resourceJson.ContainsKey from && resourceJson[from].Type = JTokenType.Object then
            let propertiesObject = resourceJson[from] :?> JObject
            for property in propertiesObject.Properties() do
                property.Name, parseTypeDefinition (property.Value :?> JObject)
    ]

    {
        token = token
        description = optionalText resourceJson "description"
        isComponent = readBoolean resourceJson "isComponent"
        requiredInputs =
            if resourceJson.ContainsKey "requiredInputs" && resourceJson["requiredInputs"].Type = JTokenType.Array then
               resourceJson["requiredInputs"].ToObject<string array>()
               |> Array.toList
            else
                []

        inputProperties = parseProperties "inputProperties"
        properties = parseProperties "properties"
    }

let parseSchema (json: string) : Schema =
    let schemaJson = JObject.Parse(json)
    
    let name = text schemaJson "name"
    let description = optionalText schemaJson "description"

    let resources = [
        if schemaJson.ContainsKey "resources" && schemaJson["resources"].Type = JTokenType.Object then 
            let resourcesJson = schemaJson["resources"] :?> JObject
            for resoureJson in resourcesJson.Properties() do
                let resourceTypeToken = resoureJson.Name
                let resourceDef = resoureJson.Value :?> JObject
                resourceTypeToken, parseResource resourceTypeToken resourceDef
    ]

    { name = name
      description = description
      resources = Map.ofList resources }
