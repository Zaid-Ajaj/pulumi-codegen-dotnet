module PulumiSchema.Parser

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

let private optionalArrayText (json: JObject) (key: string) : string list option =
    match json.[key] with
    | :? JArray as array -> Some(array.ToObject<string list>())
    | _ -> None

let private parseMapOfStrings (json: JObject) (key: string) : Map<string, string> = 
    match json.[key] with
    | :? JObject as dictionary -> 
        Map.ofList [ for prop in dictionary.Properties() -> prop.Name, prop.Value.ToString() ]
    | _ -> 
        Map.empty

let parseDotnetPropertyInfo (schemaJson: JObject) : DotnetPropertyInfo option = 
    if schemaJson.ContainsKey "language" && schemaJson["language"].Type = JTokenType.Object then
        let languageJson = schemaJson["language"] :?> JObject
        if languageJson.ContainsKey "csharp" && languageJson["csharp"].Type = JTokenType.Object then
            let csharpJson = languageJson["csharp"] :?> JObject
            match optionalText csharpJson "name" with 
            | Some name -> Some { name = name }
            | None -> None
        else 
            None
    else
        None

let rec parsePropertyDefinition (required: bool) (typeDefJson: JObject) : Property =
    let description = optionalText typeDefJson "description"
    let deprecationMessage = optionalText typeDefJson "deprecationMessage"
    { schemaType = parseType typeDefJson
      description = description |> Option.map (fun desc -> desc.TrimEnd())
      deprecationMessage = deprecationMessage
      secret = readBoolean typeDefJson "secret"
      plain = readBoolean typeDefJson "plain"
      replaceOnChanges = readBoolean typeDefJson "replaceOnChanges"
      required = required
      dotnetPropertyInfo = parseDotnetPropertyInfo typeDefJson }

and parseTypeDefinition (typeDefJson: JObject) : TypeDefinition =
    let description = optionalText typeDefJson "description"
    let deprecationMessage = optionalText typeDefJson "deprecationMessage"
    { schemaType = parseType typeDefJson
      description = description |> Option.map (fun desc -> desc.TrimEnd())
      deprecationMessage = deprecationMessage }

and parseType (typeJson: JObject) : SchemaType =
    if typeJson.ContainsKey "type" then 
        match string typeJson.["type"] with
        | "string" -> SchemaType.String
        | "number" -> SchemaType.Number
        | "integer" -> SchemaType.Integer
        | "boolean" -> SchemaType.Boolean
        | "array" ->
            let elementType = parseType (typeJson.["items"] :?> JObject)
            SchemaType.Array elementType
        | "object" when not (typeJson.ContainsKey "additionalProperties") ->
            let required = optionalArrayText typeJson "required" |> Option.defaultValue []
            let properties = typeJson.["properties"] :?> JObject
            let propertiesMap = Map.ofList [
                for property in properties.Properties() do
                    let requiredProperty = List.contains property.Name required
                    let propertyObject = property.Value :?> JObject
                    property.Name, parsePropertyDefinition requiredProperty propertyObject
            ]

            SchemaType.Object propertiesMap
        | "object" ->
            // assume additionalProperties is a type
            let additionalProperties = parseType (typeJson.["additionalProperties"] :?> JObject)
            SchemaType.Map additionalProperties

        | otherwise ->
            failwithf "Invalid type definition: %s" otherwise

    elif typeJson.ContainsKey "$ref" then
        match string typeJson.["$ref"] with
        | "pulumi.json#/Archive" -> SchemaType.Archive
        | "pulumi.json#/Asset" -> SchemaType.Asset
        | "pulumi.json#/Json" -> SchemaType.Json
        | "pulumi.json#/Any" -> SchemaType.Any
        | referencedType ->  SchemaType.Ref referencedType
    else 
        failwith "Invalid type definition"

let parseResource (token: string) (functions: Map<string, Function>) (resourceJson: JObject) : Resource =
    let requiredInputs = optionalArrayText resourceJson "requiredInputs" |> Option.defaultValue []
    let parseProperties (from: string) = Map.ofList [
        if resourceJson.ContainsKey from && resourceJson[from].Type = JTokenType.Object then
            let propertiesObject = resourceJson[from] :?> JObject
            
            for property in propertiesObject.Properties() do
                let required = List.contains property.Name requiredInputs
                property.Name, parsePropertyDefinition required (property.Value :?> JObject)
    ]

    let methods = Map.ofList [
        if resourceJson.ContainsKey "methods" && resourceJson.["methods"].Type = JTokenType.Object then
            let methodsObject = resourceJson.["methods"] :?> JObject
            for method in methodsObject.Properties() do
                let functionToken = method.Name
                match Map.tryFind functionToken functions with
                | Some functionDefinition -> yield method.Name, functionDefinition
                | None -> ()
    ]

    let stateInputs = 
        if resourceJson.ContainsKey "stateInputs" && resourceJson["stateInputs"].Type = JTokenType.Object then
            let stateInputsObject = resourceJson.["stateInputs"] :?> JObject
            Some (parseTypeDefinition stateInputsObject)
        else
            None

    {
        token = token
        description = optionalText resourceJson "description"
        isComponent = readBoolean resourceJson "isComponent"
        inputProperties = parseProperties "inputProperties"
        properties = parseProperties "properties"
        methods = methods
        stateInputs = stateInputs
    }

let parseFunction (token: string) (functionJson: JObject) : Function =
    let requiredInputs = optionalArrayText functionJson "requiredInputs" |> Option.defaultValue []
    let parseProperties (from: string) = Map.ofList [
        if functionJson.ContainsKey from && functionJson[from].Type = JTokenType.Object then
            let propertiesObject = functionJson[from] :?> JObject
            
            for property in propertiesObject.Properties() do
                let required = List.contains property.Name requiredInputs
                property.Name, parsePropertyDefinition required (property.Value :?> JObject)
    ]

    {
        token = token
        deprecationMessage = optionalText functionJson "deprecationMessage"
        description = optionalText functionJson "description"
        inputs = parseProperties "inputs"
        multiArgumentInputs = optionalArrayText functionJson "multiArgumentInputs" |> Option.defaultValue []
        returnType = parseType (functionJson.["outputs"] :?> JObject)
        isOverlay = readBoolean functionJson "isOverlay"
    }

let parseDotnetPackageInfo (schemaJson: JObject) : DotnetPackageInfo = 
    let defaultDotnetPackageInfo = {
        namespaces = Map.empty
        packageReferences = Map.empty
        projectReferences = []
        rootNamespace = "Pulumi"
        compatibility = None
        respectSchemaVersion = false
    }

    if schemaJson.ContainsKey "language" && schemaJson["language"].Type = JTokenType.Object then
        let languageJson = schemaJson["language"] :?> JObject
        if languageJson.ContainsKey "csharp" && languageJson["csharp"].Type = JTokenType.Object then
            let csharpJson = languageJson["csharp"] :?> JObject
            {
                namespaces = parseMapOfStrings csharpJson "namespaces"
                packageReferences = parseMapOfStrings csharpJson "packageReferences"
                projectReferences = optionalArrayText csharpJson "projectReferences" |> Option.defaultValue []
                rootNamespace = optionalText csharpJson "rootNamespace" |> Option.defaultValue defaultDotnetPackageInfo.rootNamespace
                compatibility = optionalText csharpJson "compatibility"
                respectSchemaVersion = readBoolean csharpJson "respectSchemaVersion"
            }
        else 
            defaultDotnetPackageInfo
    else
        defaultDotnetPackageInfo

let parseSchema (json: string) : Schema =
    let schemaJson = JObject.Parse(json)
    let name = text schemaJson "name"
    let description = optionalText schemaJson "description"
    let version = optionalText schemaJson "version"

    let functions = Map.ofList [
        if schemaJson.ContainsKey "functions" && schemaJson["functions"].Type = JTokenType.Object then 
            let functionsJson = schemaJson["functions"] :?> JObject
            for functionJson in functionsJson.Properties() do
                let functionTypeToken = functionJson.Name
                let functionDef = functionJson.Value :?> JObject
                functionTypeToken, parseFunction functionTypeToken functionDef
    ]

    let resources = Map.ofList [
        if schemaJson.ContainsKey "resources" && schemaJson["resources"].Type = JTokenType.Object then 
            let resourcesJson = schemaJson["resources"] :?> JObject
            for resoureJson in resourcesJson.Properties() do
                let resourceTypeToken = resoureJson.Name
                let resourceDef = resoureJson.Value :?> JObject
                resourceTypeToken, parseResource resourceTypeToken functions resourceDef
    ]

    let types = Map.ofList [
        if schemaJson.ContainsKey "types" && schemaJson["types"].Type = JTokenType.Object then 
            let typesJson = schemaJson["types"] :?> JObject
            for typeJson in typesJson.Properties() do
                let typeToken = typeJson.Name
                let typeDef = typeJson.Value :?> JObject
                typeToken, parseTypeDefinition typeDef
    ]

    let dotnetPackageInfo = parseDotnetPackageInfo schemaJson

    { name = name
      description = description
      resources = resources
      functions = functions
      types = types
      dotnetPackageInfo = dotnetPackageInfo
      version = version }