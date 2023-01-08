module PulumiSchema.Parser

open Newtonsoft.Json.Linq
open Types

let canonicalToken (token: string) = 
    match token.Split("::") with
    | [| package; typeName |] -> sprintf "%s:index:%s" package typeName
    | _ ->
        match token.Split(":") with
        | [| package; moduleName; typeName |] -> 
            match moduleName.Split "/" with
            | [| firstPart; secondPart |] -> sprintf "%s:%s:%s" package firstPart typeName
            | _ -> sprintf "%s:%s:%s" package moduleName typeName
        | _ ->
            failwithf "invalid token %s" token

let private optionalText (json: JObject) (key: string) : string option =
    match json.[key] with
    | :? JValue as value when value.Type = JTokenType.String -> Some(value.Value.ToString())
    | _ -> None

let private text (json: JObject) (key: string) : string =
    match json.[key] with
    | :? JValue as value when value.Type = JTokenType.String -> value.Value.ToString()
    | _ -> failwithf "Expected '%s' to be a string" key

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

and parseProperties (typeDefJson: JObject) : Map<string, Property> =
    let required = optionalArrayText typeDefJson "required" |> Option.defaultValue []
    Map.ofList [
        if  typeDefJson.ContainsKey "properties" && typeDefJson["properties"].Type = JTokenType.Object then
            let properties = typeDefJson["properties"] :?> JObject
            for property in properties.Properties() do
                let requiredProperty = List.contains property.Name required
                let propertyObject = property.Value :?> JObject
                property.Name, parsePropertyDefinition requiredProperty propertyObject
    ]

and parseObjectTypeDefinition (typeDefJson: JObject) : ObjectTypeDefinition =
    let description = optionalText typeDefJson "description"
    let deprecationMessage = optionalText typeDefJson "deprecationMessage" 
    let properties = parseProperties typeDefJson
    { properties = properties
      description = description |> Option.map (fun desc -> desc.TrimEnd())
      deprecationMessage = deprecationMessage }

and parseType (typeJson: JObject) : SchemaType =
    if typeJson.ContainsKey "$ref" then
        match string typeJson.["$ref"] with
        | "pulumi.json#/Archive" -> SchemaType.Archive
        | "pulumi.json#/Asset" -> SchemaType.Asset
        | "pulumi.json#/Json" -> SchemaType.Json
        | "pulumi.json#/Any" -> SchemaType.Any
        | referencedType ->  SchemaType.Ref referencedType
    
    elif typeJson.ContainsKey "type" && typeJson.ContainsKey "enum" then
        match string typeJson.["type"] with
        | "string" ->
            let enumValues = typeJson.["enum"] :?> JArray
            let cases : EnumCase seq =
                enumValues
                |> Seq.filter (fun enumValue -> enumValue.Type = JTokenType.Object)
                |> Seq.cast<JObject>
                |> Seq.map (fun enumJson -> { 
                    value = text enumJson "value"
                    name = optionalText enumJson "name" 
                    description = optionalText enumJson "description" 
                    deprecationMessage = optionalText enumJson "deprecationMessage"
                })
        
            SchemaType.StringEnum (Seq.toList cases)
        
        | "integer" ->
            let enumValues = typeJson.["enum"] :?> JArray
            let cases : IntegerEnumCase seq =
                enumValues
                |> Seq.filter (fun enumValue -> enumValue.Type = JTokenType.Object)
                |> Seq.cast<JObject>
                |> Seq.map (fun enumJson -> { 
                    value = enumJson["value"].ToObject<int>()
                    name = optionalText enumJson "name" 
                    description = optionalText enumJson "description" 
                    deprecationMessage = optionalText enumJson "deprecationMessage"
                })
        
            SchemaType.IntegerEnum (Seq.toList cases)

        | otherwise ->
            failwithf "Invalid enum definition from type: %s" otherwise

    elif typeJson.ContainsKey "type" then 
        match string typeJson.["type"] with
        | "string" -> SchemaType.String
        | "number" -> SchemaType.Number
        | "integer" -> SchemaType.Integer
        | "boolean" -> SchemaType.Boolean
        | "array" ->
            let elementType = parseType (typeJson.["items"] :?> JObject)
            SchemaType.Array elementType
        | "object" when not (typeJson.ContainsKey "additionalProperties") ->
            let properties = parseProperties typeJson
            SchemaType.Object properties
        | "object" ->
            // assume additionalProperties is a type
            let additionalProperties = parseType (typeJson.["additionalProperties"] :?> JObject)
            SchemaType.Map additionalProperties
        | otherwise ->
            failwithf "Invalid type definition: %s" otherwise

    elif typeJson.ContainsKey "properties" then 
        let properties = parseProperties typeJson
        SchemaType.Object properties
    elif typeJson.ContainsKey "oneOf" then
        let oneOfTypes = typeJson.["oneOf"] :?> JArray
        oneOfTypes
        |> Seq.filter (fun typeJson -> typeJson.Type = JTokenType.Object)
        |> Seq.cast<JObject>
        |> Seq.map parseType
        |> Seq.toList
        |> SchemaType.Union
    else 
        failwithf "Invalid type definition: \n%s" (typeJson.ToString())

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
            Some (parseObjectTypeDefinition stateInputsObject)
        else
            None

    {
        token = token
        description = optionalText resourceJson "description"
        isComponent = readBoolean resourceJson "isComponent"
        isProvider = readBoolean resourceJson "isProvider"
        inputProperties = parseProperties "inputProperties"
        properties = parseProperties "properties"
        methods = methods
        stateInputs = stateInputs
    }

let parseFunction (token: string) (functionJson: JObject) : Function =
    try 
        let inputs = 
            if functionJson.ContainsKey "inputs" && functionJson.["inputs"].Type = JTokenType.Object then
                let inputsObject = functionJson.["inputs"] :?> JObject
                if inputsObject.Count = 0 then
                    None
                else
                    Some (parseObjectTypeDefinition inputsObject)
            else
                None

        let returnType = 
            if functionJson.ContainsKey "outputs" && functionJson.["outputs"].Type = JTokenType.Object then
                let outputsObject = functionJson.["outputs"] :?> JObject
                if outputsObject.Count = 0 then
                    SchemaType.Any
                else
                    parseType outputsObject
            else
                SchemaType.Any

        {
            token = token
            deprecationMessage = optionalText functionJson "deprecationMessage"
            description = optionalText functionJson "description"
            inputs = inputs
            multiArgumentInputs = optionalArrayText functionJson "multiArgumentInputs" |> Option.defaultValue []
            returnType = returnType
            isOverlay = readBoolean functionJson "isOverlay"
        }

    with ex -> 
        failwithf "Failed to parse function %s: %s" token ex.Message

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

let parseConfig (schemaJson: JObject) = 
    if schemaJson.ContainsKey "config" && schemaJson["config"].Type = JTokenType.Object then
        let configJson = schemaJson["config"] :?> JObject
        parseProperties configJson
    else 
        Map.empty

let parseProvider (name: string) functions (schemaJson: JObject) = 
    if schemaJson.ContainsKey "provider" && schemaJson["provider"].Type = JTokenType.Object then
        let providerJson = schemaJson["provider"] :?> JObject
        if providerJson.Count = 0 then
            None
        else
            Some (parseResource (sprintf "pulumi:providers:%s" name) functions providerJson)
    else 
        None

let parseSchema (json: string) : Schema =
    let schemaJson = JObject.Parse(json)
    let name = text schemaJson "name"
    let description = optionalText schemaJson "description"
    let displayName = optionalText schemaJson "displayName"
    let homepage = optionalText schemaJson "homepage"
    let license = optionalText schemaJson "license"
    let repository = optionalText schemaJson "repository"
    let keywords = optionalArrayText schemaJson "keywords" |> Option.defaultValue []
    let version = optionalText schemaJson "version"
    let publisher = optionalText schemaJson "publisher"
    let config = parseConfig schemaJson

    let functions = Map.ofList [
        if schemaJson.ContainsKey "functions" && schemaJson["functions"].Type = JTokenType.Object then 
            let functionsJson = schemaJson["functions"] :?> JObject
            for functionJson in functionsJson.Properties() do
                let functionTypeToken = canonicalToken functionJson.Name
                let functionDef = functionJson.Value :?> JObject
                functionTypeToken, parseFunction functionTypeToken functionDef
    ]

    let resources = Map.ofList [
        if schemaJson.ContainsKey "resources" && schemaJson["resources"].Type = JTokenType.Object then 
            let resourcesJson = schemaJson["resources"] :?> JObject
            for resoureJson in resourcesJson.Properties() do
                let resourceTypeToken = canonicalToken resoureJson.Name
                let resourceDef = resoureJson.Value :?> JObject
                resourceTypeToken, parseResource resourceTypeToken functions resourceDef
    ]

    let types = Map.ofList [
        if schemaJson.ContainsKey "types" && schemaJson["types"].Type = JTokenType.Object then 
            let typesJson = schemaJson["types"] :?> JObject
            for typeJson in typesJson.Properties() do
                let typeToken = canonicalToken typeJson.Name
                let typeDef = typeJson.Value :?> JObject
                typeToken, parseTypeDefinition typeDef
    ]

    let dotnetPackageInfo = parseDotnetPackageInfo schemaJson
    let provider = parseProvider name functions schemaJson

    { name = name
      displayName = displayName
      homepage = homepage
      license = license
      repository = repository
      publisher = publisher
      keywords = keywords
      config = config
      description = description
      resources = resources
      provider = provider
      functions = functions
      types = types
      dotnetPackageInfo = dotnetPackageInfo
      version = version }