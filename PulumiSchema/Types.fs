module rec PulumiSchema.Types

type DotnetPropertyInfo = {
    name: string
}

type Property = {
    schemaType: SchemaType
    description: string option
    deprecationMessage: string option
    secret: bool
    required: bool
    plain: bool
    replaceOnChanges: bool
    dotnetPropertyInfo: DotnetPropertyInfo option
}
  with 
  member this.ExpandSchemaType() = 
    if this.plain && this.required then 
        this.schemaType
    elif this.plain then
        SchemaType.Optional(this.schemaType)
    elif this.required then
        SchemaType.Output(this.schemaType)
    else
        SchemaType.Optional(SchemaType.Output(SchemaType.Optional(this.schemaType)))

type EnumCase = {
    name: string option
    description: string option
    deprecationMessage: string option
    value: string
}

type IntegerEnumCase = {
    name: string option
    description: string option
    deprecationMessage: string option
    value: int
}

[<RequireQualifiedAccess>]
type SchemaType = 
  | String
  | Number
  | Integer
  | Boolean
  | Asset
  | Archive
  | Any
  | Json
  | Ref of string
  | Optional of SchemaType
  | Array of SchemaType
  | Output of SchemaType
  | Map of SchemaType
  | StringEnum of EnumCase list
  | IntegerEnum of IntegerEnumCase list
  | Union of SchemaType list
  | Object of Map<string, Property>

type TypeDefinition = {
    schemaType: SchemaType
    description: string option
    deprecationMessage: string option
}

type ObjectTypeDefinition = {
    properties: Map<string, Property>
    description: string option
    deprecationMessage: string option
}

type Resource = {
    token: string
    description: string option
    isComponent: bool
    isProvider: bool
    inputProperties: Map<string, Property>
    properties: Map<string, Property>
    methods: Map<string, Function>
    stateInputs: ObjectTypeDefinition option
}

type Function = {
    token: string
    description: string option
    deprecationMessage: string option
    inputs: ObjectTypeDefinition option
    multiArgumentInputs: string list
    returnType: SchemaType
    isOverlay: bool
}

type DotnetPackageInfo = {
    namespaces: Map<string, string>
    packageReferences: Map<string, string>
    projectReferences: string list
    rootNamespace: string
    compatibility: string option
    respectSchemaVersion: bool
}

type Schema = {
    name: string 
    displayName: string option
    description: string option
    homepage: string option
    license : string option
    repository: string option
    publisher: string option
    keywords: string list
    resources: Map<string, Resource>
    provider: Resource option
    functions: Map<string, Function>
    config: Map<string, Property>
    types: Map<string, TypeDefinition>
    dotnetPackageInfo: DotnetPackageInfo
    version: string option
}