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
  | Object of Map<string, Property>

type TypeDefinition = {
    schemaType: SchemaType
    description: string option
    deprecationMessage: string option
}

type Resource = {
    token: string
    description: string option
    isComponent: bool
    inputProperties: Map<string, Property>
    properties: Map<string, Property>
    methods: Map<string, Function>
    stateInputs: TypeDefinition option
}

type Function = {
    token: string
    description: string option
    deprecationMessage: string option
    inputs: Map<string, Property>
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
    description: string option
    resources: Map<string, Resource>
    functions: Map<string, Function>
    types: Map<string, TypeDefinition>
    dotnetPackageInfo: DotnetPackageInfo
    version: string option
}