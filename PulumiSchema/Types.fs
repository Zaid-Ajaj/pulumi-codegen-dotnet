module rec PulumiSchema.Types

type TypeDefinition = {
    schemaType: SchemaType
    description: string option
    deprecationMessage: string option
    secret: bool
}

[<RequireQualifiedAccess>]
type SchemaType = 
  | String
  | Number
  | Integer
  | Boolean
  | Asset
  | Archive
  | Ref of string
  | Array of SchemaType
  | Output of SchemaType
  | Map of SchemaType
  | Object of Map<string, TypeDefinition>

type Resource = {
    token: string
    description: string option
    isComponent: bool
    inputProperties: Map<string, TypeDefinition>
    requiredInputs: string list
    properties: Map<string, TypeDefinition>
}

type Schema = {
    name: string 
    description: string option
    resources: Map<string, Resource>
}