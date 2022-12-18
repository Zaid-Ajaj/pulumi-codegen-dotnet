open System.IO
open PulumiSchema 

let cwd = __SOURCE_DIRECTORY__
let schemaPath = Path.Combine(cwd, "random.json")
let schema = File.ReadAllText(schemaPath)
let parsedSchema = Parser.parseSchema schema
printfn "%A" parsedSchema