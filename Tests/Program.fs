open System.IO
open PulumiSchema 
open Expecto
open Expecto.Logging

let cwd = __SOURCE_DIRECTORY__
let schemas = Path.Combine(cwd, "schemas")

let tests = testList "Schema parsing" [

    test "Parse random-4.2.0" { 
        let schema = File.ReadAllText(Path.Combine(schemas, "random-4.2.0.json"))
        let parsedSchema = Parser.parseSchema schema
        Expect.equal parsedSchema.name "random" "The name of the schema is correct"
    }

    test "Parse aws-5.16.2" { 
        let schema = File.ReadAllText(Path.Combine(schemas, "aws-5.16.2.json"))
        let parsedSchema = Parser.parseSchema schema
        Expect.equal parsedSchema.name "aws" "The name of the schema is correct"
    }

    test "Parse aws-native-0.13.0" { 
        let schema = File.ReadAllText(Path.Combine(schemas, "aws-native-0.13.0.json"))
        let parsedSchema = Parser.parseSchema schema
        Expect.equal parsedSchema.name "aws-native" "The name of the schema is correct"
    }

    test "Parse azure-native-1.56.0" { 
        let schema = File.ReadAllText(Path.Combine(schemas, "azure-native-1.56.0.json"))
        let parsedSchema = Parser.parseSchema schema
        Expect.equal parsedSchema.name "azure-native" "The name of the schema is correct"
    }

    test "Parse awsx-1.0.0-beta.5" { 
        let schema = File.ReadAllText(Path.Combine(schemas, "awsx-1.0.0-beta.5.json"))
        let parsedSchema = Parser.parseSchema schema
        Expect.equal parsedSchema.name "awsx" "The name of the schema is correct"
    }
]


[<EntryPoint>]
let main argv = 
    let testConfig = { defaultConfig with verbosity = Verbose }
    runTestsWithArgs testConfig argv tests