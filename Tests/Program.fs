open System.IO
open PulumiSchema 
open Expecto
open Expecto.Logging

module PCL = PulumiConfigurationLanguage.Parser

let cwd = __SOURCE_DIRECTORY__
let schemas = Path.Combine(cwd, "schemas")

let tests = testList "Parsing" [

    test "Parse random-4.2.0" { 
        let schema = File.ReadAllText(Path.Combine(schemas, "random-4.2.0.json"))
        let parsedSchema = Parser.parseSchema schema
        Expect.equal parsedSchema.name "random" "The name of the schema is correct"
    }

    test "Parse aws-5.16.2" { 
        let schema = SchemaLoader.FromJsonFile(Path.Combine(schemas, "aws-5.16.2.json"))
        Expect.equal schema.name "aws" "The name of the schema is correct"
    }

    test "Parse aws-native-0.13.0" { 
        let schema = SchemaLoader.FromJsonFile(Path.Combine(schemas, "aws-native-0.13.0.json"))
        Expect.equal schema.name "aws-native" "The name of the schema is correct"
    }

    test "Parse azure-native-1.56.0" { 
        let schema = SchemaLoader.FromJsonFile(Path.Combine(schemas, "azure-native-1.56.0.json"))
        Expect.equal schema.name "azure-native" "The name of the schema is correct"
    }

    test "Parse awsx-1.0.0-beta.5" { 
        let schema = SchemaLoader.FromJsonFile(Path.Combine(schemas, "awsx-1.0.0-beta.5.json"))
        Expect.equal schema.name "awsx" "The name of the schema is correct"
    }

    test "Loading aws schema from Pulumi CLI using specific version" {
        let schema = SchemaLoader.FromPulumi("aws", "5.16.2")
        match schema with
        | Ok schema -> 
            Expect.equal schema.name "aws" "The name of the schema is correct"
            Expect.equal schema.version (Some "5.16.2") "The version of the schema is correct"
        | Result.Error errorMessage -> 
            failwith $"could not load schema aws@5.16.2: {errorMessage}"
    }

    test "Loading aws schema from Pulumi CLI using latest" {
        let schema = SchemaLoader.FromPulumi("aws")
        match schema with
        | Ok schema -> Expect.equal schema.name "aws" "The name of the schema is correct"
        | Result.Error errorMessage -> failwith $"could not load schema aws@5.16.2: {errorMessage}"
    }

    test "Parsing basic JSON AWS program works" {
        let program = File.ReadAllText(Path.Combine(cwd, "programs-json", "basic-aws.json")) 
        let parsedProgram = PCL.parseProgram program
        Expect.equal parsedProgram.nodes.Length 3 "Node count is correct"
    }

    test "Parsing webserver JSON AWS program works" {
        let program = File.ReadAllText(Path.Combine(cwd, "programs-json", "webserver.json")) 
        let parsedProgram = PCL.parseProgram program
        Expect.equal parsedProgram.nodes.Length 9 "Node count is correct"
    }
]

[<EntryPoint>]
let main argv = 
    let testConfig = { defaultConfig with verbosity = Verbose }
    runTestsWithArgs testConfig argv tests