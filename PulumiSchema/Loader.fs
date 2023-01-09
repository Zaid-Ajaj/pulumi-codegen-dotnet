namespace PulumiSchema

open System.IO
open CliWrap
open CliWrap.Buffered

type SchemaLoader() =
    /// <summary>
    /// Loads a Pulumi schema from a JSON file. Reads the content, then parses it.
    /// </summary>
    static member FromJsonFile(schemaPath: string) =
        let schema = File.ReadAllText(schemaPath)
        Parser.parseSchema schema

    /// <summary>
    /// Loads a Pulumi schema from a JSON string. Parses the content.
    /// </summary>
    static member FromJson(schemaJsonContent: string) =
        Parser.parseSchema schemaJsonContent

    /// <summary>
    /// Loads a Pulumi schema by shelling out to the Pulumi CLI and running `pulumi package get-schema {pluginName}@{version}`.
    /// If no version is specified or provided as "latest", the latest version will be retrieved.
    /// </summary>
    static member FromPulumi(pluginName: string, ?version: string) =
        let packageName = 
            match version with
            | Some "latest" -> pluginName
            | Some version -> sprintf "%s@%s" pluginName version
            | None -> pluginName

        let getSchema = Cli.Wrap("pulumi").WithArguments(sprintf "package get-schema %s" packageName).WithValidation(CommandResultValidation.None)
        let output = getSchema.ExecuteBufferedAsync().GetAwaiter().GetResult()
        if output.ExitCode <> 0 then
            Error output.StandardError
        else
            Ok (Parser.parseSchema output.StandardOutput)