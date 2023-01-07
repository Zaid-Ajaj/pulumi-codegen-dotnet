open System
open System.IO
open System.Linq
open Fake.IO
open Fake.Core

/// Recursively tries to find the parent of a file starting from a directory
let rec findParent (directory: string) (fileToFind: string) = 
    let path = if Directory.Exists(directory) then directory else Directory.GetParent(directory).FullName
    let files = Directory.GetFiles(path)
    if files.Any(fun file -> Path.GetFileName(file).ToLower() = fileToFind.ToLower()) 
    then path 
    else findParent (DirectoryInfo(path).Parent.FullName) fileToFind

let repositoryRoot = findParent __SOURCE_DIRECTORY__ "README.md";

let pulumiProgramConvert = Path.Combine(repositoryRoot, "PulumiProgramConvert")

/// <summary>
/// Packs Snowflaqe CLI and installs it locally for testing
/// </summary>
let pack() =
    Shell.deleteDir (Path.Combine(pulumiProgramConvert,"bin"))
    Shell.deleteDir (Path.Combine(pulumiProgramConvert,"obj"))
    if Shell.Exec("dotnet", "pack --configuration Release", pulumiProgramConvert) <> 0 then
        failwith "Pack failed"
    else
        let outputPath = Path.Combine(pulumiProgramConvert, "bin", "Release")
        try
            // try get the version to see if Snowflaqe is already installed
            Shell.Exec("pulumi-program-convert", "--version") |> ignore
            printfn "pulumi-program-convert is already installed globally, uninstalling..."
            if Shell.Exec("dotnet", "tool uninstall PulumiProgramConvert -g") = 0 then
                if Shell.Exec("dotnet", sprintf "tool install -g PulumiProgramConvert --add-source %s" outputPath) <> 0
                then failwith "Local install failed"
            else
                failwith "Failed to uninstall existing pulumi-program-convert"
        with
        | _ ->
            // install snowflaqe
            if Shell.Exec("dotnet", sprintf "tool install -g PulumiProgramConvert --add-source %s" outputPath) <> 0
            then failwith "Local install failed"

let publish projectDir =
    Path.Combine(projectDir, "bin") |> Shell.deleteDir
    Path.Combine(projectDir; "obj") |> Shell.deleteDir

    if Shell.Exec("dotnet", "pack --configuration Release", projectDir) <> 0 then
        failwithf "Packing '%s' failed" projectDir
    else
        let nugetKey =
            match Environment.environVarOrNone "NUGET_KEY" with
            | Some nugetKey -> nugetKey
            | None -> 
                printfn "The Nuget API key was not found in a NUGET_KEY environmental variable"
                printf "Enter NUGET_KEY: "
                Console.ReadLine()

        let nugetPath =
            Directory.GetFiles(Path.Combine(projectDir, "bin", "Release"))
            |> Seq.head
            |> Path.GetFullPath

        if Shell.Exec("dotnet", sprintf "nuget push %s -s https://api.nuget.org/v3/index.json -k %s" nugetPath nugetKey, projectDir) <> 0
        then failwith "Publish failed"

[<EntryPoint>]
let main (args: string[]) : int =
    match args with
    | [| "pack" |] -> pack()
    | [| "publish-schema-types" |] -> publish (Path.Combine(repositoryRoot, "PulumiSchemaTypes"))
    | [| "publish-schema" |] -> publish (Path.Combine(repositoryRoot, "PulumiSchema"))
    | _ -> printfn "No valid arguments provided"

    0