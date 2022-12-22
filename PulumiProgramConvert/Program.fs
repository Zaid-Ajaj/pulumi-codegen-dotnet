open System
open System.IO
open CliWrap
open CliWrap.Buffered

let useTempDir (f: string -> 'a) : 'a =
    let tempDir = Path.GetTempPath()
    let tempDirName = Path.Combine(tempDir, Path.GetRandomFileName())
    try
        let info = Directory.CreateDirectory(tempDirName)
        f info.FullName
    finally
        Directory.Delete(tempDirName, true)

[<EntryPoint>]
let main (args: string[]) : int =
    if args = [| "--version" |] then
        printfn "0.1.0"
        0
    else
    let cwd = Directory.GetCurrentDirectory()
    let files = Directory.GetFiles(cwd) |> Array.map Path.GetFileName
    if not (Array.contains "Pulumi.yaml" files) then
        printfn "No Pulumi.yaml found in current directory"
        1
    else 
        let convertedProgram = useTempDir (fun tempDir -> 
            let homeDir = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            let devPulumiBuild = 
                if Environment.OSVersion.Platform = PlatformID.Unix then
                    Path.Combine(homeDir, ".pulumi-dev", "bin", "pulumi")
                else
                    Path.Combine(homeDir, ".pulumi-dev", "bin", "pulumi.exe")

            let convetCmd = 
                Cli.Wrap(devPulumiBuild)
                   .WithArguments($"convert --language json --out {tempDir}")
                   .WithEnvironmentVariables(readOnlyDict [ "PULUMI_EXPERIMENTAL", "1" ])
    
            let output = convetCmd.ExecuteBufferedAsync().GetAwaiter().GetResult()
            if output.ExitCode <> 0 then
                Error output.StandardError
            else
                let programJson = File.ReadAllText(Path.Combine(tempDir, "program.json"))
                Ok (programJson, PulumiConfigurationLanguage.Parser.parseProgram programJson)
        )

        match convertedProgram with 
        | Ok (syntaxJson, program) -> 
            match args with 
            | [| |] -> 
                printfn "No arguments provided"
                1
            | [| "--out"; outDir |] -> 
                let outDirectory = 
                    match args with
                    | [| "--out"; outDir |] -> 
                        if not (Directory.Exists(Path.Combine(cwd, outDir))) then
                            let _ = Directory.CreateDirectory(Path.Combine(cwd, outDir))
                            Path.Combine(cwd, outDir)
                        else
                            Path.Combine(cwd, outDir)
                        
                    |  _ -> cwd

                let converted = PulumiConfigurationLanguage.Converter.convertToCSharp program
                let convertedPath = Path.Combine(outDirectory, "converted-program.cs")
                File.WriteAllText(convertedPath, converted)
                0

            | [| "--show-syntax-tree" |] -> 
                printfn "%A" program
                0

            | [| "--show-pcl-json" |] ->
                printfn "%s" syntaxJson
                0
        
            | _ ->
                printfn "Invalid arguments"
                1


        | Error err ->
            printfn "Error converting program: %s" err
            1