# Pulumi Codgen in Dotnet

Hackathon project at Pulumi during Q4 of 2022. Streaming the progress live on [twitch.tv/zaid_ajaj](https://twitch.tv/zaid_ajaj).

- [x] Schema parsing
  - [x] Resources
  - [x] Functions
  - [x] Types
  - [x] Config
  - [x] Provider

- [x] Schema loading

- [ ] Client SDK-generation (for C#)

- [x] PCL-to-dotnet generation
  - [x] PCL parsing as a syntax tree
  - [x] PCL to dotnet code generation (partial)

# Development and Testing
```bash
# build the project
> dotnet build

# run the tests
> cd Tests
> dotnet run
```