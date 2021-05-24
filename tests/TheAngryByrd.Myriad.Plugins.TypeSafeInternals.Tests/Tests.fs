namespace TypeSafeInternals.Tests

open System
open Expecto
open TypeSafeInternals
open TheAngryByrd.Myriad.Plugins.TypeSafeInternals




module RelctionPlaygroundTests =

    open System.Reflection

    let getVisibleRecordWithPrivateProperties (assembly : Assembly) =
        assembly.GetTypes()
        |> Array.filter(fun ty -> Reflection.FSharpType.IsRecord(ty,BindingFlags.NonPublic) && ty.IsVisible)
        |> Array.toList
    [<Tests>]
    let tests = testList "Reflection Playground" [
        testCase "Find Public Records with Private members" <| fun _ ->
            let assembly = Assembly.Load("Npgsql.FSharp")
            let records = getVisibleRecordWithPrivateProperties assembly
            // Debugging.waitForDebuggerAttached "tests"
            for r in records do
                for mem in r.GetProperties(BindingFlags.Instance ||| BindingFlags.NonPublic) do
                    printfn $"{r.FullName} -> {mem.Name}"
            Expect.equal records.Length 2 ""
    ]
module SayTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Add two integerss" <| fun _ ->

                printfn "%A" <| Npgsql.FSharp.Sql.p_defaultProps ()
                printfn "%A" <| Npgsql.FSharp.Sql.p_defaultConString()

                ()
        ]
