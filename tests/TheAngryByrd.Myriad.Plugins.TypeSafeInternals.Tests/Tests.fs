namespace TypeSafeInternals.Tests

open System
open Expecto
open TypeSafeInternals
open TheAngryByrd.Myriad.Plugins.TypeSafeInternals


module SayTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Add two integerss" <| fun _ ->

                printfn "%A" <| Npgsql.FSharp.Sql.p_defaultProps ()

                ()
        ]
