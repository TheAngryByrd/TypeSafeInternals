namespace TypeSafeInternals.Tests

open System
open Expecto
open TypeSafeInternals
open TheAngryByrd.Myriad.Plugins.TypeSafeInternals


module GenerationTests =
    [<Tests>]
    let tests =
        testList "Generation tests" [
            testCase "Can generate correct module paths to static functions"
            <| fun _ ->

                printfn "%A"
                <| Npgsql.FSharp.Sql.p_defaultProps ()

                printfn "%A"
                <| Npgsql.FSharp.Sql.p_defaultConString ()

            testCase "Can create getter for Visible Records with Internal properties"
            <| fun _ ->

                let props = Npgsql.FSharp.Sql.p_defaultProps ()
                let ct = Npgsql.FSharp.Sql.SqlProps.get_CancellationToken props

                Expect.equal System.Threading.CancellationToken.None ct ""
        ]
