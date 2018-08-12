open System
open Expecto

let tests =
    testList "dag tests" [
        testAsync "simple" {
            ()
        } 
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests