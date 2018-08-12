open System
open Expecto

let tests =
    testList "dag tests" [
        testAsync "one cell add" {
            let dag,cell1 = Dag.add Dag.empty 7
            Expect.equal 7 (Dag.getValue dag cell1) "one cell"
        }
        testAsync "two cell add" {
            let dag,cell1 = Dag.add Dag.empty 8
            let dag,cell2 = Dag.add dag 9
            Expect.equal 8 (Dag.getValue dag cell1) "first 8"
            Expect.equal 9 (Dag.getValue dag cell2) "second 9"
        }
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests