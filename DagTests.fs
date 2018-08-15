open System
open Expecto

let tests =
    testList "dag tests" [
        testAsync "one cell" {
            let dag,cell1 = Dag.add 7 Dag.empty
            Expect.equal 7 (Dag.get cell1 dag) "one cell"
        }
        testAsync "two cell" {
            let dag,cell1 = Dag.add 8 Dag.empty
            let dag,cell2 = Dag.add 9 dag
            Expect.equal 8 (Dag.get cell1 dag) "first 8"
            Expect.equal 9 (Dag.get cell2 dag) "second 9"
        }
        testAsync "one calc" {
            let dag,cell1 = Dag.add 42 Dag.empty
            let dag,cell2 = Dag.add1 ((*)10) cell1 dag
            Expect.equal 420 (Dag.getAsync cell2 dag).Result "42 * 10 = 420"
        }
        testAsync "one calc with set" {
            let dag,cell1 = Dag.add 42 Dag.empty
            let dag,cell2 = Dag.add1 ((*)10) cell1 dag
            let dag = Dag.set cell1 43 dag
            Expect.equal 430 (Dag.getAsync cell2 dag).Result "43 * 10 = 430"
        }
        testAsync "one calc with set twice" {
            let dag,cell1 = Dag.add 42 Dag.empty
            let dag,cell2 = Dag.add1 ((*)10) cell1 dag
            let dag = Dag.set cell1 43 dag
            let dag = Dag.set cell1 44 dag
            Expect.equal 440 (Dag.getAsync cell2 dag).Result "44 * 10 = 440"
        }
        testAsync "not changed input" {
            let dagBefore,cell1 = Dag.add 42 Dag.empty
            let dagAfter,_ = Dag.add 45 dagBefore
            Expect.isFalse (Dag.changed cell1 dagBefore dagAfter) "no change"
        }
        testAsync "changed input" {
            let dagBefore,cell1 = Dag.add 42 Dag.empty
            let dagAfter = Dag.set cell1 45 dagBefore
            Expect.isTrue (Dag.changed cell1 dagBefore dagAfter) "input changed"
        }
        testAsync "not changed calc" {
            let dag,cell1 = Dag.add 42 Dag.empty
            let dagBefore,cell2 = Dag.add1 ((*)10) cell1 dag
            let dagAfter,_ = Dag.add 45 dagBefore
            Expect.isFalse (Dag.changed cell2 dagBefore dagAfter) "no change"
        }
        testAsync "changed calc" {
            let dag,cell1 = Dag.add 42 Dag.empty
            let dagBefore,cell2 = Dag.add1 ((*)10) cell1 dag
            let dagAfter = Dag.set cell1 45 dagBefore
            Expect.isTrue (Dag.changed cell2 dagBefore dagAfter) "calc changed"
            Expect.equal 450 (Dag.getAsync cell2 dagAfter).Result "45 * 10 = 450"
            Expect.equal 420 (Dag.getAsync cell2 dagBefore).Result "42 * 10 = 420"
        }
        testAsync "two calc" {
            let dag,cell1 = Dag.add 42 Dag.empty
            let dag,cell2 = Dag.add1 ((*)10) cell1 dag
            let dagBefore,cell3 = Dag.add1 ((+)1) cell2 dag
            let dagAfter = Dag.set cell1 43 dagBefore
            Expect.equal 431 (Dag.getAsync cell3 dagAfter).Result "431"
            Expect.equal 421 (Dag.getAsync cell3 dagBefore).Result "421"
        }
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests