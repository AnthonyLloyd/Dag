open DagSystem
open Expecto

let tests =
    testList "dag tests" [
        testAsync "one cell" {
            let dag, cell1 = Dag.addInput 7 Dag.empty
            Expect.equal 7 (Dag.getValue cell1 dag) "one cell"
        }
        testAsync "two cell" {
            let dag, cell1 = Dag.addInput 8 Dag.empty
            let dag, cell2 = Dag.addInput 9 dag
            Expect.equal 8 (Dag.getValue cell1 dag) "first 8"
            Expect.equal 9 (Dag.getValue cell2 dag) "second 9"
        }
        testAsync "one function" {
            let dag, cell1 = Dag.addInput 42 Dag.empty
            let dag, cell2 =
                Dag.buildFunction dag (fun x -> x * 10)
                |> Dag.applyCell cell1
                |> Dag.addFunction
            let! result = Dag.getValueTask cell2 dag |> Async.AwaitTask
            Expect.equal 420 result "42 * 10 = 420"
        }
        testAsync "one function with set" {
            let dag, cell1 = Dag.addInput 13 Dag.empty
            let dag, cell2 =
                Dag.buildFunction dag (fun x -> x * 10)
                |> Dag.applyCell cell1
                |> Dag.addFunction
            let dag = Dag.setInput cell1 43 dag
            let! result = Dag.getValueTask cell2 dag |> Async.AwaitTask
            Expect.equal 430 result "43 * 10 = 430"
        }
        testAsync "one function with set twice" {
            let dag, cell1 = Dag.addInput 15 Dag.empty
            let dag, cell2 =
                Dag.buildFunction dag (fun x -> x * 10)
                |> Dag.applyCell cell1
                |> Dag.addFunction
            let dag = Dag.setInput cell1 43 dag
            let dag = Dag.setInput cell1 44 dag
            let! result = Dag.getValueTask cell2 dag |> Async.AwaitTask
            Expect.equal 440 result "44 * 10 = 440"
        }
        testAsync "not changed input" {
            let dagBefore, cell1 = Dag.addInput 42 Dag.empty
            let dagAfter,_ = Dag.addInput 45 dagBefore
            Expect.isFalse (Dag.changed cell1 dagBefore dagAfter) "no change"
        }
        testAsync "changed input" {
            let dagBefore, cell1 = Dag.addInput 42 Dag.empty
            let dagAfter = Dag.setInput cell1 45 dagBefore
            Expect.isTrue (Dag.changed cell1 dagBefore dagAfter) "changed"
        }
        testAsync "not changed function" {
            let dag, cell1 = Dag.addInput 42 Dag.empty
            let dagBefore, cell2 =
                Dag.buildFunction dag (fun x -> x * 10)
                |> Dag.applyCell cell1
                |> Dag.addFunction
            let dagAfter,_ = Dag.addInput 45 dagBefore
            Expect.isFalse (Dag.changed cell2 dagBefore dagAfter) "no change"
        }
        testAsync "changed function" {
            let dag, cell1 = Dag.addInput 17 Dag.empty
            let dagBefore, cell2 =
                Dag.buildFunction dag (fun x -> x * 10)
                |> Dag.applyCell cell1
                |> Dag.addFunction
            let dagAfter = Dag.setInput cell1 45 dagBefore
            Expect.isTrue (Dag.changed cell2 dagBefore dagAfter) "changed"
            let! result = Dag.getValueTask cell2 dagAfter |> Async.AwaitTask
            Expect.equal 450 result "45 * 10 = 450"
            let! result = Dag.getValueTask cell2 dagBefore |> Async.AwaitTask
            Expect.equal 170 result "17 * 10 = 170"
        }
        testAsync "chained functions" {
            let dag, cell1 = Dag.addInput 18 Dag.empty
            let dag, cell2 =
                Dag.buildFunction dag (fun x -> x * 10)
                |> Dag.applyCell cell1
                |> Dag.addFunction
            let dagBefore, cell3 =
                Dag.buildFunction dag (fun x -> x + 1)
                |> Dag.applyCell cell2
                |> Dag.addFunction
            let dagAfter = Dag.setInput cell1 23 dagBefore
            let! result = Dag.getValueTask cell3 dagAfter |> Async.AwaitTask
            Expect.equal 231 result "231"
            let! result = Dag.getValueTask cell3 dagBefore |> Async.AwaitTask
            Expect.equal 181 result "181"
        }
        testAsync "two function" {
            let dag, cell1 = Dag.addInput "z" Dag.empty
            let dag, cell2 = Dag.addInput 7 dag
            let dag, cell3 =
                Dag.buildFunction dag (fun s (n:int) -> s + string n)
                |> Dag.applyCell cell1
                |> Dag.applyCell cell2
                |> Dag.addFunction
            let! result = Dag.getValueTask cell3 dag |> Async.AwaitTask
            Expect.equal "z7" result "z7"
        }
        testAsync "three function with set" {
            let dag, cell1 = Dag.addInput "f" Dag.empty
            let dag, cell2 = Dag.addInput 8 dag
            let dag, cell3 = Dag.addInput 1.5 dag
            let dag, cell4 =
                Dag.buildFunction dag
                    (fun s (n:int) (f:float) -> s + string n + "-" + string f)
                |> Dag.applyCell cell1
                |> Dag.applyCell cell2
                |> Dag.applyCell cell3
                |> Dag.addFunction
            let! result = Dag.getValueTask cell4 dag |> Async.AwaitTask
            Expect.equal "f8-1.5" result "f8-1.5"
            let dag = Dag.setInput cell1 "w" dag
            let! result = Dag.getValueTask cell4 dag |> Async.AwaitTask
            Expect.equal "w8-1.5" result "w8-1.5"
        }
        testAsync "chained functions multi" {
            let dag, cell1 = Dag.addInput "a" Dag.empty
            let dag, cell2 = Dag.addInput 1 dag
            let dag, cell3 =
                Dag.buildFunction dag (fun s (n:int) -> "x:" + s + string n)
                |> Dag.applyCell cell1
                |> Dag.applyCell cell2
                |> Dag.addFunction
            let dag, cell4 = Dag.addInput "b" dag
            let dag, cell5 = Dag.addInput 2 dag
            let dag, cell6 =
                Dag.buildFunction dag (fun s (n:int) -> "y:" + s + string n)
                |> Dag.applyCell cell4
                |> Dag.applyCell cell5
                |> Dag.addFunction
            let dag, cell7 = Dag.addInput "c" dag
            let dag, cell8 = Dag.addInput 3 dag
            let dag, cell9 =
                Dag.buildFunction dag (fun s (n:int) -> "z:" + s + string n)
                |> Dag.applyCell cell7
                |> Dag.applyCell cell8
                |> Dag.addFunction
            let dagBefore, cell10 =
                Dag.buildFunction dag
                    (fun s1 s2 s3 -> String.concat "|" [s1;s2;s3])
                |> Dag.applyCell cell3
                |> Dag.applyCell cell6
                |> Dag.applyCell cell9
                |> Dag.addFunction
            let dagAfter = Dag.setInput cell5 4 dagBefore
            let! result = Dag.getValueTask cell10 dagAfter |> Async.AwaitTask
            Expect.equal "x:a1|y:b4|z:c3" result "x:a1|y:b4|z:c3"
            let! result = Dag.getValueTask cell10 dagBefore |> Async.AwaitTask
            Expect.equal "x:a1|y:b2|z:c3" result "x:a1|y:b2|z:c3"
        }
    ]

[<EntryPoint>]
let main args = runTestsWithArgs defaultConfig args tests