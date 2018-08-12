namespace global

open System
open System.Threading.Tasks

module Dag =
    let private append a v =
        let mutable a = a
        Array.Resize(&a, Array.length a + 1)
        a.[Array.length a - 1] <- v
        a

    type CellValue =
    | Value of obj
    | Func of Lazy<Task<obj>>

    type Dag = {
        InputKeys : obj array
        InputValues : obj array
        CalculationKeys : obj array
        CalculationInputs : (int array * int array) array
        CalculationFunctions : (obj array -> obj array -> obj) array
        CalculationValues : Lazy<Task<obj>> array
    }

    type Input = CellInput
    type Calculation = CellCalculation

    type Cell<'a,'b> = Cell of obj

    let empty = {
        InputKeys = [||]
        InputValues = [||]
        CalculationKeys = [||]
        CalculationInputs = [||]
        CalculationFunctions = [||]
        CalculationValues = [||]
    }

    let add (d:Dag) (v:'a) : Dag * Cell<'a, Input> =
        let key = obj()
        let dag = {
          d with
            InputKeys = append d.InputKeys key
            InputValues = append d.InputValues (box v)
        }
        dag, Cell key

    let getValue (d:Dag) (Cell key:Cell<'a,Input>) : 'a =
        let i = Array.findIndex ((=)key) d.InputKeys
        d.InputValues.[i] :?> 'a

    // let add1 (d:Dag) (f:'a->'b) (Cell dKey:Cell<'a,'t>) : Dag * Cell<'b, Calculation> =
    //     let key = obj()
    //     let inputs = if typeof<'t> = typeof<Calculation> then
    //                     [||],[|Array.findIndex ((=)dKey) d.CalculationKeys|]
    //                  else [|Array.findIndex ((=)dKey) d.InputKeys|],[||]
    //     let calc =
    //         fun 
    //     let dag = {
    //         d with
    //             CalculationKeys = append d.CalculationKeys key
    //             CalculationInputs =
    //                 inputs
    //                 |> append d.CalculationInputs
    //     }
    //     let lf =
    //         lazy
    //             let a = getAsync dag a
    //             a.ContinueWith(fun (o:Task<'a>) -> f o.Result :> obj)
    //     dag.Values.[dag.Values.Length-1] <- Func lf
    //     dag, Cell key

    let getResult (d:Dag) (Cell key:Cell<'a,Calculation>) : Task<'a> =
        let i = Array.findIndex ((=)key) d.CalculationKeys
        d.CalculationValues.[i].Value.ContinueWith(fun (o:Task<obj>) -> o.Result :?> 'a)


    
//     let set (d:Dag) (Cell key:Cell<'a,CellTypeValue>) (a:'a) : Dag =
//         let i = Array.findIndex ((=)key) d.Keys
//         if d.Values.[i]=Value a then d
//         else
//             // Reset all Funcs
//             let vs = Array.copy d.Values
//             vs.[i] <- Value a
//             {
//                 Keys = d.Keys
//                 Values = vs
//             }


//     let change (before:Dag) (after:Dag) (n:Cell<'a,'t>) : 'a * 'a =
//         failwith "hi"

// module DagExample =
//     let test1() =
//         let dag, cellA = Dag.add Dag.empty 7
//         let dag, cellB = Dag.add dag "h"
//         let dag, cellC = Dag.add1 dag (fun s -> s + "i") cellB
//         let c1 = Dag.get dag cellC
//         let dag = Dag.set dag cellB "b"
//         // consider splitting cell into cellvalue and cellfunc
//         ()