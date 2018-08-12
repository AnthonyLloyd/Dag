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
        ValueKeys : obj array
        ValueValues : obj array
        CalcKeys : obj array
        CalcValueDependencies : int array array
        CalcCalcDependencies : int array array
        CalcFunctions : (obj array -> obj array -> obj) array
        CalcValues : Lazy<Task<obj>> array
    }

    type Value = CellTypeValue
    type Function = CellTypeFunction

    type Cell<'a,'b> = Cell of obj

    let empty = {
        ValueKeys = [||]
        ValueValues = [||]
        CalcKeys = [||]
        CalcValueDependencies = [||]
        CalcCalcDependencies = [||]
        CalcFunctions = [||]
        CalcValues = [||]
    }

    let add (d:Dag) (v:'a) : Dag * Cell<'a, Value> =
        let key = obj()
        let dag = {
          d with
            ValueKeys = append d.ValueKeys key
            ValueValues = append d.ValueValues (box v)
        }
        dag, Cell key

    let getValue (d:Dag) (Cell key:Cell<'a,Value>) : 'a =
        let i = Array.findIndex ((=)key) d.ValueKeys
        d.ValueValues.[i] :?> 'a

    let getResult (d:Dag) (Cell key:Cell<'a,Function>) : Task<'a> =
        let i = Array.findIndex ((=)key) d.CalcKeys
        d.CalcValues.[i].Value.ContinueWith(fun (o:Task<obj>) -> o.Result :?> 'a)

//     let add1 (d:Dag) (f:'a->'b) (a:Cell<'a,'t>) : Dag * Cell<'b, Function> =
//         let key = obj()
//         let dag = {
//             d with
//                 CalcKeys = append d.CalcKeys key
//                 Values = insertAt d.Values d.Keys.Length (Func null)
//         }
//         let lf =
//             lazy
//                 let a = getAsync dag a
//                 a.ContinueWith(fun (o:Task<'a>) -> f o.Result :> obj)
//         dag.Values.[dag.Values.Length-1] <- Func lf
//         dag, Cell key
    
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