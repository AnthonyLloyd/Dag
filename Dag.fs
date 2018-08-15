namespace global

open System
open System.Threading.Tasks

type Dag = {
    InputKeys : obj array
    InputValues : obj array
    CalculationKeys : obj array
    CalculationInputs : (Set<int> * Set<int>) array
    CalculationFunctions : (Dag -> Task<obj>) array
    CalculationValues : Lazy<Task<obj>> array
}

module Dag =
    let private append a v =
        let mutable a = a
        Array.Resize(&a, Array.length a + 1)
        a.[Array.length a - 1] <- v
        a

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

    let add (v:'a) (d:Dag) : Dag * Cell<'a, Input> =
        let key = obj()
        { d with
            InputKeys = append d.InputKeys key
            InputValues = append d.InputValues (box v)
        }, Cell key

    let get (Cell key:Cell<'a,Input>) (d:Dag) : 'a =
        let i = Array.findIndex ((=)key) d.InputKeys
        d.InputValues.[i] :?> 'a

    let set (Cell key:Cell<'a,Input>) (a:'a) (d:Dag) : Dag =
        let i = Array.findIndex ((=)key) d.InputKeys
        if d.InputValues.[i] :?> 'a = a then d
        else
            let dirtyCalcs =
                Seq.fold (fun (j,s) (inputs,calcInputs) ->
                    if Set.contains i inputs || Set.intersect s calcInputs |> Set.isEmpty |> not then
                        (j+1),Set.add j s
                    else
                        (j+1),s
                ) (0,Set.empty) d.CalculationInputs
                |> snd

            let vs = Array.copy d.InputValues
            vs.[i] <- box a

            if Set.isEmpty dirtyCalcs then
                { d with
                    InputValues = vs
                }
            else
                let calcValues = Array.copy d.CalculationValues
                let dag = {
                    d with
                        InputValues = vs
                        CalculationValues = calcValues
                }
                Set.iter (fun i ->
                    let calc = d.CalculationFunctions.[i]
                    calcValues.[i] <- lazy calc dag
                ) dirtyCalcs
                dag

    let getAsync (Cell key:Cell<'a,Calculation>) (d:Dag) : Task<'a> =
        let i = Array.findIndex ((=)key) d.CalculationKeys
        d.CalculationValues.[i].Value.ContinueWith(fun (o:Task<obj>) -> o.Result :?> 'a)

    let add1 (f:'a->'b) (Cell dKey:Cell<'a,'t>) (d:Dag) : Dag * Cell<'b,Calculation> =
        let isCalcInput = typeof<'t> = typeof<Calculation>
        let i =
            if isCalcInput then d.CalculationKeys else d.InputKeys
            |> Array.findIndex ((=)dKey)
        let calc =
            if isCalcInput then
                fun (d:Dag) ->
                    d.CalculationValues.[i].Value.ContinueWith(fun (o:Task<obj>) -> o.Result :?> 'a |> f |> box)
            else
                fun (d:Dag) ->
                    Task.Run(fun () -> d.InputValues.[i] :?> 'a |> f |> box)
        let key = obj()
        let dag = {
            d with
                CalculationKeys = append d.CalculationKeys key
                CalculationInputs =
                    if isCalcInput then Set.empty, Set.singleton i else Set.singleton i, Set.empty
                    |> append d.CalculationInputs
                CalculationFunctions = append d.CalculationFunctions calc
                CalculationValues = append d.CalculationValues null
        }
        dag.CalculationValues.[dag.CalculationValues.Length-1] <- lazy calc dag
        dag, Cell key

    let changed (Cell key:Cell<'a,'t>) (before:Dag) (after:Dag) : bool =
        if typeof<'t> = typeof<Calculation> then
            let i = Array.findIndex ((=)key) before.CalculationKeys
            LanguagePrimitives.PhysicalEquality before.CalculationValues.[i] after.CalculationValues.[i] |> not
        else
            let i = Array.findIndex ((=)key) before.InputKeys
            (before.InputValues.[i] :?> 'a) <> (after.InputValues.[i] :?> 'a)
