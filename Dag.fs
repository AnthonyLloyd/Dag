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

    let inline private taskMap (f:'a->'b) (t:Task<'a>) =
        t.ContinueWith(fun (r:Task<'a>) -> f r.Result)

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

    let addInput (v:'a) (d:Dag) : Dag * Cell<'a, Input> =
        let key = obj()
        { d with
            InputKeys = append d.InputKeys key
            InputValues = append d.InputValues (box v)
        }, Cell key

    let getInput (Cell key:Cell<'a,Input>) (d:Dag) : 'a =
        let i = Array.findIndex ((=)key) d.InputKeys
        d.InputValues.[i] :?> 'a

    let setInput (Cell key:Cell<'a,Input>) (a:'a) (d:Dag) : Dag =
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
        d.CalculationValues.[i].Value |> taskMap (fun o -> downcast o)

    let changed (Cell key:Cell<'a,'t>) (before:Dag) (after:Dag) : bool =
        if typeof<'t> = typeof<Calculation> then
            let i = Array.findIndex ((=)key) before.CalculationKeys
            LanguagePrimitives.PhysicalEquality before.CalculationValues.[i] after.CalculationValues.[i] |> not
        else
            let i = Array.findIndex ((=)key) before.InputKeys
            (before.InputValues.[i] :?> 'a) <> (after.InputValues.[i] :?> 'a)

    type 'a Builder = {
        Dag : Dag
        Inputs : Set<int> * Set<int>
        Function : Dag -> Task<'a>
    }

    let build (d:Dag) f = {
        Dag = d
        Inputs = Set.empty, Set.empty
        Function = fun _ -> Task.FromResult f
    }

    let apply (Cell key:Cell<'a,'t>) (b:Builder<'a->'b>) =
        let isCalcInput = typeof<'t> = typeof<Calculation>
        let i =
            if isCalcInput then b.Dag.CalculationKeys else b.Dag.InputKeys
            |> Array.findIndex ((=)key)
        {
            Dag = b.Dag
            Inputs =
                match isCalcInput, b.Inputs with
                | true, (iInputs,cInputs) -> iInputs, Set.add i cInputs
                | false, (iInputs, cInputs) -> Set.add i iInputs, cInputs
            Function =
                if isCalcInput then
                    fun d ->
                        let fTask = b.Function d
                        ( d.CalculationValues.[i].Value
                          |> taskMap (fun o -> fTask |> taskMap (fun f -> downcast o |> f))
                        ).Unwrap()
                else
                    fun d ->
                        let fTask = b.Function d
                        fTask |> taskMap (fun f -> downcast d.InputValues.[i] |> f)
        }

    let addCalculation (b:Builder<'a>) : Dag * Cell<'b,Calculation> =
        let key = obj()
        let calc d = taskMap box (b.Function d)
        let dag = {
            b.Dag with
                CalculationKeys = append b.Dag.CalculationKeys key
                CalculationInputs = append b.Dag.CalculationInputs b.Inputs
                CalculationFunctions = append b.Dag.CalculationFunctions calc
                CalculationValues = append b.Dag.CalculationValues null
        }
        dag.CalculationValues.[dag.CalculationValues.Length-1] <- lazy calc dag
        dag, Cell key
