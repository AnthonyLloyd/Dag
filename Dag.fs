namespace DagSystem

open System
open System.Threading.Tasks

type Dag = private {
    InputValues : obj array
    FunctionInputs : (Set<int> * Set<int>) array
    FunctionFunctions : (Dag -> Task<obj>) array
    FunctionValues : Lazy<Task<obj>> array
}

module Dag =
    let private append a v =
        let mutable a = a
        Array.Resize(&a, Array.length a + 1)
        a.[Array.length a - 1] <- v
        a

    let inline private taskMap f (t:Task<_>) =
        t.ContinueWith(fun (r:Task<_>) -> f r.Result)

    type Input = private | CellInput
    type Function = private | CellFunction

    type Cell<'a,'b> = private | Cell of int

    let empty = {
        InputValues = [||]
        FunctionInputs = [||]
        FunctionFunctions = [||]
        FunctionValues = [||]
    }

    let addInput (v:'a) (d:Dag) : Dag * Cell<'a, Input> =
        { d with
            InputValues = box v |> append d.InputValues
        }, Cell d.InputValues.Length

    let getValue (Cell key:Cell<'a,Input>) (d:Dag) : 'a =
        downcast d.InputValues.[key]

    let setInput (Cell key:Cell<'a,Input>) (a:'a) (d:Dag) : Dag =
        if downcast d.InputValues.[key] = a then d
        else
            let dirtyCalcs =
                Seq.fold (fun (j,s) (inputs,calcInputs) ->
                    if Set.contains key inputs ||
                       Set.intersect s calcInputs |> Set.isEmpty |> not then
                        j+1, Set.add j s
                    else
                        j+1, s
                ) (0,Set.empty) d.FunctionInputs
                |> snd

            let inputValues = Array.copy d.InputValues
            inputValues.[key] <- box a

            if Set.isEmpty dirtyCalcs then { d with InputValues = inputValues }
            else
                let functionValues = Array.copy d.FunctionValues
                let dag = {
                    d with
                        InputValues = inputValues
                        FunctionValues = functionValues
                }
                Set.iter (fun i ->
                    functionValues.[i] <- lazy d.FunctionFunctions.[i] dag
                ) dirtyCalcs
                dag

    let getValueTask (Cell key:Cell<'a,Function>) (d:Dag) : Task<'a> =
        d.FunctionValues.[key].Value |> taskMap (fun o -> downcast o)

    let changed (Cell key:Cell<'a,'t>) (before:Dag) (after:Dag) : bool =
        if typeof<'t> = typeof<Function> then
            before.FunctionValues.[key] <> after.FunctionValues.[key]
        else
            downcast before.InputValues.[key] <> downcast after.InputValues.[key]

    type 'a Builder = private {
        Dag : Dag
        Inputs : Set<int> * Set<int>
        Function : Dag -> Task<'a>
    }

    let buildFunction (d:Dag) f = {
        Dag = d
        Inputs = Set.empty, Set.empty
        Function = fun _ -> Task.FromResult f
    }

    let applyCell (Cell key:Cell<'a,'t>) {Dag=dag;Inputs=inI,inC;Function=bFn} =
        let isFunctionCell = typeof<'t> = typeof<Function>
        {
            Dag = dag
            Inputs =
                if isFunctionCell then inI, Set.add key inC
                              else Set.add key inI, inC
            Function =
                if isFunctionCell then
                    fun d ->
                        let fTask = bFn d
                        ( d.FunctionValues.[key].Value |> taskMap (fun o ->
                            taskMap (fun f -> downcast o |> f) fTask  )
                        ).Unwrap()
                else
                    fun d ->
                        bFn d |> taskMap (fun f ->
                            downcast d.InputValues.[key] |> f  )
        }

    let addFunction ({Dag=dag;Inputs=ips;Function=fn}:'a Builder) =
        let calc = fn >> taskMap box
        let d = {
            dag with
                FunctionInputs = append dag.FunctionInputs ips
                FunctionFunctions = append dag.FunctionFunctions calc
                FunctionValues = append dag.FunctionValues null
        }
        d.FunctionValues.[d.FunctionValues.Length-1] <- lazy calc d
        let cell : Cell<'a,Function> = Cell dag.FunctionValues.Length
        d, cell