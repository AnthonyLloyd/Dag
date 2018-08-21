namespace DagSystem

open System
open System.Threading.Tasks

type Dag = private {
    InputKeys : obj array
    InputValues : obj array
    FunctionKeys : obj array
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

    type Cell<'a,'b> = private | Cell of obj

    let empty = {
        InputKeys = [||]
        InputValues = [||]
        FunctionKeys = [||]
        FunctionInputs = [||]
        FunctionFunctions = [||]
        FunctionValues = [||]
    }

    let addInput (v:'a) (d:Dag) : Dag * Cell<'a, Input> =
        let key = obj()
        { d with
            InputKeys = append d.InputKeys key
            InputValues = box v |> append d.InputValues
        }, Cell key

    let getValue (Cell key:Cell<'a,Input>) (d:Dag) : 'a =
        let i = Array.findIndex ((=)key) d.InputKeys
        downcast d.InputValues.[i]

    let setInput (Cell key:Cell<'a,Input>) (a:'a) (d:Dag) : Dag =
        let i = Array.findIndex ((=)key) d.InputKeys
        if downcast d.InputValues.[i] = a then d
        else
            let dirtyCalcs =
                Seq.fold (fun (j,s) (inputs,calcInputs) ->
                    if Set.contains i inputs ||
                       Set.intersect s calcInputs |> Set.isEmpty |> not then
                        j+1, Set.add j s
                    else
                        j+1, s
                ) (0,Set.empty) d.FunctionInputs
                |> snd

            let inputValues = Array.copy d.InputValues
            inputValues.[i] <- box a

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
        let i = Array.findIndex ((=)key) d.FunctionKeys
        d.FunctionValues.[i].Value |> taskMap (fun o -> downcast o)

    let changed (Cell key:Cell<'a,'t>) (before:Dag) (after:Dag) : bool =
        if typeof<'t> = typeof<Function> then
            let i = Array.findIndex ((=)key) before.FunctionKeys
            before.FunctionValues.[i] <> after.FunctionValues.[i]
        else
            let i = Array.findIndex ((=)key) before.InputKeys
            downcast before.InputValues.[i] <> downcast after.InputValues.[i]

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
        let i =
            if isFunctionCell then dag.FunctionKeys else dag.InputKeys
            |> Array.findIndex ((=)key)
        {
            Dag = dag
            Inputs =
                if isFunctionCell then inI, Set.add i inC
                              else Set.add i inI, inC
            Function =
                if isFunctionCell then
                    fun d ->
                        let fTask = bFn d
                        ( d.FunctionValues.[i].Value |> taskMap (fun o ->
                            taskMap (fun f -> downcast o |> f) fTask  )
                        ).Unwrap()
                else
                    fun d ->
                        bFn d |> taskMap (fun f ->
                            downcast d.InputValues.[i] |> f  )
        }

    let addFunction ({Dag=dag;Inputs=ips;Function=fn}:'a Builder) =
        let key = obj()
        let calc = fn >> taskMap box
        let d = {
            dag with
                FunctionKeys = append dag.FunctionKeys key
                FunctionInputs = append dag.FunctionInputs ips
                FunctionFunctions = append dag.FunctionFunctions calc
                FunctionValues = append dag.FunctionValues null
        }
        d.FunctionValues.[d.FunctionValues.Length-1] <- lazy calc d
        let cell : Cell<'a,Function> = Cell key
        d, cell