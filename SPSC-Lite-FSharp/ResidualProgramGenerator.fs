module SPSC.ResidualProgramGenerator

open SPSC
open SPSC.ProcessTree
open SPSC.SLanguage

type ResidualProgramGenerator(tree:Tree) =
    let sigs = new System.Collections.Generic.Dictionary<Node, (string * Var list)>()
    let defs = new System.Collections.Generic.List<Rule>()

    let rename(f:string, b:string) = sprintf "%s%s%d" b (f.Substring 1) (sigs.Count + 1)

    let rec walkCall (n:Node, name:string, args:Term list) = 
        let vs = Algebra.vars n.Expr
        match (tree.FindChildren n).Head.Contraction with
        | Some contraction -> 
            let gname = 
                match sigs.TryGetValue(n) with 
                | true,(x,_) -> x
                | false,_ -> 
                    let s = rename(name, "g")
                    sigs.Add(n, (s,vs))
                    s
            for cn in tree.FindChildren n do
                defs.Add(GRule(gname, cn.Contraction.Value.Pat, vs.Tail, walk(cn)))
            CFG.GCall(gname, vs |> List.map (fun x -> x :> Term)) :> Term // ToDo: reconsider this.
        | None when tree.Leaves() |> List.exists (fun x -> match x.FuncAncetor() with None -> false | Some x -> x = n) -> 
            let fname,fargs = 
                match sigs.TryGetValue(n) with 
                | true,x -> x
                | false,_ -> 
                    let x = rename(name, "g"),vs
                    (sigs.Add(n, x) ; x)
            defs.Add(FRule(fname, fargs, walk (tree.FindChildren n).Head))
            CFG.FCall(fname, vs |> List.map (fun x -> x :> Term)) :> Term
        | None -> walk (tree.FindChildren n).Head

    and walk (n:Node) : Term =
        match n.FuncAncetor() with
        | None -> 
            match n.Expr with
            | :? Var as v -> v :> Term
            | :? Let as l -> 
                let c = tree.FindChildren n
                let body = walk(c.Head)
                let substs = (List.zip (l.Bindings |> List.map fst) (c.Tail |> List.map walk)) |> Map.ofList
                Algebra.applySubst substs body
            | CFGObject.Ctr_(name,_) -> CFG.Ctr(name, n |> tree.FindChildren |> List.map walk) :> Term
            | CFGObject.FCall_(name, args) -> walkCall(n, name, args)
            | CFGObject.GCall_(name, args) -> walkCall(n, name, args)
            | _ -> failwith "unexpected type"
        | Some ancetor -> 
            match sigs.[ancetor] with 
            | (name, args) when (tree.FindChildren ancetor).Head.Contraction |> Option.isSome -> 
                Algebra.applySubst 
                    (Algebra.matchAgainst ancetor.Expr n.Expr).Value 
                    (CFG.FCall(name, args |> List.map (fun x -> x :> Term))) // ToDo: Reconsider this.
            | (name, args) -> 
                Algebra.applySubst 
                    (Algebra.matchAgainst ancetor.Expr n.Expr).Value 
                    (CFG.GCall(name, args |> List.map (fun x -> x :> Term))) // ToDo: Reconsider this.

    let result = lazy (walk(tree.Root), Program(List.ofSeq defs))
    member x.Result = result.Value
