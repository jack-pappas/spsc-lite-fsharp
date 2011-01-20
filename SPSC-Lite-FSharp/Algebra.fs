namespace SPSC

open System.Collections.Generic

module Algebra =
    open SPSC.SLanguage

    let shellEq (e1:CFG, e2:CFG) = 
        e1.Kind = e2.Kind && 
        e1.Name = e2.Name && 
        e1.Args.Length = e2.Args.Length

    let rec applySubst (m:Map<Var, Term>) (term:Term) : Term = 
        match term with
        | :? Var as v -> 
            match Map.tryFind v m with
            | Some x -> x
            | None -> v :> Term
        | :? CFG as e -> e.ReplaceArgs (List.map (applySubst m) e.Args) :> Term
        | _ -> failwith "unexpected type"

    let matchAgainst (t1:Term) (t2:Term) = 
        let map = new Dictionary<Var, Term>()
        let rec walk (t1:Term) (t2:Term) : bool = 
            match (t1, t2) with
            | (:? Var as v1, _) -> 
                match map.TryGetValue v1 with
                | true, t3 -> t2 = t3
                | false, _ -> map.Add(v1, t2) ; true
            | ((:? CFG as e1), (:? CFG as e2)) when shellEq(e1, e2) -> List.forall2 walk e1.Args e2.Args
            | _ -> false
        if not <| walk t1 t2 then None
        else (seq{ for key in map.Keys do yield (key, map.[key]) }) |> Map.ofSeq |> Option.Some

    let instOf (t1:Term) (t2:Term) = matchAgainst t2 t1 |> Option.isSome

    let equiv(t1:Term) (t2:Term) : bool = instOf t1 t2 && instOf t2 t1

    let rec vars (t:Term) : Var list = 
        match t with 
        | :? Var as v -> [v]
        | :? CFG as e -> 
            e.Args |> List.fold (fun vs exp ->
                        exp |> vars 
                            |> List.filter (fun x -> vs |> List.tryFind ((=) x) |> Option.isNone)
                            |> List.append vs) [] 
        | _ -> failwith "unexpected type"

    let private i = ref 0

    let resetVarGen() = i := 0

    let freshVar x = ( i := !i + 1 ; Var(sprintf "v%d" !i) )

    let isFGCall (expr:Term) : bool = 
        match expr with
        | CFGObject.Ctr_(_,_) -> true
        | _ -> false