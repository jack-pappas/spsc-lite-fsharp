namespace SPSC.BasicSuperCompiler

open SPSC
open SPSC.SLanguage
open SPSC.ProcessTree

type BasicSuperCompiler(p:Program) = 
    member x.FreshPat (p:Pat) = Pat(p.Name, List.map Algebra.freshVar p.Args)

    member x.DriveExp(expr:Term) : (Term*Contraction option) list = 
        match expr with
        | CFGObject.Ctr_(name, args) -> List.map (fun x -> (x,None)) args
        | CFGObject.FCall_(name, args) -> 
            let varTermMap = Map.ofList (List.zip (Map.find name p.GetF).Args args)
            let term = (Map.find name p.GetF).Term
            let subst = Algebra.applySubst varTermMap term
            [ (subst, None) ]
        | CFGObject.GCall_(name, CFGObject.Ctr_(cname, cargs) :: args ) -> 
            let g = Map.find (name, cname) p.GetG
            let varTermMap = Map.ofList (List.zip (g.Pat.Args @ g.Args) (cargs @ args))
            let subst = Algebra.applySubst varTermMap g.Term
            [ (subst, None) ]
        | CFGObject.GCall_(name, (:? Var as v) :: args) as gCall -> 
            [
                let gs = Map.find name p.GetGS
                for g in gs do
                    let fp = x.FreshPat g.Pat
                    let ctr = CFG.Ctr(fp.Name, List.map (fun x -> x :> Term) fp.Args) // ToDo: Implement implicit boxing on lists
                    let subst = Algebra.applySubst (Map.ofList [(v, ctr :> Term)]) gCall
                    let drivedExp = x.DriveExp(subst)
                    match drivedExp with
                    | (k,_) :: _ -> yield (k, Some (Contraction (v, fp)))
                    | _ -> failwith "Unexpected structure of drivedExp"
            ]
        | CFGObject.GCall_(name, args) -> 
            List.map (fun (k, v) -> (CFG.GCall(name, k :: args.Tail) :> Term, v)) (x.DriveExp(args.Item(0)))
        | :? Let as l -> (l.Term, None) :: (List.map (fun (_, v) -> (v, None)) l.Bindings)
        | _ -> failwith "Other matches are unexpected"

    member x.BuildProcessTree(e:Term) : Tree = 
        let t = Tree.Create(e) |> ref 
        while List.exists (fun (n:Node) -> n.IsProcessed |> not) ((!t).Leaves()) do
            let b = List.find (fun (n:Node) -> n.IsProcessed |> not) ((!t).Leaves())
            t := 
                match (b.Ancetors()) |>
                    List.tryFind (fun (a:Node) -> Algebra.isFGCall a.Expr && Algebra.instOf b.Expr a.Expr) with
                | Some a -> 
                    let mapRef = Algebra.matchAgainst a.Expr b.Expr
                    let letTerm = Let(a.Expr, mapRef.Value |> Map.toList)
                    (!t).Replece(b, letTerm :> Term)
                | None -> (!t).AddChildren(b, x.DriveExp(b.Expr))
        !t