namespace SPSC.BasicSuperCompiler

open SPSC
open SPSC.SLanguage
open SPSC.ProcessTree

type AdvancedSuperCompiler(p:Program) =
    inherit BasicSuperCompiler(p)

    let abs (t:Tree, a:Node, b:Node) = 
        (fun (g:Gen) -> t.Replace(a, Let(g.T, g.M1 |> Map.toList))) (MSG.msg(a.Expr, b.Expr))

    let split (t:Tree, n:Node) = 
        match n.Expr with
        | :? CFG as e -> 
            let vs = List.map Algebra.freshVar e.Args
            t.Replace(n, Let(e.ReplaceArgs (List.map (fun x -> x :> Term) vs), List.zip vs e.Args))
        | _ -> failwith "unexpected"

    member x.BuildProcessTree (e:Term) : Tree = 
        let t = Tree.Create(e) |> ref
        while List.exists (fun (n:Node) -> n.IsProcessed |> not) (t.Value.Leaves()) do
            let b = List.find (fun (n:Node) -> n.IsProcessed |> not) ((!t).Leaves())
            t := 
                if b.Expr |> Algebra.isFGCall |> not then
                    t.Value.AddChildren(b, x.DriveExp(b.Expr))
                else
                    match (b.Ancetors()) |>
                        List.tryFind (fun (a:Node) -> Algebra.isFGCall a.Expr && HE.embeddedIn (a.Expr, b.Expr)) with
                    | Some a -> 
                        if Algebra.instOf b.Expr a.Expr then abs(!t, b, a)
                        elif Algebra.equiv (MSG.msg(a.Expr, b.Expr).T) (Var("z")) then split(!t, b)
                        else abs(!t, a, b)
                    | None -> t.Value.AddChildren(b, x.DriveExp(b.Expr))
        !t