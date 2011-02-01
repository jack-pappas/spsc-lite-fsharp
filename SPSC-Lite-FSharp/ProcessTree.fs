namespace SPSC.ProcessTree

open System
open System.Text

open SPSC
open SPSC.SLanguage

type Contraction (v:Var, pat:Pat) = 
    override x.ToString() = sprintf "%O=%O" v pat

    member x.Pat = pat
    member x.Var = v

type Node (nodeId:int, expr:Term, parent:Node option, contr:Contraction option) =
    member x.Expr = expr
    member x.Parent = parent
    member x.Contraction = contr
    member x.NodeId = nodeId

    member x.Ancetors() : Node list = 
        match parent with 
        | Some p -> p :: p.Ancetors()
        | None -> []

    member x.FuncAncetor() = 
        List.tryFind (fun (n:Node) -> Algebra.isFGCall n.Expr && Algebra.equiv expr n.Expr) (x.Ancetors())

    member x.IsProcessed = 
        match expr with
        | CFGObject.Ctr_(_, []) -> true
        | :? Var as v -> true
        | _ -> x.FuncAncetor() |> Option.isSome

    interface IComparable with
        member x.CompareTo o = 
            let node = o :?> Node
            if   nodeId > node.NodeId then 1 
            elif nodeId < node.NodeId then -1 
            else 0

    override x.Equals o = nodeId = (o :?> Node).NodeId

    override x.ToString () = sprintf "[node:%d]" nodeId

type Tree(freeId:int, root:Node, children:Map<Node, Node list>) =
    member x.Root = root
    //member x.Children = children
    member x.FindChildren n = children |> Map.find n

    // ToDo: How to make 'cs:(#Term*Contraction option) list' ? 
    member x.AddChildren(n:Node, cs:(Term*Contraction option) list) =
        let i = ref (freeId - 1)
        new Tree(freeId + cs.Length, 
                 root, 
                 Map.add n (List.map (fun (t,b) -> (i:=!i+1 ; new Node(!i, t, Some n, b))) cs) children)

    member x.Replace (n:Node, exp:Term) = 
        if (n = root) then new Tree(freeId, n, Map.empty)
        else
            let p = n.Parent |> Option.get
            let cs = List.map 
                        (fun m -> if m = n then new Node(freeId, exp, Some p, n.Contraction) else m) 
                        (Map.find p children)
            new Tree(freeId+1, root, Map.add p cs children)

    member x.Leaves_(node:Node) : Node list = 
        match Map.tryFind node children with
        | Some ns when ns |> List.isEmpty -> [node]
        | Some ns -> List.concat (List.map x.Leaves_ (Map.find node children))
        | None -> [node]

    member x.Leaves() = x.Leaves_(root)

    override x.ToString() : String = 
        let acc = new StringBuilder()
        let rec walk (n:Node) : unit = 
            let parentId_s = match n.Parent with None -> String.Empty | Some x -> x.NodeId.ToString() 
            let children_s = 
                List.mkString "," 
                    [ match Map.tryFind n children with
                        | Some ns -> for child in ns do yield child.NodeId.ToString() 
                        | None -> yield String.Empty]
            let contr_s = match n.Contraction with None -> String.Empty | Some x -> x.ToString()
            let node_s = sprintf "%d:(%O,%s,%s,[%s])" n.NodeId n.Expr contr_s parentId_s children_s

            if acc.Length > 0 then acc.Append "," |> ignore
            acc.Append node_s |> ignore

            match Map.tryFind n children with 
            | Some ns -> List.iter walk ns
            | None -> ()
        acc.Append "{"  |> ignore
        walk root
        (acc.Append "}").ToString()

    static member Create(e:Term) = new Tree(1, new Node(0, e, None, None), Map.empty)