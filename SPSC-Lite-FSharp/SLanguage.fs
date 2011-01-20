namespace SPSC.SLanguage

open System

module List =
    // ToDo: Make tail-recursive and with StringBuilder
    let rec mkString separator l = 
        match l with
        | [] -> String.Empty
        | x :: [] -> x.ToString()
        | x1 :: x2 :: [] -> sprintf "%O%s%O" x1 separator x2
        | x :: xs -> sprintf "%O%s%s" x separator (mkString separator xs)
    let mkStringEnclosed left separator right l = sprintf "%s%s%s" left (mkString separator l) right
    

type Term = interface end

type Var (name:String) =
    member v.Name = name

    override x.ToString() = name

    interface Term
    interface IComparable with
        member x.CompareTo o = 
            match o with 
            | :? Var as v -> String.Compare (name, v.Name)
            | _ -> failwith "should be of type Var"

    override v.Equals(x) = name = (x :?> Var).Name

    override v.GetHashCode() = name.GetHashCode()

type TKind = Ctr = 0 | FCall = 1 | GCall = 2

type CFG (kind:TKind, name:String, args:Term list) =
    member x.ReplaceArgs (newArgs:Term list) = CFG(kind, name, newArgs)
    override x.ToString() = name + List.mkStringEnclosed "(" "," ")" args

    member x.Kind = kind
    member x.Name = name
    member x.Args = args

    static member Ctr  (name:String, args:Term list) = CFG(TKind.Ctr  , name, args) 
    static member FCall(name:String, args:Term list) = CFG(TKind.FCall, name, args) 
    static member GCall(name:String, args:Term list) = CFG(TKind.GCall, name, args)

    override v.Equals(x) = 
        let cfg = x :?> CFG
        kind = cfg.Kind && name = cfg.Name && args = cfg.Args

    interface Term

module CFGObject = 
    // ToDo: Make it static member of CFG
    // ToDo (Done): Make cfgObj of type Term
    let (|Ctr_|_|) (cfgObj:Term) = 
        match cfgObj with 
        | :? CFG as c when c.Kind = TKind.Ctr   -> Some (Ctr_(c.Name, c.Args))
        | _ -> None

    let (|FCall_|_|) (cfgObj:Term) = 
        match cfgObj with 
        | :? CFG as c when c.Kind = TKind.FCall -> Some (FCall_(c.Name, c.Args))
        | _ -> None

    let (|GCall_|_|) (cfgObj:Term) = 
        match cfgObj with 
        | :? CFG as c when c.Kind = TKind.GCall -> Some (GCall_(c.Name, c.Args))
        | _ -> None


(*
    let (|Ctr_|FCall_|GCall_|) (cfgObj:Term) = 
        match cfgObj with
        | :? CFG as x when x.Kind = TKind.Ctr -> Ctr_(x.Name, x.Args)
        | :? CFG as x when x.Kind = TKind.FCall -> FCall_(x.Name, x.Args)
        | :? CFG as x when x.Kind = TKind.GCall -> GCall_(x.Name, x.Args)
        | _ -> failwith "Should be of CFG type"
*)

(*
// ToDo (Done): This is very complicated. Make it simplier! 
type CFGObject(kind:TKind, name:String, args:Term list) = 
    // Note: why to inherit ((String, List[Term]) => CFG) ? Answer: E.g. now can construct FCall("some", args)
    // ToDo: make abstract

    member x.Apply (name:String, args:Term list) = new CFG(kind, name, args)
    member x.Unapply (e:CFG) = if (e.Kind = kind) then Some(e.Name, e.Args) else None

    member x.Name = name
    member x.Args = args

    interface Term

type Ctr(name:String, args:Term list) = inherit CFGObject(TKind.Ctr, name, args)
type FCall(name:String, args:Term list) = inherit CFGObject(TKind.FCall, name, args)
type GCall(name:String, args:Term list) = inherit CFGObject(TKind.GCall, name, args)

module CFGObjectMatch = 
    let (|Ctr_|FCall_|GCall_|) (cfgObj:Term) = 
        match cfgObj with
        | :? Ctr as x -> Ctr_(x.Name, x.Args)
        | :? FCall as x -> FCall_(x.Name, x.Args)
        | :? GCall as x -> GCall_(x.Name, x.Args)
        | _ -> failwith "Should be of CFGObject type"
*)

type Let(term:Term, bindings:(Var*Term) list) = 
    let bindings_s = List.map (fun (v,e) -> sprintf "%O=%O" v e) bindings 

    member x.Term = term
    member x.Bindings = bindings

    override x.ToString() = sprintf "let %s in %O" (List.mkString "," bindings_s) term

    interface Term

type Pat (name:String, args:Var list) = 
    member x.Name = name
    member x.Args = args

    override x.ToString() = name + List.mkStringEnclosed "(" "," ")" args

// ToDo: make abstract
type Rule(name:String) = class end

type FRule(name:String, args:Var list, term:Term) =
    inherit Rule(name)

    member x.Name = name
    member x.Args = args
    member x.Term = term

    override v.ToString() = sprintf "%s%s=%O;" name (List.mkStringEnclosed "(" "," ")" args) term

type GRule(name:String, p:Pat, args:Var list, term:Term) =
    inherit Rule(name)

    member x.Pat = p
    member x.Name = name
    member x.Args = args
    member x.Term = term

    override v.ToString() = 
        // Note: Why Pat does not inherit Term?
        // ToDo: Rewrite sprintf to avoid boxing of p and args.
        let args = ((box p) :: (List.map box args)) 
        sprintf "%s%s=%O;" name (List.mkStringEnclosed "(" "," ")" args) term 

type Program (rules: Rule list) = 
    let f = 
        let func (d:Rule) (m:Map<String, FRule>) = 
            match d with
            | :? FRule as df -> Map.add df.Name df m
            | _ -> m
        List.foldBack func rules Map.empty<String, FRule> 
    let g = 
        let func (d:Rule) (m:Map<(String*String), GRule>) = 
            match d with
            | :? GRule as df -> Map.add (df.Name, df.Pat.Name) df m
            | _ -> m
        List.foldBack func rules Map.empty<(String*String), GRule>
    let gs = 
        let func ((n:String),(_:String)) (d:GRule) (m:Map<String, GRule list>) = 
            let ds = 
                match Map.tryFind n m with
                | Some x -> x
                | None -> []
            Map.add n (d :: ds) m
        Map.foldBack func g Map.empty<String, GRule list>

    member x.GetF = f
    member x.GetG = g
    member x.GetGS = gs
    override x.ToString() = List.mkString String.Empty rules