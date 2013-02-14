module Supercompiler

open FSharpx    // For State workflow
open FSharpx.DataStructures // IntMap
open FSharpx.DataStructures.IntMap  // TEMP : Rewrite code below to use qualified access
open SLanguage
open ShowUtil
open SParsers
open Algebra
open MSG
open HE
open ProcessTree


//
let lookupF (Program rules) name =
    rules
    |> List.choose (function
        | FRule (name', ``params``, body)
            when name = name' ->
            Some (``params``, body)
        | _ ->
            None)
    |> List.head

// lookupGC :: Program -> Name -> Name -> (Params, Params, Exp)
let lookupGC (Program rules) (name : Name) (cname : Name) : Params * Params * Exp =
    rules
    |> List.choose (function
        | GRule (name', cname', cparams, ``params``, body)
            when name = name' && cname = cname' ->
            Some (cparams, ``params``, body)
        | _ ->
            None)
    |> List.head

// lookupG :: Program -> Name -> [(Name, Params, Params, Exp)]
let lookupG (Program rules) (name : Name) : (Name * Params * Params * Exp) list =
    rules
    |> List.choose (function
        | GRule (name', cname, cparams, ``params``, body)
            when name' = name ->
            Some (cname, cparams, ``params``, body)
        | _ ->
            None)


(* Driving *)

// drivingStep :: Program -> Exp -> State Int [Branch]
let rec drivingStep (prog : Program) (e : Exp) : State.State<Branch list, int> =
    State.state {
    match e with
    | Call (Ctr, name, args) ->
        return List.map (fun arg -> arg, None) args
    
    | Call (FCall, name, args) ->
        let ``params``, body = lookupF prog name
        let p2a = Map.ofList (List.zip ``params`` args)
        let body' = applySubst p2a body
        return [(body', None)]

    | Call (GCall, name, Call (Ctr, cname, cargs) :: args) ->
        let cparams, ``params``, body = lookupGC prog name cname
        let cp2ca = Map.ofList (List.zip cparams cargs)
        let p2a = Map.ofList (List.zip ``params`` args)
        let body' = applySubst (Map.union cp2ca p2a) body
        return [(body', None)]

    | Call (GCall, name, (Var vname) :: args) ->
        return!
            lookupG prog name
            |> State.mapM (fun (cname, cparams, ``params``, _) ->
                driveBranch prog e vname cname cparams ``params``)

    | Call (GCall, name, arg0 :: args) ->
        let! branches = drivingStep prog arg0
        return
            branches
            |> List.map (fun (e', c) ->
                Call (GCall, name, e' :: args), c)

    | Let (body, bindings) ->
        let vals =
            bindings |> List.map (fun (_, e') -> e', None)
        return (body, None) :: vals
    }

// driveBranch :: Program -> Exp -> Name -> Name -> Params -> Params -> State Int (Exp, Maybe Contraction)
and driveBranch (prog : Program) (e : Exp) (vname : Name) (cname : Name) (cparams : Params) (``params`` : Params)
    : State.State<Exp * Contraction option, int> =
    State.state {
    let! cparams' = freshNameList (List.length cparams)
    let cargs = List.map Var cparams'
    let vname2ctr =
        Map.ofList [(vname, Call (Ctr, cname, cargs))]
    let e' = applySubst vname2ctr e
    let! [(e'', None)] = drivingStep prog e'
    return (e'', Some <| Contraction (vname, cname, cparams'))
    }


(* The parts common to the basic and advanced supercompilers. *)

// findMoreGeneralAncestors :: Tree -> Node -> [Node]
let findMoreGeneralAncestors (tree : Tree) ({ nodeExp = eB } as beta) : Node list =
    ancestors tree beta
    |> List.filter (fun ({ nodeExp = eA } as alpha) ->
        isFGCall eA && instOf eB eA)

// unprocessedNodes :: Tree -> [Node]
let unprocessedNodes (tree : Tree) : Node list =
    treeLeaves tree
    |> List.choose (fun nId ->
        let leaf = IntMap.find nId tree
        if isProcessed tree leaf then None
        else Some leaf)

// buildLoop :: (Program -> Tree -> Node -> State Int Tree) -> Program -> Tree -> State Int Tree
let rec buildLoop (buildStep : Program -> Tree -> Node -> State.State<Tree, int>)
                (prog : Program) (tree : Tree) : State.State<Tree, int> =
    State.state {
    match unprocessedNodes tree with
    | [] ->
        return tree
    | beta :: _ ->
        let! tree' = buildStep prog tree beta
        return! buildLoop buildStep prog tree'
    }

// initTree :: Exp -> Tree
let initTree (e : Exp) : Tree =
    IntMap.singleton 0 {
        nodeId = 0;
        nodeExp = e;
        nodeContr = None;
        nodeParent = None;
        nodeChildren = []; }

// buildProcessTree :: (Program -> Tree -> Node -> State Int Tree) -> Program -> Exp -> Tree
let buildProcessTree (buildStep : Program -> Tree -> Node -> State.State<Tree, int>)
                        (prog : Program) (e : Exp) : Tree =
    (State.eval <| buildLoop buildStep prog (initTree e)) 10000

// loopBack :: Program -> Tree -> Node -> Node -> State Int Tree
/// If beta `instOf` alpha, we generalize beta by introducing
/// a let-expression, in order to make beta the same as alpha
/// (modulo variable names).
let loopBack (prog : Program) (tree : Tree) ({ nodeId = bId; nodeExp = eB } as beta) ({ nodeExp = eA } as alpha)
    : State.State<Tree, int> =
    State.state {
    let bindings =
        matchAgainst eA eB
        |> Option.get
        |> Map.toList
    let letExp = Let (eA, bindings)
    return replaceSubtree tree bId letExp
    }

// expandNode :: Program -> Tree -> Node -> State Int Tree
/// This function applies a driving step to the node's expression,
/// and, in general, adds children to the node.
let expandNode (prog : Program) (tree : Tree) ({ nodeId = bId; nodeExp = eB } as beta) : State.State<Tree, int> =
    State.state {
    let! branches = drivingStep prog eB
    return! addChildren tree bId branches
    }


(* Basic supercompiler *)
[<RequireQualifiedAccess>]
module Basic =
    // basic_buildStep :: Program -> Tree -> Node -> State Int Tree
    let buildStep (prog : Program) (tree : Tree) (beta : Node) : State.State<Tree, int> =
        match findMoreGeneralAncestors tree beta with
        | [] ->
            expandNode prog tree beta
        | alpha :: _ ->
            loopBack prog tree beta alpha

    // basic_buildProcessTree :: Program -> Exp -> Tree
    let buildProcessTree (prog : Program) (e : Exp) : Tree =
        buildProcessTree buildStep prog e


(* Advanced Supercompiler with homeomorphic imbedding and generalization *)

[<RequireQualifiedAccess>]
module Advanced =
    // abstract :: Tree -> Node -> Exp -> Subst -> State Int Tree
    let ``abstract`` (tree : Tree) ({ nodeId = aId; nodeExp = eA } as alpha) (e : Exp) (subst : Subst)
        : State.State<Tree, int> =
        State.state {
        let letExp =
            let bindings = Map.toList subst
            Let (e, bindings)
        return replaceSubtree tree aId letExp
        }

    // split :: Tree -> Node -> State Int Tree
    let split (tree : Tree) { nodeId = nId; nodeExp = Call (kind, name, args) as e; }
        : State.State<Tree, int> =
        State.state {
        let! names' = freshNameList (List.length args)
        let letExp =
            let call = Call (kind, name, List.map Var names')
            let argValues = List.zip names' args
            Let (call, argValues)
        return replaceSubtree tree nId letExp
        }

    // 
    let generalizeAlphaOrSplit (tree : Tree) ({ nodeExp = eB } as beta) ({ nodeExp = eA } as alpha)
        : State.State<Tree, int> =
        State.state {
        let! gen = msg eA eB
        match gen with
        | Gen (e, aSubst, bSubst) ->
            match e with
            | Var _ ->
                return! split tree beta
            | _ ->
                return! ``abstract`` tree alpha e aSubst
        }

    // findEmbeddedAncestors :: Tree -> Node -> [Node]
    let findEmbeddedAncestors (tree : Tree) ({ nodeExp = eB } as beta) : Node list =
        ancestors tree beta
        |> List.filter (fun ({ nodeExp = eA } as alpha) ->
            isFGCall eA && embeddedIn eA eB)

    // advanced_buildStep :: Program -> Tree -> Node -> State Int Tree
    let buildStep (prog : Program) (tree : Tree) (beta : Node) : State.State<Tree, int> =
        match findMoreGeneralAncestors tree beta with
        | [] ->
            match findEmbeddedAncestors tree beta with
            | [] ->
                expandNode prog tree beta
            | alpha :: _ ->
                generalizeAlphaOrSplit tree beta alpha
        | alpha :: _ ->
            loopBack prog tree beta alpha

    // advanced_buildProcessTree :: Program -> Exp -> Tree
    let buildProcessTree (prog : Program) (e : Exp) : Tree =
        buildProcessTree buildStep prog e
