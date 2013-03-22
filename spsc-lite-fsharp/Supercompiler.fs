module Supercompiler

open ExtCore.Control
open ExtCore.Control.Collections
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

// drivingStep :: Program -> Exp -> State NodeId [Branch]
let rec drivingStep (prog : Program) (e : Exp) : StateFunc<NodeId, Branch list> =
    state {
    match e with
    | Call (Ctor, name, args) ->
        return List.map (fun arg -> arg, None) args
    
    | Call (FCall, name, args) ->
        let ``params``, body = lookupF prog name
        let p2a = Map.ofList (List.zip ``params`` args)
        let body' = applySubst p2a body
        return [(body', None)]

    | Call (GCall, name, Call (Ctor, cname, cargs) :: args) ->
        let cparams, ``params``, body = lookupGC prog name cname
        let cp2ca = Map.ofList (List.zip cparams cargs)
        let p2a = Map.ofList (List.zip ``params`` args)
        let body' = applySubst (Map.union cp2ca p2a) body
        return [(body', None)]

    | Call (GCall, name, (Var vname) :: args) ->
        return!
            lookupG prog name
            |> State.List.map (fun (cname, cparams, ``params``, _) ->
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

// driveBranch :: Program -> Exp -> Name -> Name -> Params -> Params -> State NodeId (Exp, Maybe Contraction)
and driveBranch (prog : Program) (e : Exp) (vname : Name) (cname : Name) (cparams : Params) (``params`` : Params)
    : StateFunc<NodeId, Exp * Contraction option> =
    state {
    let! cparams' = freshNameList (List.length cparams)
    let cargs = List.map Var cparams'
    let vname2ctor =
        Map.ofList [(vname, Call (Ctor, cname, cargs))]
    let e' = applySubst vname2ctor e
    let! [(e'', None)] = drivingStep prog e'
    return (e'', Some <| Contraction (vname, cname, cparams'))
    }


(* The parts common to the basic and advanced supercompilers. *)

// findMoreGeneralAncestors :: Tree -> Node -> [Node]
let findMoreGeneralAncestors (tree : Tree) ({ Exp = eB } as beta) : Node list =
    ancestors tree beta
    |> List.filter (fun ({ Exp = eA } as alpha) ->
        isFGCall eA && instOf eB eA)

// unprocessedNodes :: Tree -> [Node]
let unprocessedNodes (tree : Tree) : Node list =
    treeLeaves tree
    |> List.choose (fun nId ->
        let leaf = TagMap.find nId tree
        if isProcessed tree leaf then None
        else Some leaf)

// buildLoop :: (Program -> Tree -> Node -> State NodeId Tree) -> Program -> Tree -> State NodeId Tree
let rec buildLoop (buildStep : Program -> Tree -> Node -> StateFunc<NodeId, Tree>)
                (prog : Program) (tree : Tree) : StateFunc<NodeId, Tree> =
    state {
    match unprocessedNodes tree with
    | [] ->
        return tree
    | beta :: _ ->
        let! tree' = buildStep prog tree beta
        return! buildLoop buildStep prog tree'
    }

// initTree :: Exp -> Tree
let initTree (e : Exp) : Tree =
    TagMap.singleton 0<_> {
        Id = 0<_>;
        Exp = e;
        Contr = None;
        Parent = None;
        Children = []; }

// buildProcessTree :: (Program -> Tree -> Node -> State NodeId Tree) -> Program -> Exp -> Tree
let buildProcessTree (buildStep : Program -> Tree -> Node -> StateFunc<NodeId, Tree>)
                        (prog : Program) (e : Exp) : Tree =
    (State.evaluate <| buildLoop buildStep prog (initTree e)) 10000<_>

// loopBack :: Program -> Tree -> Node -> Node -> State NodeId Tree
/// If beta `instOf` alpha, we generalize beta by introducing
/// a let-expression, in order to make beta the same as alpha
/// (modulo variable names).
let loopBack (prog : Program) (tree : Tree) ({ Id = bId; Exp = eB } as beta) ({ Exp = eA } as alpha)
    : StateFunc<NodeId, Tree> =
    state {
    let bindings =
        matchAgainst eA eB
        |> Option.get
        |> Map.toList
    let letExp = Let (eA, bindings)
    return replaceSubtree tree bId letExp
    }

// expandNode :: Program -> Tree -> Node -> State NodeId Tree
/// This function applies a driving step to the node's expression,
/// and, in general, adds children to the node.
let expandNode (prog : Program) (tree : Tree) ({ Id = bId; Exp = eB } as beta) : StateFunc<NodeId, Tree> =
    state {
    let! branches = drivingStep prog eB
    return! addChildren tree bId branches
    }


(* Basic supercompiler *)
[<RequireQualifiedAccess>]
module Basic =
    // basic_buildStep :: Program -> Tree -> Node -> State NodeId Tree
    let buildStep (prog : Program) (tree : Tree) (beta : Node) : StateFunc<NodeId, Tree> =
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
    // abstract :: Tree -> Node -> Exp -> Subst -> State NodeId Tree
    let ``abstract`` (tree : Tree) ({ Id = aId; Exp = eA } as alpha) (e : Exp) (subst : Subst)
        : StateFunc<NodeId, Tree> =
        state {
        let letExp =
            let bindings = Map.toList subst
            Let (e, bindings)
        return replaceSubtree tree aId letExp
        }

    // split :: Tree -> Node -> State NodeId Tree
    let split (tree : Tree) { Id = nId; Exp = Call (kind, name, args) as e; }
        : StateFunc<NodeId, Tree> =
        state {
        let! names' = freshNameList (List.length args)
        let letExp =
            let call = Call (kind, name, List.map Var names')
            let argValues = List.zip names' args
            Let (call, argValues)
        return replaceSubtree tree nId letExp
        }

    // 
    let generalizeAlphaOrSplit (tree : Tree) ({ Exp = eB } as beta) ({ Exp = eA } as alpha)
        : StateFunc<NodeId, Tree> =
        state {
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
    let findEmbeddedAncestors (tree : Tree) ({ Exp = eB } as beta) : Node list =
        ancestors tree beta
        |> List.filter (fun ({ Exp = eA } as alpha) ->
            isFGCall eA && embeddedIn eA eB)

    // advanced_buildStep :: Program -> Tree -> Node -> State NodeId Tree
    let buildStep (prog : Program) (tree : Tree) (beta : Node) : StateFunc<NodeId, Tree> =
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
