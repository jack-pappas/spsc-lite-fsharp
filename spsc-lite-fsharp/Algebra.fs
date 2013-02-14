module Algebra

open FSharpx    // State workflow
open SLanguage

type Subst = Map<Name, Exp>

let theSameFunctor exp1 exp2 =
    match exp1, exp2 with
    | Call (kind1, name1, _), Call (kind2, name2, _) ->
        kind1 = kind2 && name1 = name2
    | _ ->
        false

let rec applySubst (m : Subst) e : Exp =
    match e with
    | Var name ->
        let e' = Map.tryFind name m
        defaultArg e' e
    | Call (kind, name, args) ->
        let args' = List.map (applySubst m) args
        Call (kind, name, args')

let rec matchAgainstAccL (m : Subst option) args1 args2 : Subst option =
    match m, args1, args2 with
    | Some m, [], [] ->
        Some m
    | Some m, e :: es, e' :: es' ->
        matchAgainstAccL (matchAgainstAcc (Some m) e e') es es'
    | _ ->
        None 

and matchAgainstAcc (m : Subst option) exp1 exp2 : Subst option =
    match m, exp1, exp2 with
    | Some m, Var vname, e' ->
        match Map.tryFind vname m with
        | None ->
            Some <| Map.add vname e' m
        | Some e'' ->
            if e' <> e'' then None else Some m
    | Some m, Call (kind, name, args), Call (kind', name', args')
        when kind = kind' && name = name' ->
        matchAgainstAccL (Some m) args args'
    | _ ->
        None

let matchAgainst e e' =
    matchAgainstAcc (Some Map.empty) e e'

let instOf e' e =
    matchAgainst e e'
    |> Option.isSome

let equiv e1 e2 =
    instOf e1 e2 && instOf e2 e1

let rec private vars' exp =
    match exp with
    | Var vname ->
        Set.singleton vname
    | Call (_, _, args) ->
        (Set.empty, args)
        ||> List.fold (fun argVars arg ->
            Set.union argVars (vars' arg))

let vars exp =
    vars' exp
    |> Set.toList

let isFGCall = function
    | Call (FCall, _, _)
    | Call (GCall, _, _) ->
        true
    | _ ->
        false

let inline mkName t =
    "v" + t.ToString ()

let freshName =
    State.state {
    let! t = State.getState
    do! State.putState (t + 1)
    return mkName t
    }

let freshNameList n =
    State.state {
    let! t = State.getState
    do! State.putState (t + n)
    return
        List.map mkName [t .. (t + n - 1)]
    }
