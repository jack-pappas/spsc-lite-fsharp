/// Most Specific Generalization.
module MSG

open ExtCore.Control
open SParsers
open SLanguage
open ShowUtil
open ProcessTree
open Algebra


[<NoComparison>]
type Gen =
    | Gen of Exp * Subst * Subst

    override this.ToString () =
        match this with
        | Gen (e, subst1, subst2) ->
            e.ToString () + " =>> "
            + "{" + showBindings (Map.toList subst1) + "}"
            + "{" + showBindings (Map.toList subst2) + "}"

//
let mergeableKeyPairs m1 m2 : (_ * _) list =
    ([], Map.toList m1, Map.toList m2)
    |||> List.fold2 (fun keyPairs (k1, e1) (k2, e2) ->
        if k1 < k2 &&
            e1 = e2 &&
            Map.find k1 m2 = Map.find k2 m2 then
            (k1, k2) :: keyPairs
        else
            keyPairs)

//
let mergeSubexp (Gen (e, m1, m2) as u) =
    match mergeableKeyPairs m1 m2 with
    | [] -> u
    | (k1, k2) :: _ ->
        let e' =
            let m =
                Map.empty |> Map.add k1 (Var k2)
            applySubst m e
        
        let m1' = Map.remove k1 m1
        let m2' = Map.remove k1 m2
        Gen (e', m1', m2')

// commonFunctor :: Gen -> State NodeId Gen
let commonFunctor (Gen (e, m1, m2) as u) : StateFunc<NodeId, Gen> =
    state {
    let kes =
        ([], m1)
        ||> Map.fold (fun kes k e1 ->
            if theSameFunctor e1 (Map.find k m2) then
                (k, e1) :: kes
            else kes)

    match kes with
    | [] ->
        return u
    | (k, (Call (ctor, name, args1) as e1)) :: _ ->
        let (Call (_,_,args2) as e2) = Map.find k m2
        let! ns = freshNameList (List.length args1)
        let vs = List.map Var ns
        let e' =
            let m = Map.add k (Call (ctor, name, vs)) Map.empty
            applySubst m e
        let m1' = Map.union (Map.remove k m1) (Map.ofList <| List.zip ns args1)
        let m2' = Map.union (Map.remove k m2) (Map.ofList <| List.zip ns args2)
        return Gen (e', m1', m2')
    }

// msgLoop :: Gen -> State NodeId Gen
let rec msgLoop (u : Gen) : StateFunc<NodeId, Gen> =
    state {
    let u' = mergeSubexp u
    let! u'' = commonFunctor u'
    if u'' = u then
        return u
    else
        return! msgLoop u''
    }

// msg :: Exp -> Exp -> State Int Gen
let msg (e1 : Exp) (e2 : Exp) : StateFunc<NodeId, Gen> =
    state {
    let! k = freshName
    let u =
        let m1 = Map.empty |> Map.add k e1
        let m2 = Map.empty |> Map.add k e2        
        Gen (Var k, m1, m2)
    return! msgLoop u
    }
