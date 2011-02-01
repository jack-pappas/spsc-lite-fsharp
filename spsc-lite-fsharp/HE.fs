namespace SPSC

open SPSC.SLanguage

module HE =
    let rec he (t1:Term) (t2:Term) = heByDiving(t1, t2) || heByCoupling(t1, t2)

    and heByDiving(t1:Term, t2:Term) : bool = 
        match t2 with
        | :? CFG as e -> List.exists (he t1) e.Args 
        | _ -> false

    and heByCoupling(t1:Term, t2:Term) : bool = 
        match (t1, t2) with 
        | (:? CFG as e1, (:? CFG as e2)) when Algebra.shellEq(e1, e2) -> List.forall2 he e1.Args e2.Args
        | (:? Var, :? Var) -> true
        | _ -> false

    let rec aVarIsUnderAttack (t:Term) : bool = 
        match t with
        | CFGObject.GCall_(_, args) -> aVarIsUnderAttack args.Head
        | :? Var -> true
        | _ -> false

    let embeddedIn (t1:Term, t2:Term) : bool =
        aVarIsUnderAttack t1 = aVarIsUnderAttack t2 && he t1 t2