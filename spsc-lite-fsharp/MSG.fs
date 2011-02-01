namespace SPSC

open SPSC.SLanguage

type Gen(t:Term, m1:Map<Var, Term>, m2:Map<Var, Term>) = 
    member v.T = t
    member v.M1 = m1
    member v.M2 = m2    

module MSG =
    let rec commonSubst(gen:Gen) : Gen = 
        // ToDo: simplify it
        let res : Option<Gen> ref = ref None
        Map.iter (fun v1 e1 ->
            Map.iter (fun v2 e2 ->
                if !res |> Option.isNone && 
                   ((v1 <> v2 && e1 = e2) && (gen.M2.[v1] = gen.M2.[v2])) then
                    res := Some(new Gen(Algebra.applySubst (Map.ofList [(v1, v2 :> Term)]) gen.T, 
                                        Map.remove v1 gen.M1, 
                                        Map.remove v1 gen.M2))) gen.M1) gen.M2
        match !res with Some x -> x | None -> gen

    let commonFun(g:Gen) : Gen = 
        let foldfun (gen,isFound) (v:Var) (t:Term) = 
            if isFound then (gen, true)
            else
                match g.M1.[v], g.M2.[v] with
                | (:? CFG as e1, (:? CFG as e2)) when Algebra.shellEq(e1, e2) -> 
                    let vs = List.map Algebra.freshVar e1.Args
                    let t = Algebra.applySubst (Map.ofList [(v, e1.ReplaceArgs(List.map (fun x -> x :> Term) vs) :> Term)]) g.T
                    let m1 = Map.ofList( List.concat ([ ((Map.remove v g.M1) |> Map.toList) ; List.zip vs e1.Args ]) )
                    let m2 = Map.ofList( List.concat ([ ((Map.remove v g.M2) |> Map.toList) ; List.zip vs e2.Args ]) )
                    (new Gen(t, m1, m2), true)
                | _ -> (gen, false)
        Map.fold foldfun (g, false) g.M1 |> fst

    let msg (t1:Term, t2:Term) : Gen = 
        let v = Algebra.freshVar() 
        let g = new Gen(v, Map.ofList [(v,t1)], Map.ofList [(v,t2)]) |> ref
        let exp = ref g.Value.T
        g := commonSubst(commonFun(g.Value))
        while (exp.Value <> g.Value.T) do
            exp := g.Value.T
            g := commonSubst(commonFun(g.Value))
        g.Value