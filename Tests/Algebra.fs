[<NUnit.Framework.TestFixture>]
module Algebra.Tests

open NUnit.Framework
open FsUnit

open SLanguage



let substToString (subst:Map<Var,Term> option) : string = 
    match subst with
    | None -> null
    | Some s -> Map.fold (fun acc v e -> acc + sprintf "%O->%O;" v e) System.String.Empty s

let matchOK (pat:string, exp:string, expected:string) : unit = 
    let patTerm, expTerm = SParsers.parseTerm pat, SParsers.parseTerm exp
    let subst = Algebra.matchAgainst patTerm expTerm
    subst |> substToString |> should equal expected

let matchNo (pat:string, exp:string) : unit = 
    let patTerm, expTerm = SParsers.parseTerm pat, SParsers.parseTerm exp
    let subst = Algebra.matchAgainst patTerm expTerm
    subst |> substToString |> should be Null
    
[<Test>] 
let ``Test for functor equality.`` () = 
    Algebra.shellEq(CFG.Ctr("A", []), CFG.Ctr("A", [])) |> should be True
    Algebra.shellEq(CFG.FCall("A", []), CFG.FCall("A", [])) |> should be True
    Algebra.shellEq(CFG.GCall("A", []), CFG.GCall("A", [])) |> should be True
    Algebra.shellEq(CFG.Ctr("A", []), CFG.Ctr("B", [])) |> should be False
    Algebra.shellEq(CFG.Ctr("A", []), CFG.FCall("A", [])) |> should be False
    Algebra.shellEq(CFG.Ctr("A", []), CFG.Ctr("A", [Var("y")])) |> should be False

[<Test>]
let ``Test of Algebra.subst.`` () =
    let e1, e2, e = SParsers.parseTerm "E1()", 
                    SParsers.parseTerm "E2()", 
                    SParsers.parseTerm "Cons(x1,Cons(x2,Cons(x3,Nil())))"
    let subst = Map.ofList [ (Var("x1"), e1); (Var("x2"), e2) ]
    e |> Algebra.applySubst subst |> string |> should equal "Cons(E1(),Cons(E2(),Cons(x3,Nil())))"

[<Test>]
let ``Test of Algebra.vars.`` () =
    let e1 = SParsers.parseTerm "A(x,B(y,z),a)"
    e1 |> Algebra.vars |> should equal [Var("x"); Var("y"); Var("z"); Var("a")]

    let e2 = SParsers.parseTerm "A(x,B(y,x),a)"
    e2 |> Algebra.vars |> should equal [Var("x"); Var("y"); Var("a")]

[<Test>]
let ``Test of match of variable and expression.`` () = 
    matchOK ("x", "S(Z())", "x->S(Z());")

[<Test>]
let ``Test of match of constructor and variable.`` () = 
    matchNo ("Z()", "x")

[<Test>]
let ``Test of match of constructor and constructor.`` () = 
    matchOK("C(x,y)", "C(A(),B())", "x->A();y->B();")
    matchNo("C(x,y)", "f(A(),B())")
    matchNo("C(x,y)", "D(A(),B())")
    matchOK("C(x,x)", "C(A(),A())", "x->A();")
    matchNo("C(x,y)", "C(A(),B(),C())")

[<Test>]
let ``Test of Algebra.equiv.`` () =
    (Algebra.equiv (SParsers.parseTerm "gA(fB(x,y),C())") (SParsers.parseTerm "gA(fB(a,b),C())")) |> should be True
    (Algebra.equiv (SParsers.parseTerm "gA(fB(x,y),x)") (SParsers.parseTerm "gA(fB(a,a),b)")) |> should be False