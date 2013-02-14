[<NUnit.Framework.TestFixture>]
module SLanguage.Tests

open NUnit.Framework
open FsUnit
open SLanguage


[<Test>]
let ``Test ToString of Var and Call.`` () =
    Var "x" |> string  |> should equal "x"
    CFG.Ctr ("A", [Var "x"; Var "y"]) |> string |> should equal "A(x,y)"

    CFG.Ctr ("C", []) |> string |> should equal "C()"

    CFG.FCall ("fX", [Var "x"; Var "y"]) |> string |> should equal "fX(x,y)"

    CFG.GCall ("gX", [Var "x"; Var "y"]) |> string |> should equal "gX(x,y)"


[<Test>]
let ``Test ToString of Let.`` () =
    // ToDo: How to make Let(Term, (Var*#Term) list) ?
    string <| Let(Var("y"), [(Var("x"), (Var("y") :> Term))]) |> should equal "let x=y in y"
    let bindings = [(Var("x"), (Var("a") :> Term)); (Var("y"), (Var("b") :> Term))]
    let letTerm = Let(Var("x"), bindings) in
    string <| letTerm |> should equal "let x=a,y=b in x"

[<Test>]
let ``Test ToString of Rule.`` () =
    FRule("f", [Var "x"; Var "y"], Var "y") |> string |> should equal "f(x,y)=y;"
    
    GRule("g", Pat("C", [Var "x"]), [Var "y"], Var "y") |> string |> should equal "g(C(x),y)=y;"
    
    GRule("g", Pat("C", []), [Var "y"], Var "y") |> string |> should equal "g(C(),y)=y;"
    
    GRule("g", Pat("C", []), [], CFG.Ctr ("C", [])) |> string |> should equal "g(C())=C();"

[<Test>]
let ``Test ToString of Program.`` () = 
    (let program1 = Program([ FRule("f", [], CFG.Ctr("A", [])); 
                                FRule("f1", [], CFG.Ctr("A1", []))]) in
        string <| program1 |> should equal "f()=A();f1()=A1();" )
    (let program2 = Program([ GRule("g",  Pat("C", []), [], CFG.Ctr("A", [])) ;
                                GRule("g1", Pat("C", []), [Var("x")], CFG.Ctr("A", [])) ;
                                GRule("g2", Pat("C", [Var("x")]), [], CFG.Ctr("A", [])) ]) in
        string <| program2 |> should equal "g(C())=A();g1(C(),x)=A();g2(C(x))=A();" )

[<Test>]
let ``Equality tests.`` () =
    (Var("x") = Var("x")) |> should be True
    (Var("x") = Var("y")) |> should be False
    (CFG.Ctr("A", []) = CFG.Ctr("A", [])) |> should be True
    (CFG.Ctr("A", []) = CFG.Ctr("B", [])) |> should be False
    ([Var("x")] = [Var("x")]) |> should be True
    ([Var("x")] = [Var("y")]) |> should be False
    ([Var("x")] = [Var("y"); Var("z")]) |> should be False
    (CFG.Ctr("A", [Var("x")]) = CFG.Ctr("A",[Var("x")])) |> should be True

