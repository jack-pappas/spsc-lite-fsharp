[<NUnit.Framework.TestFixture>]
module SLanguage.Tests

open NUnit.Framework
open FsUnit
open SLanguage


[<Test>]
let ``Test ToString of Var and Call.`` () =
    Var "x"
    |> sprintf "%O"
    |> should equal "x"
    
    Call (Ctr, "A", [Var "x"; Var "y"])
    |> sprintf "%O"
    |> should equal "A(x,y)"

    Call (Ctr, "C", [])
    |> sprintf "%O"
    |> should equal "C()"

    Call (FCall, "fX", [Var "x"; Var "y"])
    |> sprintf "%O"
    |> should equal "fX(x,y)"

    Call (GCall, "gX", [Var "x"; Var "y"])
    |> sprintf "%O"
    |> should equal "gX(x,y)"

(*
[<Test>]
let ``Test ToString of Let.`` () =
    // ToDo: How to make Let(Term, (Var*#Term) list) ?
    string <| Let(Var("y"), [(Var("x"), (Var("y") :> Term))]) |> should equal "let x=y in y"
    let bindings = [(Var("x"), (Var("a") :> Term)); (Var("y"), (Var("b") :> Term))]
    let letTerm = Let(Var("x"), bindings) in
    string <| letTerm |> should equal "let x=a,y=b in x"

[<Test>]
let ``Test ToString of Rule.`` () =
    FRule ("f", [Var "x"; Var "y"], Var "y")
    |> sprintf "%O"
    |> should equal "f(x,y)=y;"
    
    GRule ("g", Pat("C", [Var "x"]), [Var "y"], Var "y")
    |> sprintf "%O"
    |> should equal "g(C(x),y)=y;"
    
    GRule ("g", Pat("C", []), [Var "y"], Var "y")
    |> sprintf "%O"
    |> should equal "g(C(),y)=y;"
    
    GRule ("g", Pat("C", []), [], CFG.Ctr ("C", []))
    |> sprintf "%O"
    |> should equal "g(C())=C();"

[<Test>]
let ``Test ToString of Program.`` () =
    Program [
        FRule("f", [], Call (Ctr, "A", [])); 
        FRule("f1", [], Call (Ctr, "A1", [])); ]
    |> sprintf "%O"
    |> should equal "f()=A();f1()=A1();"

    Program [
        GRule("g", Pat("C", []), [], Call (Ctr, "A", []));
        GRule("g1", Pat("C", []), [Var("x")], Call (Ctr, "A", []));
        GRule("g2", Pat("C", [Var("x")]), [], Call (Ctr, "A", [])); ]
    |> sprintf "%O"
    |> should equal "g(C())=A();g1(C(),x)=A();g2(C(x))=A();"
*)
