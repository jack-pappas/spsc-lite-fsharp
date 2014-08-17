[<NUnit.Framework.TestFixture>]
module Tests.SPSC.SLanguage

open NUnit.Framework
open SLanguage

module Printing =
    [<Test>]
    let expression () =
        (* Var *)
        Var "x"
        |> string
        |> assertEqual "x"
    
        (* Call *)
        Call (Ctor, "A", [Var "x"; Var "y"])
        |> string
        |> assertEqual "A(x,y)"

        Call (Ctor, "C", [])
        |> string
        |> assertEqual "C()"

        Call (FCall, "fX", [Var "x"; Var "y"])
        |> string
        |> assertEqual "fX(x,y)"

        Call (GCall, "gX", [Var "x"; Var "y"])
        |> string
        |> assertEqual "gX(x,y)"

        (* Let *)
        Let (Var "y", ["x", Var "y"])
        |> string
        |> assertEqual "let x=y in y"
        
        Let (Var "x", ["x", Var "a"; "y", Var "b"])
        |> string
        |> assertEqual "let x=a,y=b in x"
        
    [<Test>]
    let rule () =
        (* FRule *)
        FRule ("f", ["x"; "y"], Var "y")
        |> string
        |> assertEqual "f(x,y)=y;"
    
        (* GRule *)
        GRule ("g", "C", ["x"], ["y"], Var "y")
        |> string
        |> assertEqual "g(C(x),y)=y;"
    
        GRule ("g", "C", [], ["y"], Var "y")
        |> string
        |> assertEqual "g(C(),y)=y;"
    
        GRule ("g", "C", [], [], Call (Ctor, "C", []))
        |> string
        |> assertEqual "g(C())=C();"

    [<Test>]
    let program () =
        Program [
            FRule("f", [], Call (Ctor, "A", [])); 
            FRule("f1", [], Call (Ctor, "A1", [])); ]
        |> string
        |> assertEqual "f()=A();f1()=A1();"

        Program [
            GRule("g", "C", [], [], Call (Ctor, "A", []));
            GRule("g1", "C", [], ["x"], Call (Ctor, "A", []));
            GRule("g2", "C", ["x"], [], Call (Ctor, "A", [])); ]
        |> string
        |> assertEqual "g(C())=A();g1(C(),x)=A();g2(C(x))=A();"


module Parsing =
    open SParsers
    
    [<Test>]
    let expression () : unit =
        "x"
        |> pExp
        |> assertEqual (Var "x")

        "A(x,y)"
        |> pExp
        |> assertEqual (Call (Ctor, "A", [Var "x"; Var "y"]))

        "C()"
        |> pExp
        |> assertEqual (Call (Ctor, "C", []))

        "fX(x,y)"
        |> pExp
        |> assertEqual (Call (FCall, "fX", [Var "x"; Var "y"]))

        "gX(x,y)"
        |> pExp
        |> assertEqual (Call (GCall, "gX", [Var "x"; Var "y"]))

        "let x=y in y"
        |> pExp
        |> assertEqual (Let (Var "y", ["x", Var "y"]))

        "let x=a,y=b in x"
        |> pExp
        |> assertEqual (Let (Var "x", ["x", Var "a"; "y", Var "b"]))

    [<Test>]
    let rule () =
        (* FRule *)
        "f(x,y)=y;"
        |> pRule
        |> assertEqual (FRule ("f", ["x"; "y"], Var "y"))
    
        (* GRule *)
        "g(C(x),y)=y;"
        |> pRule
        |> assertEqual (GRule ("g", "C", ["x"], ["y"], Var "y"))
    
        "g(C(),y)=y;"
        |> pRule
        |> assertEqual (GRule ("g", "C", [], ["y"], Var "y"))
    
        "g(C())=C();"
        |> pRule
        |> assertEqual (GRule ("g", "C", [], [], Call (Ctor, "C", [])))

    [<Test>]
    let program () =
        "f()=A();f1()=A1();"
        |> pProg
        |> assertEqual
        <| Program [
            FRule("f", [], Call (Ctor, "A", [])); 
            FRule("f1", [], Call (Ctor, "A1", [])); ]

        "g(C())=A();g1(C(),x)=A();g2(C(x))=A();"
        |> pProg
        |> assertEqual
        <| Program [
            GRule("g", "C", [], [], Call (Ctor, "A", []));
            GRule("g1", "C", [], ["x"], Call (Ctor, "A", []));
            GRule("g2", "C", ["x"], [], Call (Ctor, "A", [])); ]
