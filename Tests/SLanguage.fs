[<NUnit.Framework.TestFixture>]
module Tests.SPSC.SLanguage

open NUnit.Framework
open SLanguage


/// Expressions and their string representations.
let expressions () =
    [|  (* Var *)
        "x", Var "x";

        (* Call *)
        "A(x,y)", Call (Ctor, "A", [Var "x"; Var "y"]);
        "C()", Call (Ctor, "C", []);
        "fX(x,y)", Call (FCall, "fX", [Var "x"; Var "y"]);
        "gX(x,y)", Call (GCall, "gX", [Var "x"; Var "y"]);

        (* Let *)
        "let x=y in y", Let (Var "y", ["x", Var "y"]);
        "let x=a,y=b in x", Let (Var "x", ["x", Var "a"; "y", Var "b"]);
    |]

/// Rules and their string representations.
let rules () =
    [|  (* FRule *)
        "f(x,y)=y;", FRule ("f", ["x"; "y"], Var "y");

        (* GRule *)
        "g(C(x),y)=y;", GRule ("g", "C", ["x"], ["y"], Var "y");
        "g(C(),y)=y;", GRule ("g", "C", [], ["y"], Var "y");
        "g(C())=C();", GRule ("g", "C", [], [], Call (Ctor, "C", []));
    |]

/// Programs and their string representations.
let programs () =
    [|  "f()=A();f1()=A1();",
        Program [
            FRule("f", [], Call (Ctor, "A", []));
            FRule("f1", [], Call (Ctor, "A1", [])); ];

        "g(C())=A();g1(C(),x)=A();g2(C(x))=A();",
        Program [
            GRule("g", "C", [], [], Call (Ctor, "A", []));
            GRule("g1", "C", [], ["x"], Call (Ctor, "A", []));
            GRule("g2", "C", ["x"], [], Call (Ctor, "A", [])); ];
    |]


module Printing =
    /// Test-case source for Exp (expression) printing.
    type private ExpPrintingTestCases () =
        member __.Items
            with get () : System.Collections.IEnumerable =
                expressions ()
                |> Seq.mapi (fun idx (str, expr) ->
                    let data = TestCaseData (expr, str)
                    data.SetName (sprintf "Exp #%i" idx))
                :> System.Collections.IEnumerable

    [<Test(Description = "Tests for printing Exp (expression) instances.")>]
    [<TestCaseSource(typeof<ExpPrintingTestCases>, "Items")>]
    let expression (expr : Exp, expectedStr) =
        expr
        |> string
        |> assertEqual expectedStr

    /// Test-case source for Rule printing.
    type private RulePrintingTestCases () =
        member __.Items
            with get () : System.Collections.IEnumerable =
                rules ()
                |> Seq.mapi (fun idx (str, rule) ->
                    let data = TestCaseData (rule, str)
                    data.SetName (sprintf "Rule #%i" idx))
                :> System.Collections.IEnumerable

    [<Test(Description = "Tests for printing Rule instances.")>]
    [<TestCaseSource(typeof<RulePrintingTestCases>, "Items")>]
    let rule (rule : Rule, expectedStr) =
        rule
        |> string
        |> assertEqual expectedStr
        
    /// Test-case source for Program printing.
    type private ProgramPrintingTestCases () =
        member __.Items
            with get () : System.Collections.IEnumerable =
                programs ()
                |> Seq.mapi (fun idx (str, prog) ->
                    let data = TestCaseData (prog, str)
                    data.SetName (sprintf "Program #%i" idx))
                :> System.Collections.IEnumerable

    [<Test(Description = "Tests for printing Program instances.")>]
    [<TestCaseSource(typeof<ProgramPrintingTestCases>, "Items")>]
    let program (prog : Program, expectedStr) =
        prog
        |> string
        |> assertEqual expectedStr


module Parsing =
    open SParsers
    
    /// Test-case source for Exp (expression) parsing.
    type private ExpParsingTestCases () =
        member __.Items
            with get () : System.Collections.IEnumerable =
                expressions ()
                |> Seq.mapi (fun idx (str, expr) ->
                    let data = TestCaseData (str, expr)
                    data.SetName (sprintf "Exp #%i" idx))
                :> System.Collections.IEnumerable

    [<Test(Description = "Tests for parsing Exp (expression) instances.")>]
    [<TestCaseSource(typeof<ExpParsingTestCases>, "Items")>]
    let expression (str, expectedExp : Exp) =
        str
        |> pExp
        |> assertEqual expectedExp

    /// Test-case source for Rule parsing.
    type private RuleParsingTestCases () =
        member __.Items
            with get () : System.Collections.IEnumerable =
                rules ()
                |> Seq.mapi (fun idx (str, rule) ->
                    let data = TestCaseData (str, rule)
                    data.SetName (sprintf "Rule #%i" idx))
                :> System.Collections.IEnumerable

    [<Test(Description = "Tests for parsing Rule instances.")>]
    [<TestCaseSource(typeof<RuleParsingTestCases>, "Items")>]
    let rule (str, expectedRule : Rule) =
        str
        |> pRule
        |> assertEqual expectedRule
        
    /// Test-case source for Program parsing.
    type private ProgramParsingTestCases () =
        member __.Items
            with get () : System.Collections.IEnumerable =
                programs ()
                |> Seq.mapi (fun idx (str, prog) ->
                    let data = TestCaseData (str, prog)
                    data.SetName (sprintf "Program #%i" idx))
                :> System.Collections.IEnumerable

    [<Test(Description = "Tests for parsing Program instances.")>]
    [<TestCaseSource(typeof<ProgramParsingTestCases>, "Items")>]
    let program (str, expectedProg : Program) =
        str
        |> pProg
        |> assertEqual expectedProg
