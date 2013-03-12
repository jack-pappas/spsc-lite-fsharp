[<NUnit.Framework.TestFixture>]
module Supercompiler.Tests

open NUnit.Framework
open FsUnit
open ExtCore
open ExtCore.Control

open SLanguage
open SParsers
open Supercompiler


let private testBuildPrTree prog e =
    Basic.buildProcessTree (pProg prog) (pExp e)

let private buildStart (buildStep : Program -> _ -> _ -> StateFunc<Algebra.NodeId, _>) prog tree =
    state {
    match unprocessedNodes tree with
    | [] ->
        return tree
    | beta :: _ ->
        let! tree' = buildStep prog tree beta
        return tree'
    }

let private buildPrTree1 prog e =
    (State.evaluate <| buildStart Basic.buildStep prog (initTree e)) 10000<_>

let private testBuildPrTree1 prog e =
    buildPrTree1 (pProg prog) (pExp e)

let private buildPrTree1Adv prog e =
    (State.evaluate <| buildStart Advanced.buildStep prog (initTree e)) 10000<_>

let private testBPT1Adv prog e =
    buildPrTree1Adv (pProg prog) (pExp e)

let [<Literal>] private pAdd = "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
let [<Literal>] private pAddAcc = "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"


let private runDrStep prog e =
    (State.evaluate <| drivingStep prog e) 100<_>

(*

testLet = testDrStep'
  (Program [])
  (Let (Call Ctr "C" [Var "x", Var "y"]) [("x", Var "a"), ("y", Var "b")])
  "[(C(x,y),Nothing),(a,Nothing),(b,Nothing)]"

*)

[<Test>]
[<TestCase(
    "",
    "C(a,b)",
    TestName = "Ctr",
    ExpectedResult = "[(a,Nothing),(b,Nothing)]")>]
[<TestCase(
    "f(x)=x;",
    "f(A(z))",
    TestName = "FCall",
    ExpectedResult = "[(A(z),Nothing)]")>]
[<TestCase(
    pAddAcc,
    "gAddAcc(S(S(Z)), Z)",
    TestName = "GCallCtr",
    ExpectedResult = "[(gAddAcc(S(Z),S(Z)),Nothing)]")>]
[<TestCase(
    pAddAcc,
    "gAddAcc(a,b)",
    TestName = "GCallVar",
    ExpectedResult = "[(b,Just a = Z),(gAddAcc(v100,S(b)),Just a = S(v100))]")>]
[<TestCase(
    pAddAcc,
    "gAddAcc(gAddAcc(a, b), c)",
    TestName = "GCallGeneral",
    ExpectedResult = "[(gAddAcc(b,c),Just a = Z),(gAddAcc(gAddAcc(v100,S(b)),c),Just a = S(v100))]")>]
let drStepTest (prog, e) =
    runDrStep (pProg prog) (pExp e)
    |> sprintf "%O"

// TODO : Can this test be modified into a test case for 'drStepTest'?
let ``Let`` () =
    Let (
        Call (Ctor, "C", [Var "x"; Var "y"]),
        [("x", Var "a"); ("y", Var "b")])
    |> runDrStep (Program [])
    |> sprintf "%O"
    |> should equal
        "[(C(x,y),Nothing),(a,Nothing),(b,Nothing)]"


(*

testPrTrVar = testBuildPrTree "" "x"
testPrTrCtr = testBuildPrTree "" "S(Z)"
testAdd1_0 = testBuildPrTree pAddAcc "gAddAcc(S(Z), Z)"
testAddAB = testBuildPrTree pAdd "gAdd(a, b)"
testAddAdd = testBuildPrTree pAdd "gAdd(gAdd(a,b),c)"

testAPTVar = testBPT1Adv "" "x"
testAPTCtr = testBPT1Adv "" "S(Z)"
testAAdd1_0 = testBPT1Adv pAddAcc "gAddAcc(S(Z), Z)"
testAAddAB = testBPT1Adv pAdd "gAdd(a, b)"
testAAddAdd = testBPT1Adv pAdd "gAdd(gAdd(a,b),c)"

*)
