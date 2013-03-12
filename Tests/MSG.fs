[<NUnit.Framework.TestFixture>]
module MSG.Tests

open NUnit.Framework
open FsUnit
open ExtCore
open ExtCore.Control

open SParsers
open SLanguage
open MSG


let private evalMSG e1 e2 =
    (State.evaluate <| msg e1 e2) 10000<_>

[<Test>]
[<TestCase(
    "A(a1,C(a2,a3))",
    "A(b1,C(b2,b3))",
    TestName = "commonFunctor",
    ExpectedResult = "A(v10001,C(v10003,v10004)) =>> {v10001=a1,v10003=a2,v10004=a3}{v10001=b1,v10003=b2,v10004=b3}")>]
[<TestCase(
    "f(a1,a2,a1)",
    "f(b1,b2,b1)",
    TestName = "mergeSubexp1",
    ExpectedResult = "f(v10003,v10002,v10003) =>> {v10002=a2,v10003=a1}{v10002=b2,v10003=b1}")>]
[<TestCase(
    "f(a,a)",
    "f(b,S(b))",
    TestName = "mergeSubexp2",
    ExpectedResult = "f(v10001,v10002) =>> {v10001=a,v10002=a}{v10001=b,v10002=S(b)}")>]
let testMSG e1 e2 =
    evalMSG (pExp e1) (pExp e2)
    |> sprintf "%O"
