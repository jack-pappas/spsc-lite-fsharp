[<NUnit.Framework.TestFixture>]
module ResidualProgramGenerator.Tests

open NUnit.Framework
open FsUnit

open ResidualProgramGenerator
open SLanguage
open SParsers
open Supercompiler


(* Sample Programs *)

let [<Literal>] private pAdd =
    "gAdd(Z,y)=y;gAdd(S(x),y)=S(gAdd(x,y));"
let [<Literal>] private pAddAcc =
    "gAddAcc(Z,y)=y;gAddAcc(S(x),y)=gAddAcc(x,S(y));"


(* Basic supercompiler *)

let private runBasicScp prog e =
    Basic.buildProcessTree prog e
    |> genResidualProgram


[<Test>]
[<TestCase(
    pAdd,
    "gAdd(a, b)",
    TestName = "AddAB",
    ExpectedResult = "(gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));,gAdd1(a,b))")>]
[<TestCase(
    pAdd,
    "gAdd(gAdd(a,b),c)",
    TestName = "AddAdd",
    ExpectedResult = "(gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));,gAdd1(a,b,c))")>]
let basicScp (prog, e) =
    runBasicScp (pProg prog) (pExp e)
    |> sprintf "%O"


(* Advanced supercompiler *)

let private runAdvScp prog e =
    Advanced.buildProcessTree prog e
    |> genResidualProgram

[<Test>]
[<TestCase(
    pAdd,
    "gAdd(a, b)",
    TestName = "AdvAddAB",
    ExpectedResult = "(gAdd1(Z,b)=b;gAdd1(S(v10000),b)=S(gAdd1(v10000,b));,gAdd1(a,b))")>]
[<TestCase(
    pAdd,
    "gAdd(a, a)",
    TestName = "AdvAddAB",
    ExpectedResult = "(gAdd1(Z,v10006)=v10006;gAdd1(S(v10010),v10006)=S(gAdd1(v10010,v10006));,gAdd1(a,a))")>]
[<TestCase(
    pAdd,
    "gAdd(gAdd(a,b),c)",
    TestName = "AdvAddAdd",
    ExpectedResult = "(gAdd2(Z,c)=c;gAdd2(S(v10003),c)=S(gAdd2(v10003,c));gAdd1(Z,b,c)=gAdd2(b,c);gAdd1(S(v10000),b,c)=S(gAdd1(v10000,b,c));,gAdd1(a,b,c))")>]
[<TestCase(
    pAddAcc,
    "gAddAcc(a, b)",
    TestName = "AdvAddAccAB",
    ExpectedResult = "(gAddAcc1(Z,b)=b;gAddAcc1(S(v10000),b)=gAddAcc1(v10000,S(b));,gAddAcc1(a,b))")>]
[<TestCase(
    pAddAcc,
    "gAddAcc(a, a)",
    TestName = "AdvAddAccAB",
    ExpectedResult = "(gAddAcc1(Z,v10005)=v10005;gAddAcc1(S(v10009),v10005)=gAddAcc1(v10009,S(v10005));,gAddAcc1(a,a))")>]
[<TestCase(
    pAddAcc,
    "gAddAcc(gAddAcc(a,b),c)",
    TestName = "AdvAddAccAddAcc",
    ExpectedResult = "(gAddAcc2(Z,c)=c;gAddAcc2(S(v10003),c)=gAddAcc2(v10003,S(c));gAddAcc1(Z,b,c)=gAddAcc2(b,c);gAddAcc1(S(v10000),b,c)=gAddAcc1(v10000,S(b),c);,gAddAcc1(a,b,c))")>]
let advancedScp (prog, e) =
    runAdvScp (pProg prog) (pExp e)
    |> sprintf "%O"

