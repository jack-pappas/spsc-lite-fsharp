[<NUnit.Framework.TestFixture>]
module Tests.SPSC.Algebra

open System.Text
open Printf
open NUnit.Framework

open SLanguage
open Algebra
open SParsers


let private showMatchRes = function
    | None -> "*"
    | Some (subst : Subst) ->
        let sb = StringBuilder ()
        subst
        |> Map.iter (fun vname e ->
            bprintf sb "%s->%O;" vname e)
        sb.ToString ()

[<Test>]
let ``Vars`` () =
    pExp "gAdd(gAdd(x,Z),S(z))"
    |> vars
    |> assertEqual ["x"; "z"]

[<Test>]
[<TestCase("x", "S(Z)", "x->S(Z);", TestName = "MatchV_E")>]
[<TestCase("Z", "x", "*", TestName = "testMatchC_V")>]
[<TestCase("C(x,y)", "C(A,B)", "x->A;y->B;", TestName = "testMatchC_C")>]
[<TestCase("C(x,y)", "D(A,B)", "*", TestName = "testMatchC1_C2")>]
[<TestCase("C(x,y)", "f(A,B)", "*", TestName = "testMatchC_F")>]
[<TestCase("C(x,x)", "C(A,A)", "x->A;", TestName = "testMatchC_C_Eq")>]
[<TestCase("C(x,x)", "C(A,B)", "*", TestName = "testMatchC_C_Ne")>]
let ``Match`` (alpha, beta, expected : string) =
    matchAgainst (pExp alpha) (pExp beta)
    |> showMatchRes
    |> assertEqual expected

[<Test>]
[<TestCase("gA(fB(x,y),C)", "gA(fB(a,b),C)", true, TestName = "testEquiv_yes")>]
[<TestCase("gA(fB(x,y),x)", "gA(fB(a,a),b)", false, TestName = "testEquiv_no")>]
let ``Equiv`` (e1, e2, expected : bool) =
    equiv (pExp e1) (pExp e2)
    |> assertEqual expected
