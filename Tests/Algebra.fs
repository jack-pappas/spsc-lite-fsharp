[<NUnit.Framework.TestFixture>]
module Algebra.Tests

open System.Text
open Printf
open NUnit.Framework
open FsUnit

open SLanguage
open Algebra
open SParsers


let private showMatchRes = function
    | None -> "*"
    | Some subst ->
        let sb = StringBuilder ()
        subst |> Map.iter (bprintf sb "%s->%O;")
        sb.ToString ()

[<Test>]
let ``Vars`` () =
    pExp "gAdd(gAdd(x,Z),S(z))"
    |> vars
    |> should equal ["x"; "z"]

[<Test>]
[<TestCase("x", "S(Z)", "x->S(Z);")>]
[<TestCase("Z", "x", "*")>]
[<TestCase("C(x,y)", "C(A,B)", "x->A;y->B;")>]
[<TestCase("C(x,y)", "D(A,B)", "*")>]
[<TestCase("C(x,y)", "f(A,B)", "*")>]
[<TestCase("C(x,x)", "C(A,A)", "x->A;")>]
[<TestCase("C(x,x)", "C(A,B)", "*")>]
let ``Match`` (alpha, beta, expected : string) =
    matchAgainst (pExp alpha) (pExp beta)
    |> showMatchRes
    |> should equal expected

[<Test>]
[<TestCase("gA(fB(x,y),C)", "gA(fB(a,b),C)", true)>]
[<TestCase("gA(fB(x,y),x)", "gA(fB(a,a),b)", false)>]
let ``Equiv`` (e1, e2, expected : bool) =
    equiv (pExp e1) (pExp e2)
    |> should equal expected
