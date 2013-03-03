[<NUnit.Framework.TestFixture>]
module HE.Tests

open NUnit.Framework
open FsUnit

open SLanguage
open HE


[<Test>]
let ``v1 <| v2`` () =
    he (Var "v1") (Var "v2")
    |> should be True

[<Test>]
let ``v1 <| f(v2)`` () =
    he (Var "v1") (Call (Ctr, "f", [(Var "v2")]))
    |> should be True

[<Test>]
let ``not f(v2) <| v1`` () =
    he (Call (Ctr, "f", [(Var "v2")])) (Var "v1")
    |> should be False

// testDiving
[<Test>]
let ``f(v1) < g(v0, f(h(v2)))`` () =
    he (Call (Ctr, "f", [(Var "v1")]))
      (Call (Ctr, "g", [(Var "v0"); (Call (Ctr, "f", [Call (Ctr, "h", [(Var "v2")])]))]))
    |> should be True

// testCoupling1
[<Test>]
let ``f(v1, g(v2)) <| f(h(w1), g(w2))`` () =
    he (Call (Ctr, "f", [Var "v1"; Call (Ctr, "g", [Var "v2"])]))
      (Call (Ctr, "f", [Call (Ctr, "h", [Var "w1"]); Call (Ctr, "g", [Var "w2"])]))
    |> should be True

// testCoupling2
[<Test>]
let ``not f(v1) <| g(w1)`` () =
    he (Call (Ctr, "f", [Var "v1"])) (Call (Ctr, "g", [Var "w1"]))
    |> should be False

