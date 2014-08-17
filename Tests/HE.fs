[<NUnit.Framework.TestFixture>]
module Tests.SPSC.HE

open NUnit.Framework

open SLanguage
open HE


[<Test>]
let ``v1 <| v2`` () =
    he (Var "v1") (Var "v2")
    |> assertTrue

[<Test>]
let ``v1 <| f(v2)`` () =
    he (Var "v1") (Call (Ctor, "f", [(Var "v2")]))
    |> assertTrue

[<Test>]
let ``not f(v2) <| v1`` () =
    he (Call (Ctor, "f", [(Var "v2")])) (Var "v1")
    |> assertFalse

// testDiving
[<Test>]
let ``f(v1) < g(v0, f(h(v2)))`` () =
    he (Call (Ctor, "f", [(Var "v1")]))
      (Call (Ctor, "g", [(Var "v0"); (Call (Ctor, "f", [Call (Ctor, "h", [(Var "v2")])]))]))
    |> assertTrue

// testCoupling1
[<Test>]
let ``f(v1, g(v2)) <| f(h(w1), g(w2))`` () =
    he (Call (Ctor, "f", [Var "v1"; Call (Ctor, "g", [Var "v2"])]))
      (Call (Ctor, "f", [Call (Ctor, "h", [Var "w1"]); Call (Ctor, "g", [Var "w2"])]))
    |> assertTrue

// testCoupling2
[<Test>]
let ``not f(v1) <| g(w1)`` () =
    he (Call (Ctor, "f", [Var "v1"])) (Call (Ctor, "g", [Var "w1"]))
    |> assertFalse

