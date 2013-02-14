[<NUnit.Framework.TestFixture>]
module HE.Tests

open NUnit.Framework
open FsUnit

open SLanguage
open HE


let heTrue input1 input2 =
    let e1 = SParsers.parseTerm input1
    let e2 = SParsers.parseTerm input2
    (HE.he e1 e2) |> should be True

let heFalse input1 input2 =
    let e1 = SParsers.parseTerm input1
    let e2 = SParsers.parseTerm input2
    (HE.he e1 e2) |> should be False

let varAttackTrue input =
    let e = SParsers.parseTerm input
    e |> HE.aVarIsUnderAttack |> should be True

let varAttackFalse input =
    let e = SParsers.parseTerm input
    e |> HE.aVarIsUnderAttack |> should be False

[<Test>]
let ``Test of VarAttack.`` () =
    varAttackTrue  "x"
    varAttackFalse "A()"
    varAttackFalse "f(x)"
    varAttackTrue  "g(x,y)"
    varAttackTrue  "g1(g2(x))"
    varAttackFalse "g(A())"
    varAttackFalse "g(f(x))"

[<Test>]
let ``Test of simple terms.`` () =
    heTrue  "v1" "v2"
    heTrue  "v1" "F(v2)"
    heFalse "F(v2)" "v1"

[<Test>]
let ``Test of diving.`` () =
    heTrue "F(v1)" "G(v0,F(H(v2)))"

[<Test>]
let ``Test of coupling.`` () =
    heTrue  "F(v1,G(v2))" "F(H(w1),G(w2))"
    heFalse "f(v1)" "g(w1)"

