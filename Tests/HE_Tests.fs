namespace SPSC.Tests

open SPSC
open SPSC.SLanguage
open SPSC.BasicSuperCompiler

open NUnit.Framework
open FsUnit

[<TestFixture>] 
type ``HE Tests`` () = 
    let heTrue (input1:string) (input2:string) = 
        let e1 = SParsers.parseTerm input1
        let e2 = SParsers.parseTerm input2
        (HE.he e1 e2) |> should be True

    let heFalse (input1:string) (input2:string) = 
        let e1 = SParsers.parseTerm input1
        let e2 = SParsers.parseTerm input2
        (HE.he e1 e2) |> should be False

    let varAttackTrue (input:string) = 
        let e = SParsers.parseTerm input
        e |> HE.aVarIsUnderAttack |> should be True

    let varAttackFalse (input:string) = 
        let e = SParsers.parseTerm input
        e |> HE.aVarIsUnderAttack |> should be False

    [<Test>]
    member v.``Test of VarAttack.`` () =
        varAttackTrue  "x"
        varAttackFalse "A()"
        varAttackFalse "f(x)"
        varAttackTrue  "g(x,y)"
        varAttackTrue  "g1(g2(x))"
        varAttackFalse "g(A())"
        varAttackFalse "g(f(x))"

    [<Test>]
    member v.``Test of simple terms.`` () =
        heTrue  "v1" "v2"
        heTrue  "v1" "F(v2)"
        heFalse "F(v2)" "v1"

    [<Test>]
    member v.``Test of diving.`` () =
        heTrue "F(v1)" "G(v0,F(H(v2)))"

    [<Test>]
    member v.``Test of coupling.`` () =
        heTrue  "F(v1,G(v2))" "F(H(w1),G(w2))"
        heFalse "f(v1)" "g(w1)"