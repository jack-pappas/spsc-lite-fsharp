namespace SPSC.Tests

open SPSC
open SPSC.SLanguage
open SPSC.BasicSuperCompiler

open NUnit.Framework
open FsUnit

module Helper =
    type Gen with 
        member v.ToStr() = sprintf "Gen(%O, %O, %O)" v.T v.M1 v.M2

open Helper

[<TestFixture>] 
type ``MSG Tests`` () = 
    let msgOK (e1:string) (e2:string) (expected:string) = 
        Algebra.resetVarGen() 
        let gen = MSG.msg(SParsers.parseTerm e1, SParsers.parseTerm e2)
        let getStr = gen.ToStr()
        getStr |> should equal expected
    
    [<Test>]
    member v.``Test of CommonFunctor.`` () =
        msgOK "A(a1,C(a2,a3))"
              "A(b1,C(b2,b3))"
              //"Gen(A(v2,C(v4,v5)),Map(v2 -> a1, v4 -> a2, v5 -> a3),Map(v2 -> b1, v4 -> b2, v5 -> b3))" // This is Scala representation
              "Gen(A(v2,C(v4,v5)), map [(v2, a1); (v4, a2); (v5, a3)], map [(v2, b1); (v4, b2); (v5, b3)])"

    [<Test>]
    member v.``Test of MergeSubexp1.`` () =
        msgOK "A(a1,C(a2,a3))"
              "A(b1,C(b2,b3))"
              //"Gen(A(v2,C(v4,v5)),Map(v2 -> a1, v4 -> a2, v5 -> a3),Map(v2 -> b1, v4 -> b2, v5 -> b3))" // This is Scala representation
              "Gen(A(v2,C(v4,v5)), map [(v2, a1); (v4, a2); (v5, a3)], map [(v2, b1); (v4, b2); (v5, b3)])"

    [<Test>]
    member v.``Test of MergeSubexp2.`` () =
        msgOK "f(a,a)"
              "f(b,S(b))"
              //"Gen(f(v2,v3),Map(v2 -> a, v3 -> a),Map(v2 -> b, v3 -> S(b)))" // This is Scala representation
              "Gen(f(v2,v3), map [(v2, a); (v3, a)], map [(v2, b); (v3, S(b))])"