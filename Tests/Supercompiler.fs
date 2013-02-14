[<NUnit.Framework.TestFixture>]
module Supercompiler.Tests

open NUnit.Framework
open FsUnit

open SLanguage
open Supercompiler


let drStep0 (prog, e, expected) : unit = 
    Algebra.resetVarGen()
    let scp = new BasicSuperCompiler(prog)
    let branches = scp.DriveExp(e)
    let branches_s = 
        branches
        |> List.map (fun (exp, contr) -> 
            match contr with
            | Some x ->
                x.ToString ()
            | None ->
                System.String.Empty
            |> sprintf "(%O,%s)" exp)
        |> List.mkString ""
    printfn "%s = %s is %b" branches_s expected (branches_s = expected)
    branches_s |> should equal expected

let drStep (prog, e, expected) : unit =
    drStep0 (
        SParsers.parseProg prog,
        SParsers.parseTerm e,
        expected)

let [<Literal>] pAdd = "gAdd(S(x),y)=S(gAdd(x,y));gAdd(Z(),y)=y;"
let [<Literal>] pAddAcc = "gAddAcc(S(x),y)=gAddAcc(x,S(y));gAddAcc(Z(),y)=y;"


[<Test>]
let ``Test of Ctr.`` () = 
    drStep("", "C(a,b)", "(a,)(b,)")

[<Test>]
let ``Test of FCall.`` () =
    drStep("f(x)=x;", "f(A(z))", "(A(z),)")

[<Test>]
let ``Test of GCall and Ctr.`` ()=
    drStep(pAddAcc, "gAddAcc(S(S(Z())),Z())", "(gAddAcc(S(Z()),S(Z())),)")

[<Test>]
let ``Test of Call and Ctr.`` () =
    //drStep(pAddAcc, "gAddAcc(a,b)", "(b,a=Z())(gAddAcc(v1,S(b)),a=S(v1))")
    drStep(pAddAcc, "gAddAcc(a,b)", "(gAddAcc(v1,S(b)),a=S(v1))(b,a=Z())")

[<Test>]
let ``Test of GCall, General.`` () =
    //drStep(pAddAcc, "gAddAcc(gAddAcc(a,b),c)", "(gAddAcc(b,c),a=Z())(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))")
    drStep(pAddAcc, "gAddAcc(gAddAcc(a,b),c)", "(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))(gAddAcc(b,c),a=Z())")

[<Test>]
let ``Test of Let.`` () =
    drStep0(
        Program([]), 
        Let(CFG.Ctr("C", [Var("x") :> Term; Var("y") :> Term]) :> Term, 
            [(Var("x"), Var("a") :> Term); (Var("y"), Var("b") :> Term)]),
        "(C(x,y),)(a,)(b,)")

