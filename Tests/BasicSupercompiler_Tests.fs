namespace SPSC.Tests

open SPSC
open SPSC.SLanguage
open SPSC.BasicSuperCompiler

open NUnit.Framework
open FsUnit

[<TestFixture>] 
type ``BasicSupercompiler Tests`` () = 
    let drStep0(prog:Program, e:Term, expected:string) : unit = 
        Algebra.resetVarGen()
        let scp = new BasicSuperCompiler(prog)
        let branches = scp.DriveExp(e)
        let branches_s = 
            branches
            |> List.map 
                (fun (exp, contr) -> 
                    sprintf "(%O,%s)" exp 
                                      (match contr with Some x -> x.ToString() | None -> System.String.Empty))
            |> List.mkString ""
        printfn "%s = %s is %b" branches_s expected (branches_s = expected)
        branches_s |> should equal expected

    let drStep(prog:string, e:string, expected:string) : unit =
        drStep0(SParsers.parseProg prog, SParsers.parseTerm e, expected)

    let pAdd = "gAdd(S(x),y)=S(gAdd(x,y));gAdd(Z(),y)=y;"
    let pAddAcc = "gAddAcc(S(x),y)=gAddAcc(x,S(y));gAddAcc(Z(),y)=y;"

    [<Test>]
    member v.``Test of Ctr.`` () = 
        drStep("", "C(a,b)", "(a,)(b,)")

    [<Test>]
    member v.``Test of FCall.`` () =
        drStep("f(x)=x;", "f(A(z))", "(A(z),)")

    [<Test>]
    member v.``Test of GCall and Ctr.`` ()=
        drStep(pAddAcc, "gAddAcc(S(S(Z())),Z())", "(gAddAcc(S(Z()),S(Z())),)")

    [<Test>]
    member v.``Test of Call and Ctr.`` () =
        //drStep(pAddAcc, "gAddAcc(a,b)", "(b,a=Z())(gAddAcc(v1,S(b)),a=S(v1))")
        drStep(pAddAcc, "gAddAcc(a,b)", "(gAddAcc(v1,S(b)),a=S(v1))(b,a=Z())")

    [<Test>]
    member v.``Test of GCall, General.`` () =
        //drStep(pAddAcc, "gAddAcc(gAddAcc(a,b),c)", "(gAddAcc(b,c),a=Z())(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))")
        drStep(pAddAcc, "gAddAcc(gAddAcc(a,b),c)", "(gAddAcc(gAddAcc(v1,S(b)),c),a=S(v1))(gAddAcc(b,c),a=Z())")

    [<Test>]
    member v.``Test of Let.`` () =
        drStep0(
            Program([]), 
            Let(CFG.Ctr("C", [Var("x") :> Term; Var("y") :> Term]) :> Term, 
                [(Var("x"), Var("a") :> Term); (Var("y"), Var("b") :> Term)]),
            "(C(x,y),)(a,)(b,)")