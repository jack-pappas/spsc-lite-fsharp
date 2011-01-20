open SPSC

open SPSC.SLanguage
open SPSC.BasicSuperCompiler
open SPSC.ResidualProgramGenerator

open System

module Samples =
    let target1 = "gApp(gApp(x, y), z)"
    let program1 = "
        gApp(Nil(), vs) = vs;
        gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));"
    let sample1 = target1, program1

    let target2 = "fMain(x, y, z)"
    let program2 = "
        fMain(x, y, z) = gAppend(gAppend(x, y), z);
        gAppend(Nil(), vs1) = vs1;
        gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));"
    let sample2 = target2, program2

    let target3 = "gRev(x)"
    let program3 = "
        gApp(Nil(), vs) = vs;
        gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
        gRev(Nil()) = Nil();
        gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));"
    let sample3 = target3, program3

    let target4 = "gAddAcc(a, b)"
    let program4 = "
        gAddAcc(Z(), y) = y;
        gAddAcc(S(x), y) = gAddAcc(x, S(y));"
    let sample4 = target4, program4

    let target5 = "f1(z)"
    let program5 = "
        f1(x) = f2(x);
        f2(x) = g1(x);
        f3(x) = f1(x);
        g1(A(a)) = f1(a);
        g1(B(b)) = f1(b);"
    let sample5 = target5, program5

    let target6 = "fEqxx(x)"
    let program6 = "
        gEq(Z(), y) = gEqZ(y);
        gEq(S(x), y) = gEqS(y, x);

        gEqZ(Z()) = True();
        gEqZ(S(x)) = False();

        gEqS(Z(), x) = False();
        gEqS(S(y), x) = gEq(x, y);

        fEqxx(x) = gEq(x, x);"
    let sample6 = target6, program6

    let target7 = "gD(S(x))"
    let program7 = "
        gD(Z()) = Z();
        gD(S(x)) = gD(S(S(x)));"
    let sample7 = target7, program7

let runBaseSuperCompiler(targetText:string, programText:string) =
    let program = SParsers.parseProg programText
    let target = SParsers.parseTerm targetText
    let sc = new BasicSuperCompiler(program)
    let pt = sc.BuildProcessTree target
    let (resTerm, resProgram) = (new ResidualProgramGenerator(pt)).Result
    printfn "** runBaseSuperCompiler **
    %O 
    %O
    %O
    %O" target program resTerm resProgram

runBaseSuperCompiler Samples.sample1

System.Console.ReadLine() |> ignore