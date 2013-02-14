// Learn more about F# at http://fsharp.net

// ToDo: samples 3 and 6 still don't work for AdvancedSuperCompiler. Additional debug is required.
module Samples =
    let [<Literal>] target1 =
        "gApp(gApp(x, y), z)"
    let [<Literal>] program1 = "
        gApp(Nil(), vs) = vs;
        gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));"
    let sample1 = target1, program1

    let [<Literal>] target2 =
        "fMain(x, y, z)"
    let [<Literal>] program2 = "
        fMain(x, y, z) = gAppend(gAppend(x, y), z);
        gAppend(Nil(), vs1) = vs1;
        gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));"
    let sample2 = target2, program2

    let [<Literal>] target3 =
        "gRev(x)"
    let [<Literal>] program3 = "
        gApp(Nil(), vs) = vs;
        gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
        gRev(Nil()) = Nil();
        gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));"
    let sample3 = target3, program3

    let [<Literal>] target4 =
        "gAddAcc(a, b)"
    let [<Literal>] program4 = "
        gAddAcc(Z(), y) = y;
        gAddAcc(S(x), y) = gAddAcc(x, S(y));"
    let sample4 = target4, program4

    let [<Literal>] target5 =
        "f1(z)"
    let [<Literal>] program5 = "
        f1(x) = f2(x);
        f2(x) = g1(x);
        f3(x) = f1(x);
        g1(A(a)) = f1(a);
        g1(B(b)) = f1(b);"
    let sample5 = target5, program5

    let [<Literal>] target6 =
        "fEqxx(x)"
    let [<Literal>] program6 = "
        gEq(Z(), y) = gEqZ(y);
        gEq(S(x), y) = gEqS(y, x);

        gEqZ(Z()) = True();
        gEqZ(S(x)) = False();

        gEqS(Z(), x) = False();
        gEqS(S(y), x) = gEq(x, y);

        fEqxx(x) = gEq(x, x);"
    let sample6 = target6, program6

    let [<Literal>] target7 =
        "gD(S(x))"
    let [<Literal>] program7 = "
        gD(Z()) = Z();
        gD(S(x)) = gD(S(S(x)));"
    let sample7 = target7, program7


module Program =
    open System
    open SLanguage
    open Supercompiler
    open ResidualProgramGenerator

    open Samples

    let runBaseSuperCompiler (targetText, programText) =
        let program = SParsers.pProg programText
        let target = SParsers.pExp targetText
        let pt = Basic.buildProcessTree program target
        let resProgram, resTerm =
            ResidualProgramGenerator.genResidualProgram pt

        printfn "** runBaseSuperCompiler **\n\n%O\n\n%O\n\n%O\n\n%s"
            target program resTerm (resProgram.ToPrettyString())

    let runAdvancedSuperCompiler (targetText, programText) =
        let program = SParsers.pProg programText
        let target = SParsers.pExp targetText
        let pt =
            Advanced.buildProcessTree program target
        let resProgram, resTerm =
            ResidualProgramGenerator.genResidualProgram pt

        printfn "** runAdvancedSuperCompiler **\n\n%O\n\n%O\n\n%O\n\n%s"
            target program resTerm (resProgram.ToPrettyString())

    [<EntryPoint>]
    let main args =
        runAdvancedSuperCompiler Samples.sample7

        printf "Press ENTER to exit..."
        System.Console.ReadLine() |> ignore
        0   // Exit code (Success)