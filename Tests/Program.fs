module Test

open System
open NUnit.Framework
open Program

open SPSC
open SPSC.SLanguage
open NUnit.Framework
open FsUnit

let substToString (subst:Map<Var,Term> option) : string = 
        match subst with
        | None -> null
        | Some s -> Map.fold (fun acc v e -> acc + sprintf "%O->%O;" v e) System.String.Empty s

do
    (new SPSC.Tests.``BasicSupercompiler Tests``()).``Test of Call and Ctr.``()

    let e1, e2, e3 = "C(x,y)", "f(A(),B())", ""
    let k = Algebra.matchAgainst (SParsers.parseTerm e1) (SParsers.parseTerm e2)
    printfn "%O\n%s\n%s" k (substToString k) e3

    printfn "Test module"
    System.Console.ReadLine() |> ignore