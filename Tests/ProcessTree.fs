﻿[<NUnit.Framework.TestFixture>]
module ProcessTree

open NUnit.Framework
open FsUnit

open SLanguage
open ProcessTree


let tree = Tree.Create(Var("x")) |> ref

do
    tree := Tree.Create(Var("r"))
    let r = tree.Value.Root
    tree := tree.Value.AddChildren(r, [ (Var("m1") :> Term, None); (Var("m2") :> Term, None) ])
    let children = tree.Value.FindChildren r
    let m1 = List.nth children 0
    let m2 = List.nth children 1
    tree := tree.Value.AddChildren(m1, [ (Var("n") :> Term, None) ])
    tree := tree.Value.Replace(m2, Var("x"))


[<Test>]
let ``Test of tree building.`` () = 
    tree.Value |> string |> should equal "{,0:(r,,,[1,4]),1:(m1,,0,[3]),3:(n,,1,[]),4:(x,,0,[])}"

[<Test>]
let ``Test of tree leaves.`` () = 
    //let leaves = tree.Value.Leaves()
    //let ids = leaves |> List.map (fun n -> n.NodeId)
    tree.Value.Leaves() |> List.map (fun n -> n.NodeId) |> should equal [3;4]