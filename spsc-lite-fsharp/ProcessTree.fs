module ProcessTree

open ExtCore
open ExtCore.Collections
open ExtCore.Control
open SLanguage
open ShowUtil
open Algebra

type NodeId = int

type Contraction =
    | Contraction of Name * Name * Params

    override this.ToString () =
        match this with
        | Contraction (vname, cname, cparams) ->
            vname + " = " + showPat cname cparams

type Branch = Exp * Contraction option

type Node = {
    nodeId : NodeId;
    nodeExp : Exp;
    nodeContr : Contraction option;
    nodeParent : NodeId option;
    nodeChildren : NodeId list;
    }
    // deriving Show

// By convention, the root node's id is 0.
type Tree = IntMap<Node>

let rec ancestors (tree : Tree) node =
    match node.nodeParent with
    | None -> []
    | Some parentId ->
        let parentNode = IntMap.find parentId tree
        parentNode :: ancestors tree parentNode

// funcAncestors :: Tree -> Node -> [Node]
let funcAncestors tree node : Node list =
    ancestors tree node
    |> List.filter (fun node' ->
        equiv node.nodeExp node'.nodeExp)

// isProcessed :: Tree -> Node -> Bool
let isProcessed (tree : Tree) node =
    match node.nodeExp with
    | Var _ ->
        true
    | Let (_,_) ->
        false
    | Call (Ctr, _, args) ->
        List.isEmpty args
    | Call (_,_,_) ->
        funcAncestors tree node
        |> List.isEmpty
        |> not
  
// equivCall :: Exp -> Exp -> Bool
let equivCall e e' =
    isFGCall e' && equiv e e'

// treeLeavesAcc :: Tree -> NodeId -> [NodeId] -> [NodeId]
let rec treeLeavesAcc (tree : Tree) (nId : NodeId) acc =
    let node = IntMap.find nId tree
    if List.isEmpty node.nodeChildren then
        nId :: acc
    else
        List.foldBack (treeLeavesAcc tree) acc node.nodeChildren

// treeLeaves :: Tree -> [NodeId]
let treeLeaves (tree : Tree) : NodeId list =
    treeLeavesAcc tree 0 []

// funcNodes :: Tree -> [Node]
let funcNodes (tree : Tree) =
    let node =
        let leafId =
            // TODO : Is List.head the correct way to implement this?
            List.head <| treeLeaves tree
        IntMap.find leafId tree
    funcAncestors tree node

// isFuncNode :: Tree -> NodeId -> Bool
let isFuncNode (tree : Tree) (nId : NodeId) : bool =
    funcNodes tree
    |> List.map (fun node ->
        node.nodeId)
    |> List.exists ((=) nId)

//
let freshNodeId =
    state {
    let! (t : NodeId) = State.getState
    do! State.setState (t + 1)
    return t
    }

//
let freshNodeIdList (n : int) =
    state {
    let! (t : NodeId) = State.getState
    do! State.setState (t + n)
    return [t .. (t + n - 1)]
    }

// addChildren :: Tree -> NodeId -> [Branch] -> State Int Tree
let addChildren (tree : Tree) (nId : NodeId) (branches : Branch list) : StateFunc<int, Tree> =
    state {
    match IntMap.find nId tree with
    | { nodeExp = e; nodeContr = c; nodeParent = p; nodeChildren = chIds; } as node ->
        assert (node.nodeId = nId)
        let! chIds' = freshNodeIdList (List.length branches)
        let tree' =
            tree |> IntMap.add nId
                { node with nodeChildren = chIds @ chIds' }
        let chNodes =
            (chIds', branches)
            ||> List.map2 (fun nId' (e', c') ->
                { nodeId = nId';
                  nodeExp = e';
                  nodeContr = c';
                  nodeParent = Some nId;
                  nodeChildren = []; })
        let tree'' =
            List.zip chIds' chNodes
            |> IntMap.ofList
            |> IntMap.union tree'
        return tree''
    }

// replaceSubtree :: Tree -> NodeId -> Exp -> Tree
let replaceSubtree (tree : Tree) (nId : NodeId) (e' : Exp) : Tree =
    match IntMap.find nId tree with
    | { nodeExp = e; nodeContr = c; nodeParent = p; nodeChildren = chIds; } as node ->
        assert (node.nodeId = nId)
        (tree, chIds)
        ||> List.fold (fun tree chId ->
            IntMap.remove chId tree)
        |> IntMap.add nId
            { node with
                nodeExp = e';
                nodeChildren = []; }



