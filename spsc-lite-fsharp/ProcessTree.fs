module ProcessTree

open ExtCore
open ExtCore.Collections
open ExtCore.Control
open SLanguage
open ShowUtil
open Algebra

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
type Tree = TagMap<NodeIdentifier, Node>

let rec ancestors (tree : Tree) node =
    match node.nodeParent with
    | None -> []
    | Some parentId ->
        let parentNode = TagMap.find parentId tree
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
    | Call (Ctor, _, args) ->
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
    let node = TagMap.find nId tree
    if List.isEmpty node.nodeChildren then
        nId :: acc
    else
        List.foldBack (treeLeavesAcc tree) acc node.nodeChildren

// treeLeaves :: Tree -> [NodeId]
let treeLeaves (tree : Tree) : NodeId list =
    treeLeavesAcc tree 0<_> []

// funcNodes :: Tree -> [Node]
let funcNodes (tree : Tree) =
    let node =
        let leafId =
            // TODO : Is List.head the correct way to implement this?
            List.head <| treeLeaves tree
        TagMap.find leafId tree
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
    do! State.setState (t + 1<_>)
    return t
    }

//
let freshNodeIdList (n : int) =
    state {
    let! (t : NodeId) = State.getState
    do! State.setState (t + Tag.ofInt n)
    return
        [Tag.toInt t .. (Tag.toInt t + n - 1)]
        // HACK : The F# 2.0 (and maybe 3.0) compiler doesn't seem to
        // allow list comprehensions over ranges of tagged integers,
        // so we have to convert the list like this (which causes unnecessary overhead).
        |> List.map Tag.ofInt
    }

// addChildren :: Tree -> NodeId -> [Branch] -> State NodeId Tree
let addChildren (tree : Tree) (nId : NodeId) (branches : Branch list) : StateFunc<NodeId, Tree> =
    state {
    match TagMap.find nId tree with
    | { nodeExp = e; nodeContr = c; nodeParent = p; nodeChildren = chIds; } as node ->
        assert (node.nodeId = nId)
        let! chIds' = freshNodeIdList (List.length branches)
        let tree' =
            tree |> TagMap.add nId
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
            |> TagMap.ofList
            |> TagMap.union tree'
        return tree''
    }

// replaceSubtree :: Tree -> NodeId -> Exp -> Tree
let replaceSubtree (tree : Tree) (nId : NodeId) (e' : Exp) : Tree =
    match TagMap.find nId tree with
    | { nodeExp = e; nodeContr = c; nodeParent = p; nodeChildren = chIds; } as node ->
        assert (node.nodeId = nId)
        (tree, chIds)
        ||> List.fold (fun tree chId ->
            TagMap.remove chId tree)
        |> TagMap.add nId
            { node with
                nodeExp = e';
                nodeChildren = []; }



