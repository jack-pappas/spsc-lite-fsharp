module HE

open SLanguage
open Algebra

(*  We distinguish a specific category of expressions:
    the ones that generate contractions in the process tree. *)
let rec private aVarIsUnderAttack exp =
    match exp with
    | Call (GCall, _, args) ->
        aVarIsUnderAttack <| List.head args
    | Var _ ->
        true
    | _ ->
        false

(* This is the "classic" homeomorphic imbedding relation. *)

let rec private heByDiving e1 e2 =
    match e2 with
    | Var _ ->
        false
    | Call (_, _, args) ->
        List.exists (he e1) args

and private heByCoupling e1 e2 =
    match e1, e2 with
    | Var _, Var _ ->
        true
    | Call (kind1, name1, args1), Call (kind2, name2, args2)
        when kind1 = kind2 && name1 = name2 ->
        List.map2 he args1 args2
        |> List.reduce (&&)
    | _ ->
        false

and he e1 e2 =
    heByDiving e1 e2 || heByCoupling e1 e2

(*  Enhanced homeomorphic embedding:
    expressions are compared only if they belong
    to the same category (as defined by `aVarIsUnderAttack`). *)

let embeddedIn e1 e2 =
    aVarIsUnderAttack e1 = aVarIsUnderAttack e2
    && he e1 e2
