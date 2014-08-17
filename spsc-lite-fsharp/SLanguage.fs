//
module SLanguage

open System
open ShowUtil

//
[<NoComparison>]
type CKind = Ctor | FCall | GCall

type Name = string
type Params = Name list

//
[<NoComparison>]
type Exp =
    | Var of Name
    | Call of CKind * Name * Args
    | Let of Exp * (Name * Exp) list

    override this.ToString () =
        match this with
        | Var name ->
            name
        | Call (_, name, args) ->
            name + showArgs args
        | Let (e, bindings) ->
            "let " + showBindings bindings + " in " + e.ToString ()

and Arg = Exp
and Args = Arg list

type Rule =
    | FRule of Name * Params * Exp
    | GRule of Name * Name * Params * Params * Exp

    override this.ToString () =
        match this with
        | FRule (name, ``params``, expression) ->
            name + "(" + showParams ``params`` + ")=" + expression.ToString ()
        | GRule (name, cname, cparams, ``params``, expression) ->
            name + "(" + showPat cname cparams + showParamsTail ``params`` + ")=" + expression.ToString ()

type Program =
    | Program of Rule list

    override this.ToString () =
        match this with
        | Program rules ->
            let sb = System.Text.StringBuilder ()

            rules
            |> List.iter (fun rule ->
                sb.Append (rule.ToString ()) |> ignore
                sb.Append ";" |> ignore)

            sb.ToString ()
