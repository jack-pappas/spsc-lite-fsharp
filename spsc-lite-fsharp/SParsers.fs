module SParsers

open FParsec.Primitives
open FParsec.CharParsers
open SLanguage
open ShowUtil


let alphaNum : Parser<char, unit> =
    letter <|> digit

let identifier : Parser<string, unit> =
    manyChars2 letter alphaNum

let uIdent : Parser<string, unit> = parse {
    do! followedBy upper
    return! identifier
    }

let lIdent : Parser<string, unit> = parse {
    do! followedBy lower
    return! identifier
    }

let fIdent : Parser<string, unit> = parse {
    do! followedBy (pchar 'f')
    return! identifier
    }

let gIdent : Parser<string, unit> = parse {
    do! followedBy (pchar 'g')
    return! identifier
    }


(* Programs *)

// pattern :: Parser (Name, Params)
let pattern : Parser<Name * Params, unit> = parse {
    let! constructorName = uIdent
    let! variableList =
        sepBy lIdent (pchar ',')
        |> between (pchar '(') (pchar ')')
        <|>% []
    return (constructorName, variableList)
    }

//
let rec ``constructor`` = parse {
    let! ctrName = uIdent
    let! argList =
        sepBy expression (pchar ',')
        |> between (pchar '(') (pchar ')')
        <|>% []
    return Call (Ctr, ctrName, argList)
    }

//
and variableOrFunctionCall = parse {
    let! name = lIdent
    let! argList =
        sepBy1 expression (pchar ',')
        |> between (pchar '(') (pchar ')')
    match name.[0] with
    | 'f' ->
        return Call (FCall, name, argList)
    | 'g' ->
        return Call (GCall, name, argList)
    | _ ->
        return Var name
    }

//
and expression =
    ``constructor`` <|> variableOrFunctionCall

//
let fRule = parse {
    let! functionName = fIdent
    let! paramList =
        sepBy1 lIdent (pchar ',')
        |> between (pchar '(') (pchar ')')
    do! skipChar '='
    let! ruleRhs = expression
    do! skipChar ';'
    return FRule (functionName, paramList, ruleRhs)
    }

//
let gRule = parse {
    let! functionName = gIdent
    do! skipChar '('
    let! cname, cparamList = pattern
    let! paramList =
        many <| parse {
        do! skipChar ','
        return! lIdent
        }
    do! skipChar ')'
    do! skipChar '='
    let! ruleRhs = expression
    do! skipChar ';'
    return GRule (functionName, cname, cparamList, paramList, ruleRhs)
    }

let rule =
    fRule <|> gRule

// program :: Parser Program
let program = parse {
    let! ruleList = many rule
    do! eof
    return Program ruleList
    }

// run :: Show a => Parser a -> String -> IO ()
let run' p input =
    match run p input with
    | Failure (errorString, error, state) ->
        printfn "Parse error at %O" error
    | Success (result, userState, endPos) ->
        printfn "%O" result

let parseSLL =
    run' program

let pProg input =
    match run program input with
    | Success (result, _, _) ->
        result

let pExp input =
    match run expression input with
    | Success (result, _, _) ->
        result

