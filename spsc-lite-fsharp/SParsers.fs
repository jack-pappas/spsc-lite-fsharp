module SParsers

open FParsec.Primitives
open FParsec.CharParsers
open SLanguage
open ShowUtil


let ch c = skipChar c >>. spaces
let str s = skipString s >>. spaces

let ident predicate =
    let firstChar = satisfy (fun c -> isAsciiLetter c && predicate c)
    let restChars = satisfy (fun c -> isAsciiLetter c || isDigit c)
    let p = pipe2 firstChar (manyChars restChars) (fun c s -> c.ToString() + s)
    p .>> spaces

let uIdent : Parser<string, unit> =
    ident isUpper <?> "uIdent"

let lIdent : Parser<string, unit> =
    ident isLower <?> "lIdent"

let fIdent : Parser<string, unit> =
    ident ((=) 'f') <?> "fIdent"

let gIdent : Parser<string, unit> =
    ident ((=) 'g') <?> "gIdent"


(* Programs *)

// pattern :: Parser (Name, Params)
let pattern : Parser<Name * Params, unit> =
    parse {
    let! constructorName = uIdent
    let! variableList =
        (ch '(' >>. (sepBy lIdent (ch ',')) .>> ch ')')
//        sepBy lIdent (skipChar ',')
//        |> between (skipChar '(') (skipChar ')')
        <|>% []
    return (constructorName, variableList)
    }
    <?> "pattern"

//
let rec ``constructor`` =
    parse {
    let! ctrName = uIdent
    let! argList =
        (ch '(' >>. (sepBy expression (ch ',')) .>> ch ')')
//        sepBy expression (skipChar ',')
//        |> between (skipChar '(') (skipChar ')')
        <|>% []
    return Call (Ctr, ctrName, argList)
    }
    <?> "constructor"

//
and variableOrFunctionCall =
    parse {
    let! name = lIdent
    let! argList =
        (ch '(' >>. (sepBy expression (ch ',')) .>> ch ')')
//        sepBy1 expression (skipChar ',')
//        |> between (skipChar '(') (skipChar ')')
    match name.[0] with
    | 'f' ->
        return Call (FCall, name, argList)
    | 'g' ->
        return Call (GCall, name, argList)
    | _ ->
        return Var name
    }
    <?> "variableOrFunctionCall"

//
and expression =
    ``constructor`` <|> variableOrFunctionCall
    <?> "expression"

//
let fRule =
    parse {
    let! functionName = fIdent
    let! paramList =
        (ch '(' >>. (sepBy1 lIdent (ch ',')) .>> ch ')')
//        sepBy1 lIdent (skipChar ',')
//        |> between (skipChar '(') (skipChar ')')
    do! skipChar '='
    let! ruleRhs = expression
    do! skipChar ';'
    return FRule (functionName, paramList, ruleRhs)
    }
    <?> "fRule"

//
let gRule =
    parse {
    let! functionName = gIdent
    do! skipChar '('
    let! cname, cparamList = pattern
    let! paramList =
        many <| parse {
        do! skipChar ','
        return! lIdent
        }
        <?> "paramList"
    do! skipChar ')'
    do! skipChar '='
    let! ruleRhs = expression
    do! skipChar ';'
    return GRule (functionName, cname, cparamList, paramList, ruleRhs)
    }
    <?> "gRule"

let rule =
    fRule <|> gRule <?> "rule"

// program :: Parser Program
let program = parse {
    let! ruleList = spaces >>. many rule
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
    | Failure (errorString, error, _) ->
        let ex =
            System.Exception (
                sprintf "Parse error at line %i, column %i: %s"
                    error.Position.Line error.Position.Column errorString)
        
        ex.Data.Add ("Messages",
            FParsec.ErrorMessageList.ToSortedArray error.Messages)
        //ex.Data.Add ("UserState", error.UserState)

        raise ex

let pExp input =
    match run expression input with
    | Success (result, _, _) ->
        result
    | Failure (errorString, error, _) ->
        let ex =
            System.Exception (
                sprintf "Parse error at line %i, column %i: %s"
                    error.Position.Line error.Position.Column errorString)
        ex.Data.Add ("Messages",
            FParsec.ErrorMessageList.ToSortedArray error.Messages)
        //ex.Data.Add ("UserState", error.UserState)

        raise ex
