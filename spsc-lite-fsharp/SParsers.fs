module SParsers

open FParsec.Primitives
open FParsec.CharParsers
open SLanguage
open ShowUtil


//let ws  = spaces // skips any whitespace
//let ch  c = skipChar c >>. ws
//let str s = skipString s >>. ws
//let space:Parser<unit,unit> = whitespace |>> ignore
//let spacing:Parser<unit,unit> = skipManyChars space

let ch c = skipChar c >>. spaces
let str s = skipString s >>. spaces

let ident predicate =
    let firstChar = satisfy (fun c -> isAsciiLetter c && predicate c)
    let restChars = satisfy (fun c -> isAsciiLetter c || isDigit c)
    let p = pipe2 firstChar (manyChars restChars) (fun c s -> c.ToString () + s)
    //(p .>> ws) <?> (sprintf "identifier:%A" predicate) // ToDo: provide readable msg
    (p .>> spaces) <?> (sprintf "identifier:%A" predicate) // ToDo: provide readable msg

let uid : Parser<string, unit> = ident isUpper
let lid : Parser<string, unit> = ident isLower
let fid : Parser<string, unit> = ident ((=) 'f')
let gid : Parser<string, unit> = ident ((=) 'g')

let pat =
    pipe2 uid
        (ch '(' >>. (sepBy lid (ch ',')) .>> ch ')')
        (fun u vs -> u, vs)

// NOTE : We have to explicitly define, then apply, an argument value here
// (basically, eta-expand the term) to avoid issues with recursive initialization.
let rec expr (str : FParsec.CharStream<_>) =
    (ctor <|> (fcall <|> gcall <|> (lid |>> Var))) str

and ctor =
    pipe2 uid
        (ch '(' >>. (sepBy expr (ch ',')) .>> ch ')')
        (fun u ts -> Call (Ctor, u, ts))
and fRule =
    pipe3 fid
        (between (ch '(') (ch ')') (sepBy lid (ch ',')))
        (between (ch '=') (ch ';') expr)
        (fun functionName paramList ruleRhs ->
            FRule (functionName, paramList, ruleRhs))
and gRule =
    pipe4 gid
        (ch '(' >>. pat)
        ((many (ch ',' >>. lid)) .>> ch ')')
        (between (ch '=') (ch ';') expr)
        (fun functionName (cname, cparamList) paramList ruleRhs ->
            GRule (functionName, cname, cparamList, paramList, ruleRhs))
and fcall =
    pipe2 fid
        (between (ch '(') (ch ')') (sepBy expr (ch ',')))
        (fun name argList ->
            Call (FCall, name, argList))
and gcall =
    pipe2 gid
        (between (ch '(') (ch ')') (sepBy expr (ch ',')))
        (fun name argList ->
            Call (GCall, name, argList))

let rule = gRule <|> fRule

let prog = spaces >>. (many rule)

//
let private createParserExn str (ex : FParsec.Error.ParserError) =
    let innerExn =
        let msg =
            let msg = sprintf "Parsing error at position %O." ex.Position
            let sb = System.Text.StringBuilder (msg)
            let mutable msgs = ex.Messages

            if msgs <> null then
                sb.AppendLine " Messages:" |> ignore
                while msgs <> null && msgs.Head <> null do
                    sb.Append("    ") |> ignore
                    sb.AppendLine (msgs.Head.ToString ()) |> ignore
                    msgs <- msgs.Tail
            sb.ToString()
        exn msg

    System.Exception (str, innerExn)

let pExp str =
    match run expr str with
    | Success(r, _, _) -> r
    | Failure(str, ex, _) ->
        raise <| createParserExn str ex

let pRule str =
    match run rule str with
    | Success(r, _, _) -> r
    | Failure(str, ex, _) ->
        raise <| createParserExn str ex

let pProg str =
    match run prog str with
    | Success(r, _, _) -> Program r
    | Failure(str, ex, _) ->
        raise <| createParserExn str ex
