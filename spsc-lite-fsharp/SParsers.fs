module SParsers

open FParsec.Primitives
open FParsec.CharParsers
open SLanguage
open ShowUtil

(* Parser helpers *)

/// Ignore whitespace on either side of the specified parser.
let ignoreWhitespace parser =
    spaces >>. parser .>> spaces

/// Parses a string surrounded by parentheses.
let parens parser =
    between (pstring "(") (pstring ")") parser

/// Parses a comma-separated list surrounded by parentheses.
let parensList parser =
    parens (sepBy parser (pstring ","))

/// Parse an identifier whose first character matches the given predicate.
let ident predicate =
    let firstChar = satisfy (fun c -> isAsciiLetter c && predicate c)
    let restChars = satisfy (fun c -> isAsciiLetter c || isDigit c)
    let p = pipe2 firstChar (manyChars restChars) (fun c s -> c.ToString () + s)
    (p .>> spaces) <?> (sprintf "identifier:%A" predicate) // ToDo: provide readable msg


/// Parses a variable identifier.
let lid : Parser<string, unit> = ident isLower
/// Parses a constructor (Ctor) identifier.
let uid : Parser<string, unit> = ident isUpper
/// Parses an FCall identifier.
let fid : Parser<string, unit> = ident ((=) 'f')
/// Parses a GCall identifier.
let gid : Parser<string, unit> = ident ((=) 'g')


/// Parses an Exp (expression).
let expr =
    // NOTE : We have to explicitly define, then apply, an argument value here
    // (basically, eta-expand the term) to avoid issues with recursive initialization.
    // TODO : The definition of 'expr' below doesn't handle 'let ... in ...' patterns, so that needs to be implemented.
    let rec expr (str : FParsec.CharStream<_>) =
        let expr = ctor <|> fcall <|> gcall <|> letBinding <|> var
        expr str

    (* Var *)
    and var =
        lid |>> Var

    (* Call *)
    and ctor =
        pipe2 uid (parensList expr)
            (fun u ts -> Call (Ctor, u, ts))

    and fcall =
        pipe2 fid (parensList expr)
            (fun name argList ->
                Call (FCall, name, argList))

    and gcall =
        pipe2 gid (parensList expr)
            (fun name argList ->
                Call (GCall, name, argList))

    (* Let *)
    and letBinding =
        // TEMP : Until this is implemented properly, fail fatally if we see the "let" keyword.
        pstring "let" >>. failFatally "The parser does not yet support 'let' bindings."

    // Return the expression parser
    expr

/// Parses a Rule.
let rule =
    // Parses a pattern.
    let pat =
        pipe2 uid
            (parens (sepBy lid (pstring ",")))
            (fun u vs -> u, vs)

    let fRule =
        pipe3 fid
            (parensList lid)
            (between (pstring "=") (pstring ";") expr)
            (FuncConvert.FuncFromTupled FRule)

    let gRule =
        pipe4 gid
            (pstring "(" >>. pat)
            ((many (pstring "," >>. lid)) .>> pstring ")")
            (between (pstring "=") (pstring ";") expr)
            (fun functionName (cname, cparamList) paramList ruleRhs ->
                GRule (functionName, cname, cparamList, paramList, ruleRhs))

    gRule <|> fRule

/// Parses a Program.
let prog = ignoreWhitespace (many rule)

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
