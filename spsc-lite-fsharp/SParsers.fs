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
    pipe2 firstChar (manyChars restChars) (fun c s -> c.ToString () + s)


/// Parses a variable identifier.
let lid : Parser<string, unit> = ident isLower <?> "lid"
/// Parses a constructor (Ctor) identifier.
let uid : Parser<string, unit> = ident isUpper <?> "uid"
/// Parses an FCall identifier.
let fid : Parser<string, unit> = ident ((=) 'f') <?> "fid"
/// Parses a GCall identifier.
let gid : Parser<string, unit> = ident ((=) 'g') <?> "gid"


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
        let binding =
            spaces >>. lid .>>. ((ignoreWhitespace <| pstring "=") >>. expr)
        let bindings =
            sepBy binding (pstring ",")

        pipe2
            (pstring "let" >>. spaces1 >>. bindings)
            (spaces1 >>. pstring "in" >>. spaces1 >>. expr)
            (fun bindings body ->
                Let (body, bindings))

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
let prog =
    // Ignore any trailing whitespace.
    (many rule) .>> spaces

let pExp str =
    match run expr str with
    | Success(r, _, _) -> r
    | Failure(msg, _, _) ->
        raise <| exn msg

let pRule str =
    match run rule str with
    | Success(r, _, _) -> r
    | Failure(msg, _, _) ->
        raise <| exn msg

let pProg str =
    match run prog str with
    | Success(r, _, _) -> Program r
    | Failure(msg, _, _) ->
        raise <| exn msg
