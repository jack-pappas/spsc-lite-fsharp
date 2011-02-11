namespace SPSC

open FParsec.Primitives
open FParsec.CharParsers

open SPSC.SLanguage

// ToDo: Simplify it. 
module SParsers =
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
        let p = pipe2 firstChar (manyChars restChars) (fun c s -> c.ToString() + s)
        //(p .>> ws) <?> (sprintf "identifier:%A" predicate) // ToDo: provide readable msg
        (p .>> spaces) <?> (sprintf "identifier:%A" predicate) // ToDo: provide readable msg

    let uid:Parser<string,unit> = ident isUpper
    let lid:Parser<string,unit> = ident isLower
    let fid:Parser<string,unit> = ident ((=) 'f')
    let gid:Parser<string,unit> = ident ((=) 'g')

    let vrb = lid |>> (fun x -> Var(x))
    let pat = pipe2 uid (ch '(' >>. (sepBy vrb (ch ',')) .>> ch ')') (fun u vs -> Pat(u, vs))

    let rec term = 
        (*
            ToDo: find out why does not work
            let rec term =
                (fcall |>> (fun x -> x :> Term)) <|>
                (gcall |>> (fun x -> x :> Term)) <|>
                (vrb |>> (fun x -> x :> Term)) <|>
                (ctr |>> (fun x -> x :> Term))
        *)
        parse {
            let! p = 
                (fcall |>> (fun x -> x :> Term)) <|>
                (gcall |>> (fun x -> x :> Term)) <|>
                (vrb |>> (fun x -> x :> Term)) <|>
                (ctr |>> (fun x -> x :> Term))
            return p
        }
    and ctr = pipe2 uid (ch '(' >>. (sepBy term (ch ',')) .>> ch ')') (fun u ts -> CFG.Ctr(u, ts))
    and fRule = pipe3 fid (between (ch '(') (ch ')') (sepBy vrb (ch ','))) (between (ch '=') (ch ';') term) (fun f vs t -> FRule(f, vs, t))
    and gRule = pipe4 gid (ch '(' >>. pat) ((many (ch ',' >>. vrb)) .>> ch ')') (between (ch '=') (ch ';') term) (fun g p vs t -> GRule(g, p, vs, t))
    and fcall = pipe2 fid (between (ch '(') (ch ')') (sepBy term (ch ','))) (fun f ts -> CFG.FCall(f, ts))
    and gcall = pipe2 gid (between (ch '(') (ch ')') (sepBy term (ch ','))) (fun g ts -> CFG.GCall(g, ts))

    let definition = 
        (*
            ToDo: find out why does not work
            let definition = (gRule |>> (fun x -> x :> Rule)) <|> (fRule |>> (fun x -> x :> Rule))
        *)
        parse {
            let! p = (gRule |>> (fun x -> x :> Rule)) <|> (fRule |>> (fun x -> x :> Rule))
            return p
        }

    let prog = spaces >>. (many definition)

    let parseTerm (s:string) = 
        match run term s with
        | Success(r, _, _) -> r
        | Failure(str, exn, _) -> failwith str

    let parseProg (s:string) = 
        match run prog s with
        | Success(r, _, _) -> Program r
        | Failure(str, exn, _) -> failwith str