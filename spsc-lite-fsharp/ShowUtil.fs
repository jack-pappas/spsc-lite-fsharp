module ShowUtil

// showParams :: [String] -> String
let rec showParams (paramList : string list) : string =
    match paramList with
    | [] -> ""
    | x :: xs ->
        x + showParamsTail xs

// showParamsTail :: [String] -> String
and showParamsTail (paramList : string list) : string =
    match paramList with
    | [] -> ""
    | xs ->
        "," + showParams xs

// showArgs :: (Show t) => [t] -> String
let rec showArgs (args : 'T list) : string =
    match args with
    | [] -> "()"
    | x :: xs ->
        "(" + x.ToString () + showArgsTail xs + ")"

// showArgsTail :: (Show t) => [t] -> String
and showArgsTail (args : 'T list) : string =
    match args with
    | [] -> ""
    | x :: xs ->
        "," + x.ToString () + showArgsTail xs

// showBindings :: (Show a) => [(String, a)] -> String
let rec showBindings (bindings : (string * _) list) : string =
    match bindings with
    | [] -> ""
    | (v, e) :: xs ->
        v + "=" + e.ToString () + showBindingsTail xs

// showBindingsTail :: (Show a) => [(String,a)] -> String
and showBindingsTail (bindings : (string * _) list) : string =
    match bindings with
    | [] -> ""
    | xs ->
        "," + showBindings xs

// showPat :: String -> [String] -> String
let showPat cname cparams =
    match cparams with
    | [] ->
        cname
    | cparams ->
        cname + "(" + showParams cparams + ")"
