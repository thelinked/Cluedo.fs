module Dot
open FParsec

let str = pstring
let ws = spaces
let ws_with_semi = (ws .>> str ";" .>> ws) <|> ws
let str_ws s = str s .>> ws 

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg



//ID's
let identifier: Parser<_> = 
    let isIDFirstChar c = isLetter c || c = '_'
    let isIDChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIDFirstChar isIDChar "identifier"

let numeral = 
    let negative = pipe2 (str "-") (many1Satisfy isDigit) (fun x y -> x+y)
    let positve = (many1Satisfy isDigit)

    negative <|> positve

let string_literal: Parser<_> = 
    let normal = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escaped = str "\\" >>. (anyOf "\\\"" |>> (fun c -> string c))

    between (str "\"") (str "\"") 
            (manyStrings (normal <|> escaped))

let html_string: Parser<_> = 
    pipe3 (str "<") (manySatisfy (fun c -> c <> '>')) (str ">") (fun a s b -> a+s+b)
 
let ID = (numeral <|> string_literal <|> html_string <|> identifier) .>> ws



//Statements
let edge_statement op =
    let edge_rhs = many (str_ws op >>. ID .>> ws)

    pipe2 ID edge_rhs (fun x y -> x::y) .>> ws_with_semi

let undirected_edge_smt = edge_statement "--"
let directed_edge_smt = edge_statement "->"



//Graph
let graph graph_type edge_type = 
    let graph_type = (str graph_type) .>> ws
    let graph_name = ID .>> ws .>> str "{" .>> ws
    let edges = many (edge_type)

    tuple3 graph_type graph_name edges .>> str "}"

let directed_graph = graph "digraph" directed_edge_smt
let undirected_graph = graph "graph" undirected_edge_smt

