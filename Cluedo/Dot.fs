module Dot
open FParsec

//Types
type GraphType = 
    | Graph
    | Digraph

type Name = string
type EdgeSmt = Name list
type Graph = GraphType * Name * EdgeSmt list

type UserState = unit
type Parser<'t> = Parser<'t, UserState>


//Helper functions
let str = pstring
let ws = spaces
let ws_with_semi = (ws .>> str ";" .>> ws) <|> ws
let str_ws s = str s .>> ws 


//ID's
let identifier = 
    let isIDFirstChar c = isLetter c || c = '_'
    let isIDChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIDFirstChar isIDChar "identifier"

let numeral = 
    let negative = pipe2 (str "-") (many1Satisfy isDigit) (fun x y -> x+y)
    let positive = (many1Satisfy isDigit)
    negative <|> positive

let stringLiteral = 
    let normal = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escaped = str "\\" >>. (anyOf "\\\"" |>> (fun c -> string c))
    between (str "\"") (str "\"") (manyStrings (normal <|> escaped))

let htmlString: Parser<_> = 
    pipe3 (str "<") (manySatisfy (fun c -> c <> '>')) (str ">") (fun a s b -> a+s+b)
 
let ID: Parser<_> = (numeral <|> stringLiteral <|> htmlString <|> identifier) .>> ws


//Statements
let edgeStatement op =
    let edge_rhs = many (str_ws op >>. ID .>> ws)
    pipe2 ID edge_rhs (fun x y -> x::y) .>> ws_with_semi

let undirected = str "graph" |>> (fun x -> Graph) .>> ws
let directed = str "digraph" |>> (fun x -> Digraph) .>> ws


//Graph
let graph graph_type edge_type = 
    let graph_name = ID .>> ws .>> str "{" .>> ws
    let edges = many (edge_type)
    tuple3 graph_type graph_name edges .>> str "}"

let dot:Parser<Graph> = 
    graph directed (edgeStatement "->") <|> graph undirected (edgeStatement "--")


//Parsers
let parseDotFile fileName = 
    runParserOnFile dot () fileName System.Text.Encoding.UTF8

let parseDotStream stream = 
    runParserOnStream dot () "" stream System.Text.Encoding.UTF8

let parse = run dot