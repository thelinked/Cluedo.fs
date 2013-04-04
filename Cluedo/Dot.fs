namespace Cluedo
module Dot = 
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
    let preProcesser = pchar '#' >>. skipRestOfLine true
    let singlelineComment = skipString "//" .>> restOfLine true
    let multilineComment =
        between
            (pstring "/*")
            (pstring "*/")
            (charsTillString "*/" false System.Int32.MaxValue)
            |>> ignore
    let ws = (preProcesser <|> singlelineComment <|> multilineComment <|> spaces1) |> skipMany

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
        between 
            (str "\"") 
            (str "\"") 
            (manyStrings (normal <|> escaped))

    let htmlString = 
        pipe3 (str "<") (manySatisfy (fun c -> c <> '>')) (str ">") (fun a s b -> a+s+b)
 
    let ID: Parser<_> =  choice[numeral; stringLiteral; htmlString; identifier] .>> ws


    //Statements
    let edgeStatement op =
        let edge_rhs = many (str op >>. ws >>. ID .>> ws)
        pipe2 ID edge_rhs (fun x y -> x::y) .>> (str ";" |> optional) .>> ws

    let undirected = str "graph" |>> (fun x -> Graph) .>> ws
    let directed = str "digraph" |>> (fun x -> Digraph) .>> ws


    //Graph
    let graph graphType edgeType = 
        let graphName = ID .>> ws .>> str "{" .>> ws
        let edges = many (edgeType)
        tuple3 graphType graphName edges .>> str "}"

    let dot:Parser<Graph> = 
        choice[ graph directed (edgeStatement "->"); 
                graph undirected (edgeStatement "--")]


    //Parsers
    let parseDotFile fileName = 
        runParserOnFile dot () fileName System.Text.Encoding.UTF8

    let parseDotStream stream = 
        runParserOnStream dot () "" stream System.Text.Encoding.UTF8

    let parse = run dot

    let getResult p = 
        match p with
        | Success(result, _, _)   -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg


    //Helper functions to be used with parser results
    let getType = function | (t,_,_) -> t
    let getName = function | (_,n,_) -> n
    let getEdgeSmts = function | (_,_,e) -> e