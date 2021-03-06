﻿namespace Cluedo
module Dot = 
    open FParsec

    //Types
    type GraphType = 
        | Graph
        | Digraph

    type Attribute = string * string
    type AttributeTarget = 
        | AllGraph 
        | AllNodes
        | AllEdges

    type Statement = 
        | AttributeStatement of AttributeTarget * Attribute list
        | NodeStatement of string * Attribute list
        | EdgeStatement of string list * (Attribute list option)

    type DotAST = GraphType * string * Statement list

    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>


    //Whitespace
    let preProcesser = pchar '#' >>. skipRestOfLine true
    let singlelineComment = skipString "//" >>. skipRestOfLine true
    let multilineComment =
        between
            (pstring "/*")
            (pstring "*/")
            (charsTillString "*/" false System.Int32.MaxValue)
            |>> ignore
    let ws = 
        choice[preProcesser; singlelineComment; multilineComment; spaces1] |> skipMany


    //ID's
    let identifier = 
        let isIDFirstChar c = isLetter c || c = '_'
        let isIDChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIDFirstChar isIDChar "identifier"

    let numeral = 
        let negative = pipe2 (pstring "-") (many1Satisfy isDigit) (fun x y -> x+y)
        let positive = (many1Satisfy isDigit)
        negative <|> positive

    let stringLiteral = 
        let normal = many1Satisfy (fun c -> c <> '\\' && c <> '"')
        let escaped = pstring "\\" >>. (anyOf "\\\"" |>> (fun c -> string c))
        between 
            (pstring "\"") 
            (pstring "\"") 
            (manyStrings (normal <|> escaped))

    let htmlString = 
        pipe3 
            (pstring "<") 
            (manySatisfy (fun c -> c <> '>')) 
            (pstring ">") 
            (fun a s b -> a+s+b)
 
    let ID: Parser<string> = 
        choice[numeral; stringLiteral; htmlString; identifier] .>> ws <?> "Expected Identifier"


    //Attributes
    let attribs = 
        let attrib = pipe2 
                        (ID .>> pstring "=" .>> ws) ID  
                        (fun key value -> key,value)
        let attrib_rhs = pstring "," >>. attrib |> many
        between
            (pstring "[")
            (pstring "]")
            (pipe2 attrib attrib_rhs (fun x y -> x::y))
    
    let attribGlobalTarget = 
        let p name value = pstring name |>> (fun _ -> value)
        choice[ p "graph" AllGraph; p "node" AllNodes; p "edge" AllEdges] .>> ws

    //Statements
    let attributeStatement = 
        tuple2 attribGlobalTarget attribs |>> AttributeStatement

    let nodeStatement = 
        tuple2 ID attribs |>> NodeStatement

    let edgeStatement (op:string) =
        let edge_rhs = (pstring op >>. ws >>. ID ) |> many1
        pipe3
            ID edge_rhs (opt attribs)
            (fun x y z -> (x::y),z)
        |>> EdgeStatement

    let undirected = pstring "graph" |>> (fun x -> Graph) .>> ws
    let directed = pstring "digraph" |>> (fun x -> Digraph) .>> ws

    //Graph
    let graph graphType edgeType = 
        let graphName = ID .>> pstring "{" .>> ws
        let smts = 
            choice [
                (attempt attributeStatement); 
                (attempt nodeStatement);
                (attempt edgeType)
            ]
            .>> (pstring ";" |> optional) .>> ws
            |> many
        tuple3 graphType graphName smts .>> pstring "}"

    let dot:Parser<DotAST> = 
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
