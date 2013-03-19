module Dot
open FParsec

let str = pstring
let ws = spaces
let ws_with_semi = ws .>> str ";" .>> ws <|> ws
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
 
let ID = (numeral <|> string_literal <|> html_string <|> identifier) .>> ws_with_semi


