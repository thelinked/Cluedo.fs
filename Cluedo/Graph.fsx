#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#r @"..\packages\FSharpx.Core.1.8.3.0\lib\40\FSharpx.Core.dll"
#load "dot.fs"
#load "graph.fs"

open Cluedo.Dot
open Cluedo.Graph
#load "board.fs"
open Cluedo.Board

let printRoute path = 
    match path with
    | None -> printfn "No route available"
    | Some(cost,route) -> 
        route 
        |> List.ofSeq 
        |> List.map (fun node -> printfn "%A - %A" node gameBoard.[node] ) 
        |> ignore

let route = shortestPathBetween gameBoard "WhiteStart" "BallRoom"
let route' = shortestPathBetweenBlocked gameBoard "WhiteStart" "BallRoom" []
let route'' = shortestPathBetweenBlocked gameBoard "WhiteStart" "BallRoom" ["Hallway11"]
let route''' = shortestPathBetweenBlocked gameBoard "WhiteStart" "BallRoom" ["Hallway1"]




