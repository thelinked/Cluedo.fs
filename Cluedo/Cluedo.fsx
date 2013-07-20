#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#r @"..\packages\FSharpx.Core.1.8.3.0\lib\40\FSharpx.Core.dll"
#load "dot.fs"
#load "graph.fs"

open Cluedo
open Cluedo.Dot
open Cluedo.Graph
#load "board.fs"
open Cluedo

#load "cluedo.fs"
open Cluedo.Model

let game = createGame [Miss_Scarlett; Colonel_Mustard; Mrs_White;]
let query = { murderer = Miss_Scarlett; weapon = Candlestick; room = Kitchen }
let p0Suggests = suggest game 0
let p1Suggests = suggest game 1
let p2Suggests = suggest game 2




let playGame card_state = 
    let turnsleft = 20
    let player = 0
    let rec loop (cards: GameContext) p turnsleft = 
        printfn "Its players %A turn with %A turns left" player turnsleft
        match (player,turnsleft) with 
        | (_,0) -> 0
        | (n,_) when n < cards.numPlayers -> loop card_state (p+1) (turnsleft-1)
        | _ ->  loop card_state 0 turnsleft-1
            
    loop card_state player turnsleft
