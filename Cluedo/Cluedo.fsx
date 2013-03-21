﻿#load "cluedo.fs"
#load "dot.fs"
open Cluedo.Model
open Cluedo.Graph
open Cluedo.Dot

let game = createGame 3
let query = { murderer = Miss_Scarlett; weapon = Candlestick; room = Kitchen }
let p0Suggests = suggest game 0
let p1Suggests = suggest game 1
let p2Suggests = suggest game 2

let playGame card_state = 
    let turnsleft = 20
    let player = 0
    let rec loop (cards: Cards) p turnsleft = 
        printfn "Its players %A turn with %A turns left" player turnsleft
        match (player,turnsleft) with 
        | (_,0) -> 0
        | (n,_) when n < cards.numPlayers -> loop card_state (p+1) (turnsleft-1)
        | _ ->  loop card_state 0 turnsleft-1
            
    loop card_state player turnsleft
