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

let playerTurn player movementFunc suggestFunc =
    let moveAmount = d6 ()
    let desc = movementFunc moveAmount
    let moveUpdate = move game moveAmount player desc

    match canSuggest player.position with
    | true,room -> suggestFunc room::moveUpdate::[]
    | false,_   -> moveUpdate::[]

let randomMoveGen () = 
    let descs = Map.fold (fun acc key value -> key::acc) [] game.board
    let pick = fairDice (List.length descs)
    (fun (moveAmount: int) -> List.nth descs (pick ()))

let blankSuggest = fun room -> 
    Suggestion(
     {
        player = Miss_Scarlett; 
        murder = { murderer = Miss_Scarlett; weapon = Candlestick; room = room };
        cardsRevealed = 0; 
        cardsRevealedByPlayer = 0})

type PlayerModel(n: int) = 
    member this.n = n
    member this.move = randomMoveGen ()
    member this.suggest = blankSuggest

let turns = 
    [PlayerModel(0); PlayerModel(1); PlayerModel(2);]
    |> List.generateCircularSeq
    |> Seq.map (fun (p) -> playerTurn (game.player p.n) p.move p.suggest)
    |> Seq.concat


turns
|> Seq.filter (function | Suggestion(x) -> true | _ -> false)

let playGame card_state = 
    let rec loop (cards: GameContext) p turnsleft = 
        printfn "Its players %A turn with %A turns left" p turnsleft
        match (p,turnsleft) with 
        | (_,0) -> 0
        | (n,_) when n < (cards.playerCount-1) -> loop card_state (p+1) (turnsleft-1)
        | _ ->  loop card_state 0 turnsleft-1
           
    let turnsleft = 20
    let player = 0 
    loop card_state player turnsleft
