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
open FSharpx.Collections
open System.Collections


let playerTurn context (player: Player) movementFunc suggestFunc =
    let moveAmount = d6 ()
    let desc = movementFunc moveAmount
    let context',moveUpdate = move context moveAmount player.character desc

    match canSuggestFrom player.position with
    | Some(room) -> context',(suggestFunc room::moveUpdate::[])
    | None       -> context',(moveUpdate::[])

let randomMoveGen context = 
    let descs = Map.fold (fun acc key value -> key::acc) [] context.board
    let pick = fairDice (List.length descs)
    (fun (moveAmount: int) -> List.nth descs (pick ()))

let blankSuggest room = 
    Suggestion(
     {
        character = Miss_Scarlett; 
        murder = { murderer = Miss_Scarlett; weapon = Candlestick; room = room };
        cardsRevealed = 0; 
        cardsRevealedByPlayer = 0
     })

let game = createGame [Miss_Scarlett; Colonel_Mustard; Mrs_White;]

type PlayerModel(n: int, character: Character) = 
    member this.character = character
    member this.n = n
    member this.move = randomMoveGen game
    member this.suggest = blankSuggest

let turns = 
    [PlayerModel(0,Miss_Scarlett); PlayerModel(1,Colonel_Mustard); PlayerModel(2,Mrs_White);]
    |> List.generateCircularSeq

let getNextFunc (seqOfLines : seq<'a>) =               
    let linesIE = seqOfLines.GetEnumerator()
    (fun () -> ignore (linesIE.MoveNext()); linesIE.Current);;

let play =
    let next = getNextFunc turns
    let rec loop (context: GameContext) = 
        seq {
            let (player: PlayerModel) = next ()
            let otherPlayerLocs = context.others player.character (fun p -> p.position )
            let context',updates = playerTurn context (context.player player.character) player.move player.suggest

            for update in updates 
                do yield updates
            yield! loop context'
        }
    loop game


let murder = { murderer = Miss_Scarlett; weapon = Candlestick; room = Kitchen }
let changes = play |> Seq.take 20 |> List.ofSeq



let playGame context = 
    let rec loop (context': GameContext) p turnsleft = 
        printfn "Its players %A turn with %A turns left" p turnsleft
        match (p,turnsleft) with 
        | (_,0) -> 0
        | (n,_) when n < (context'.players.Length-1) -> loop context' (p+1) (turnsleft-1)
        | _ ->  loop context 0 turnsleft-1
           
    let turnsleft = 20
    let player = 0 
    loop context player turnsleft
