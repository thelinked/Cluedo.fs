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

let randomMoveGen (game: GameContext) char i  = 
    let finish = (List.nth <| Map.fold (fun acc key value -> key::acc) [] game.board) i 
    let positions = game.players |> List.map (fun p -> p.position)
    match shortestPathBetweenBlocked game.board (game.position char) finish positions with
    | Some(cost,path) -> path
    | None -> []

let randomSuggestGen = 
    let rnd = System.Random()
    let getRandListElement (lst: 'a list) = lst.[rnd.Next(lst.Length)]
    let weapon = Weapon.All() |> getRandListElement
    let character = Character.All() |> getRandListElement

    fun room -> { murderer = character; weapon = weapon; room = room };

type PlayerModel(game: GameContext, character: Character) = 
    interface IPlayerModel with
        member this.character = character
        member this.move = randomMoveGen game character
        member this.suggest = randomSuggestGen
        member this.receiveUpdate (update:Update) = this :> IPlayerModel
        member this.show cards toPlayer = (cards |> List.head),(this :> IPlayerModel)
        member this.see suggestion (cardFromPlayer) = Some({ murderer = Miss_Scarlett; weapon = Candlestick; room = Kitchen }),(this :> IPlayerModel)


let game = createGame [Miss_Scarlett; Colonel_Mustard; Mrs_White;]
let murder = { murderer = Miss_Scarlett; weapon = Candlestick; room = Kitchen }
let starting = 
    [PlayerModel(game, Miss_Scarlett); 
     PlayerModel(game, Colonel_Mustard); 
     PlayerModel(game, Mrs_White);]
    |> List.map (fun x -> x :> IPlayerModel)

let changes = playMap game starting |> Seq.take 20 |> List.ofSeq
