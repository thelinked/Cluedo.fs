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

let rand = fairDice 9 >> (fun x -> x - 1)
let rooms = ["BallRoom"; "BilliardRoom"; "Conservatory"; "Lounge"; 
             "Library"; "Kitchen"; "Study"; "Hall"; "DiningRoom";]
let randRoom = fun () -> List.nth rooms (rand ())

let randomMoveGen char i (game: GameContext) = 
    let striped = stripNodes (game.othersThan char (fun x -> x.position)) game.board
    let finish = randRoom ()
    match shortestPathBetween striped (game.position char) finish with
    | Some(cost,path) -> 
        if List.length path >= i then
            ((game.position char)::(path |> Seq.ofList |> Seq.take i |> List.ofSeq))
        else 
            (game.position char)::path
    | None -> []

let randomSuggestGen = 
    let rnd = System.Random()
    let getRandListElement (lst: 'a list) = lst.[rnd.Next(lst.Length)]
    fun room -> 
        let weapon = Weapon.All() |> getRandListElement
        let character = Character.All() |> getRandListElement
        { murderer = character; weapon = weapon; room = room };

type PlayerModel(character: Character) = 
    interface IPlayerModel with
        member this.character = character
        member this.move = randomMoveGen character
        member this.suggest = randomSuggestGen
        member this.receiveUpdate (update:Update) = this :> IPlayerModel
        member this.show cards toPlayer = (cards |> List.head),(this :> IPlayerModel)
        member this.see suggestion (cardFromPlayer) = 
            Some({ murderer = Colonel_Mustard; weapon = Candlestick; room = Kitchen }),(this :> IPlayerModel)

let game = createGame [Miss_Scarlett; Colonel_Mustard; Mrs_White; Reverend_Green; Mrs_Peacock; Professor_Plum;]
let murder = { murderer = Miss_Scarlett; weapon = Candlestick; room = Kitchen }
let players = 
    [PlayerModel(Miss_Scarlett); 
     PlayerModel(Colonel_Mustard); 
     PlayerModel(Mrs_White);
     PlayerModel(Reverend_Green);
     PlayerModel(Mrs_Peacock);
     PlayerModel(Professor_Plum);]
    |> List.map (fun x -> x :> IPlayerModel)

let changes = play game players |> List.ofSeq