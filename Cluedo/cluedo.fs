namespace Cluedo
module List = 
    let splitOn test lst = 
        let rec loop lst inner outer =
            match lst with 
            | x::y::ys when test x y -> loop (y::ys) [] (List.rev (x::inner) :: outer)
            | x::xs ->                  loop xs (x::inner) outer
            | _ ->                      List.rev ((List.rev inner) :: outer)
        loop lst [] []

    let generateCircularSeq (lst:'a list) = 
        let rec next () = 
            seq { 
                for element in lst 
                    do yield element
                yield! next ()
            }
        next ()

module Model = 
    open Graph
    open FSharpx.Lens.Operators
    open FSharpx

    //Data Model
    type Character = 
        | Miss_Scarlett  | Colonel_Mustard | Mrs_White
        | Reverend_Green | Mrs_Peacock     | Professor_Plum
        static member All() = [ Miss_Scarlett; Colonel_Mustard; 
            Mrs_White; Reverend_Green; Mrs_Peacock; Professor_Plum; ]

    type Weapon = 
        | Candlestick | Lead_Pipe | Revolver
        | Rope        | Spanner
        static member All() = [ Candlestick; Lead_Pipe; Revolver; 
            Rope; Spanner; ]

    type Room = 
        | Kitchen | Ballroom | Conservatory | Billiard_Room
        | Library | Study    | Hall         | Lounge
        | Dining_Room
        static member All() = 
            [ Kitchen; Ballroom; Conservatory; Billiard_Room; 
            Library; Study; Hall; Lounge; Dining_Room; ]

    type Card = 
        | Player of Character
        | Weapon of Weapon
        | Room of Room
    
    type Hand = Card list
    
    type Murder = {
        murderer : Character;
        weapon : Weapon;
        room : Room;
    }

    type Accuse = {
        character: Character;
        murder: Murder;
        accuesedPlayer: int;
        success: bool;
    }

    type Suggestion = {
        character: Character;
        murder: Murder;
        cardsRevealed: int;
        cardsRevealedByPlayer: int;
    }
    
    type PlayerMovement = {
        character: Character;
        start: string;
        desc: string
    }

    type Update = 
        | NoUpdate
        | Accuse         of Accuse
        | Suggestion     of Suggestion
        | PlayerMovement of PlayerMovement

    type Player = {
        character: Character;
        hand: Card list;
        position: string;
    } with
        static member Position = 
            { Get = fun (x: Player) -> x.position
              Set = fun v (x: Player) -> { x with position = v } }
        static member Character = 
            { Get = fun (x: Player) -> x.character
              Set = fun v (x: Player) -> { x with character = v } }
        static member print (x: Player) =
            printfn "Character: %A\nPosition: %AHand" x.character x.position
            x.hand |> List.iter (fun (c: Card) -> printfn "\t'%A'" c) 

    type GameContext = {
        board : string AdjacencyGraph;
        players : Player list;
        murder: Murder;
    }
    with 
        member s.others char func = 
            s.players |> List.filter (fun p -> p.character <> char) |> List.map func
        member s.player char = 
            s.players |> List.find (fun p -> p.character = char)
        static member Players = 
            { Get = fun (x: GameContext) -> x.players
              Set = fun v (x: GameContext) -> { x with players = v } }
        
    //Lookups
    let getStart = function
        | Miss_Scarlett   -> "ScarlettStart"
        | Colonel_Mustard -> "MustardStart"
        | Mrs_White       -> "WhiteStart"
        | Reverend_Green  -> "GreenStart"
        | Mrs_Peacock     -> "PeacockStart"
        | Professor_Plum  -> "PlumStart"

    let canSuggestFrom = function
        | "BallRoom"     -> Some(Ballroom)
        | "BilliardRoom" -> Some(Billiard_Room)
        | "Conservatory" -> Some(Conservatory)
        | "Lounge"       -> Some(Lounge)
        | "Library"      -> Some(Library)
        | "Kitchen"      -> Some(Kitchen)
        | "Study"        -> Some(Study)
        | "Hall"         -> Some(Hall)
        | "DiningRoom"   -> Some(Dining_Room)
        | _              -> None

    //functions
    let fairDice n = 
        let rand = new System.Random()
        fun () -> rand.Next()%n

    let d6 = (fairDice 5) >> (+) 1

    let shuffle cards = 
        let rand = new System.Random()
        cards
        |> List.map (fun c -> (rand.Next(), c))
        |> List.sortBy fst
        |> List.map snd
      
    let deal list n: Hand list = 
        list 
        |> List.mapi (fun i elem -> i % n, elem)
        |> List.sortBy fst
        |> List.splitOn (fun (a1,a2) (b1,b2) -> b1 - a1 > 0)
        |> List.map (fun l -> List.map snd l)

    let createGame (players: Character list) : GameContext = 
        let playerCount = List.length players
        let playerCards = Character.All() |> shuffle
        let weaponCards = Weapon.All() |> shuffle
        let roomCards =     Room.All() |> shuffle
        let murder = { murderer = List.head playerCards; weapon = List.head weaponCards; room = List.head roomCards }

        let get list cardType = List.tail list |> List.map cardType
        let deck = get playerCards Player @ get weaponCards Weapon @ get roomCards Room |> shuffle
        let hands = deal deck playerCount

        let gamePlayers = List.fold2 (fun acc (hand: Hand) (character: Character) -> 
            {character = character; hand = hand; position = getStart character}::acc) [] hands players

        {murder = murder; players = gamePlayers; board = Board.gameBoard}

    let move (game:GameContext) (movement: int) (character:Character) (desc: string) : GameContext*Update = 

        let otherPlayerLocs = game.others character (fun p -> p.position )

        //otherPlayerLocs |> List.map (printfn "Position: %A") |> ignore
        let pIndex = game.players |> List.findIndex (fun p -> p.character = character)
        let index = movement-1

        let update func = 
            let game' = game |> (GameContext.Players >>| Lens.forList pIndex >>| Player.Position).Update func
            let p = game.players.[pIndex]
            let p' = game'.players.[pIndex]
            let playerMovement = { character = p.character; start = p.position; desc = p'.position}
            game',PlayerMovement(playerMovement)

        match shortestPathBetweenBlocked game.board (game.players.[pIndex].position) desc otherPlayerLocs with
        | Some(cost,route) -> 
            match List.length route with
                | x when index >= x -> update (fun _ -> desc)
                | _                 -> update (fun _ -> List.nth route index)
        | None -> (game,NoUpdate)

    let queryOrder max player = 
        [0..max-1]
        |> List.map (fun s -> (s+player)%max)
        |> List.tail

    let queryHand query hand = 
        let hasAny = function
            | Player(card) when card = query.murderer -> true
            | Weapon(card) when card = query.weapon -> true
            | Room(card)   when card = query.room -> true
            | _ -> false
        List.filter hasAny hand

    let suggest (context: GameContext) playerCount suggested = 
        let rec loop = function
            | [] -> None
            | h::t -> match queryHand suggested context.players.[h].hand with
                      | [] -> loop t
                      | x -> Some(h,x)

        loop <| queryOrder context.players.Length playerCount
