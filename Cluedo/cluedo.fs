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

    //Data Model
    type Player = 
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
        | Player of Player
        | Weapon of Weapon
        | Room of Room
    
    type Hand = Card list
    
    type Murder = {
        murderer : Player;
        weapon : Weapon;
        room : Room;
    }

    type Accuse = {
        player: Player;
        murder: Murder;
        accuesedPlayer: int;
        success: bool;
    }

    type Suggestion = {
        player: Player;
        murder: Murder;
        cardsRevealed: int;
        cardsRevealedByPlayer: int;
    }
    
    type PlayerMovement = {
        player: Player;
        start: string;
        desc: string
    }

    type Update = 
        | NoUpdate
        | Accuse         of Accuse
        | Suggestion     of Suggestion
        | PlayerMovement of PlayerMovement

    let getStart = function
        | Miss_Scarlett   -> "ScarlettStart"
        | Colonel_Mustard -> "MustardStart"
        | Mrs_White       -> "WhiteStart"
        | Reverend_Green  -> "GreenStart"
        | Mrs_Peacock     -> "PeacockStart"
        | Professor_Plum  -> "PlumStart"

    let canSuggest = function
        | "BallRoom"     -> (true,Ballroom)
        | "BilliardRoom" -> (true,Billiard_Room)
        | "Conservatory" -> (true,Conservatory)
        | "Lounge"       -> (true,Lounge)
        | "Library"      -> (true,Library)
        | "Kitchen"      -> (true,Kitchen)
        | "Study"        -> (true,Study)
        | "Hall"         -> (true,Hall)
        | "DiningRoom"   -> (true,Dining_Room)
        | _              -> (false,Ballroom)

    type GamePlayer(character, hand) = 
        let mutable _position = getStart character

        member this.character = character
        member this.hand = hand
        member this.position
            with public get() = _position
            and public set value = _position <- value

    type GameContext (murder: Murder, players: GamePlayer list) = 
        member this.murder = murder
        member this.players = players
        member this.playerCount = List.length this.players
        member this.player = function 
            | n when n < this.playerCount -> List.nth this.players n
            | _ -> failwith "Tried to get a player that doesn't exist"

        member this.board = Board.gameBoard
        member this.otherPlayers character func = 
            players
            |> List.fold (fun acc (next: GamePlayer) -> 
                match next.character with
                | x when x = character -> next::acc
                | _ -> acc) []
            |> List.map func

    let printPlayer (context: GameContext) n =
        let player = context.player n
        printfn "Player %A" n
        printfn "Character: %A" player.character
        printfn "Position: %A" player.position
        printfn "Hand" 
        player.hand |> List.iter (fun (c: Card) -> printfn "\t'%A'" c) 
        
    //Movement
    let move (game:GameContext) movement (player:GamePlayer) desc :Update = 
        if movement = 0 then 
            NoUpdate
        else
            let otherPlayerLocs = game.otherPlayers player.character (fun p -> p.position )
            let possibleRoute = shortestPathBetween game.board player.position desc
            let start = player.position
            let index = movement-1

            match possibleRoute with
            | Some(cost,route) -> 
                do match List.length route with
                   | x when index >= x -> do player.position <- desc
                   | _ -> 
                    do player.position <- List.nth route index
                PlayerMovement({ player = player.character; start = start; desc = player.position})
            | None -> NoUpdate

    //Deck
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

    //Dice
    let fairDice n = 
        let rand = new System.Random()
        fun () -> rand.Next()%n

    let d6 = fairDice 6

    //Game control
    let createGame (players: Player list) = 
        let playerCount = List.length players
        let playerCards = Player.All() |> shuffle
        let weaponCards = Weapon.All() |> shuffle
        let roomCards =     Room.All() |> shuffle
        let murder = { murderer = List.head playerCards; weapon = List.head weaponCards; room = List.head roomCards }

        let get list cardType = List.tail list |> List.map cardType
        let deck = get playerCards Player @ get weaponCards Weapon @ get roomCards Room |> shuffle
        let hands = deal deck playerCount

        let gamePlayers = List.fold2 (fun acc (hand: Hand) (player: Player) -> 
            GamePlayer(player, hand)::acc) [] hands players

        GameContext(murder, gamePlayers)

    //Query
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

    let suggest (context: GameContext) playerNumber suggested = 
        let rec loop = function
            | [] -> None
            | h::t -> match queryHand suggested ((context.player h).hand) with
                      | [] -> loop t
                      | x -> Some(h,x)

        loop <| queryOrder context.playerCount playerNumber
