namespace Cluedo
module List = 
    let splitOn prep list = 
        let rec loop lst inner outer =
            match lst with 
            | x::y::ys when prep x y -> loop (y::ys) [] (List.rev (x::inner) :: outer)
            | x::xs ->                  loop xs (x::inner) outer
            | _ ->                      List.rev ((List.rev inner) :: outer)
        loop list [] []

    let findSplit prep list = 
        let i = list |> List.findIndex prep
        let l1,(h::l2) = 
            list 
            |> List.mapi (fun j next -> j,next) 
            |> List.partition (fun (j,_) -> j < i)
        (List.map snd l1),(snd h),(List.map snd l2)

module Model = 
    open Graph
    open FSharpx.Lens.Operators
    open FSharpx
    open FSharpx.State

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
    
    type StartingPlayer = {
        character: Character;
        cardNumber: int
    }

    type Starting = {
        otherPlayers: StartingPlayer list;
    }

    type Accuse = {
        character: Character;
        murder: Murder;
        success: bool;
    }

    type Suggestion = {
        character: Character;
        suggestion: Murder;
        cardRevealedByPlayer: Character;
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
        member s.position char = 
            (s.players |> List.find (fun p -> p.character = char)).position
        member s.othersThan char func = 
            s.players |> List.filter (fun p -> p.character <> char) |> List.map func
        member s.player char = 
            s.players |> List.find (fun p -> p.character = char)
        member s.playerIndex char = 
            s.players |> List.findIndex (fun p -> p.character = char)
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

    let isRoom = function
        | "BallRoom" | "BilliardRoom" | "Conservatory" 
        | "Lounge"   | "Library"      | "Kitchen"      
        | "Study"    | "Hall"         | "DiningRoom" -> true
        | _  -> false

    //functions
    let fairDice n = 
        let rand = new System.Random()
        fun () -> (rand.Next()%n-1) + 1 

    let d6 = fairDice 5

    let createGame (players: Character list) : GameContext =
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

    let validateMove (game:GameContext) (character:Character) (position:string) (path:string list) moveAmount = 
        let otherPositions = game.othersThan character (fun p -> p.position ) |> List.filter (isRoom >> not)
        let board = stripNodes otherPositions game.board

        let rec validate pos = function
            | hd::tl when board.[pos] |> Map.containsKey hd -> validate hd tl
            | hd::[] ->   board.[game.position character] |> Map.containsKey hd
            | []     -> true
            | _      -> false
        path.Length < moveAmount || (not << (validate position)) path
        
    let updatePosition character position (context:GameContext) =
        let i = context.playerIndex character
        let pos = context.players.[i].position
        let context' = 
            (context 
            |> (GameContext.Players >>| Lens.forList i >>| Player.Position).Update (fun _ -> position))
        let pos' = context'.players.[i].position
        PlayerMovement({ character = character; start = pos; desc = pos'}),context'

    let suggest (context: GameContext) character suggested = 
        let queryOrder max player = 
            [0..max-1]
            |> List.map (fun s -> (s+player)%max)
            |> List.tail

        let queryHand query = 
            List.filter (function
                | Player(card) when card = query.murderer -> true
                | Weapon(card) when card = query.weapon -> true
                | Room(card)   when card = query.room -> true
                | _ -> false)

        let rec loop = function
            | [] -> None
            | h::t -> match queryHand suggested context.players.[h].hand with
                      | [] -> loop t
                      | x -> Some(context.players.[h].character,x)

        loop <| queryOrder context.players.Length (context.playerIndex character)

        
    type IPlayerModel = 
        abstract member character : Character
        abstract member move : (int -> string list)
        abstract member suggest : (Room -> Murder)
        abstract member receiveUpdate: Update -> IPlayerModel
        abstract member show : Card list -> 'd -> (Card*IPlayerModel)
        abstract member see : Update -> (Card*Character) option -> (Murder option*IPlayerModel)

    let sendUpdate update = List.map (fun (next:IPlayerModel) -> (next.receiveUpdate update))

    
    let playMap game (players: IPlayerModel list) =
        let rec loop (context: GameContext) ((current:IPlayerModel)::others) = 

            let accuseFlow (c: GameContext) others current accusation = seq {
                match accusation with
                | None -> yield! loop c (others @ [current])
                | Some(a) when context.murder = a -> //Success
                    yield Accuse({character = current.character; murder = a; success = true})
                | Some(a) -> //Failure
                    let accuseUpdate = Accuse({character = current.character; murder = a; success = false})
                    yield! loop c (others |> sendUpdate accuseUpdate)}

            let moveAmount = d6 ()
            //Movement is stored in reverse order
            let movement = current.move moveAmount
            let moveUpdate,context' = updatePosition current.character movement.Head context
            let current'::others' = sendUpdate moveUpdate (current::others)

            seq {
                yield moveUpdate

                match canSuggestFrom (context'.position current'.character) with
                | None -> yield! loop context' (others' @ [current'])
                | Some(room) -> 
                    let suggestion = (current.suggest room)
                    match suggest context' current'.character suggestion with
                    | None -> 
                        let suggestUpdate = 
                            Suggestion({ character = current'.character; 
                                         suggestion = suggestion; 
                                         cardRevealedByPlayer = current'.character;})
                        let others'' = others' |> sendUpdate suggestUpdate
                        let accusation,current'' = current'.see suggestUpdate None

                        yield suggestUpdate
                        yield! accuseFlow context' others'' current'' accusation

                    | Some(char,cards) -> 
                        let suggestUpdate = 
                            Suggestion({ character = current'.character; 
                                         suggestion = suggestion; 
                                         cardRevealedByPlayer = char;})
                        let l1,p,l2 = 
                            others' 
                            |> sendUpdate suggestUpdate 
                            |> List.findSplit (fun o -> o.character = char)
                        let card,p' = p.show cards current'
                        let accusation,current'' = current'.see suggestUpdate (Some(card,p'.character))

                        yield suggestUpdate
                        yield! accuseFlow context' (l1 @ [p'] @ l2) current'' accusation
                }
        loop game players