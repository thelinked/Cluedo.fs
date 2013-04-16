namespace Cluedo
module List = 
    let splitOn test lst = 
        let rec loop lst inner outer =
            match lst with 
            | x::y::ys when test x y -> loop (y::ys) [] (List.rev (x::inner) :: outer)
            | x::xs ->                  loop xs (x::inner) outer
            | _ ->                      List.rev ((List.rev inner) :: outer)
        loop lst [] []

module Model = 
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

    
    type Cards (murder: Murder, player_hands: Hand list) = 
        member this.murder = murder
        member this.playerHands = player_hands
        member this.numPlayers = List.length this.playerHands
        member this.player = function 
            | n when n < this.numPlayers -> List.nth this.playerHands n 
            | _ -> []

    let printPlayer (cards: Cards) n =
        printfn "Player %A's hand" n
        (cards.player n)
        |> List.iter (fun (c: Card) -> printfn "\t'%A'" c) 
        
    //Deck functions
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

    //Dice functions
    let fairDice n = 
        let rand = new System.Random()
        fun () -> rand.Next()%n


    //Game control functions
    let createGame n = 
        let players = Player.All() |> shuffle
        let weapons = Weapon.All() |> shuffle
        let rooms =     Room.All() |> shuffle
        let murder = { murderer = List.head players; weapon = List.head weapons; room = List.head rooms }

        let get list cardType = List.tail list |> List.map cardType

        let deck = get players Player @ get weapons Weapon @ get rooms Room |> shuffle

        Cards(murder, deal deck n)

    //Query functions
    let queryOrder player max = 
        [0..max-1]
        |> List.map (fun s -> (s+player)%max)
        |> List.tail

    let queryHand query hand = 
        let hasAny card = 
            match card with
            | Player(card) when card = query.murderer -> true
            | Weapon(card) when card = query.weapon -> true
            | Room(card) when card = query.room -> true
            | _ -> false
        List.filter hasAny hand

    let suggest (cards: Cards) playerNumber suggested = 
        let rec suggestHelper order = 
            match order with
            | [] -> None
            | h::t -> match queryHand suggested (cards.player h) with
                      | [] -> suggestHelper t
                      | x -> Some(h,x)

        suggestHelper <| queryOrder playerNumber cards.numPlayers
