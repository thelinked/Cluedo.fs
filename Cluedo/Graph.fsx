#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#r @"..\packages\FSharpx.Core.1.8.3.0\lib\40\FSharpx.Core.dll"
#load "dot.fs"
#load "graph.fs"

open Cluedo.Dot
open Cluedo.Graph

module Map =
  let transposeCombine m =
    (m, m) ||> Map.fold (fun acc k1 m' ->
      (acc, m') ||> Map.fold (fun acc' k2 v ->
        acc'
        |> Map.add k2 (Map.add k1 v (defaultArg (acc' |> Map.tryFind k2) Map.empty))
    ))

type City =
    | Vancouver  | Seattle    | Portland  | Calgary    | Winnipec   | Helena     | SaltLakeCity | SanFranciso 
    | LasVegas   | LosAngeles | Phoenix   | ElPaso     | SantaFe    | Denver     | Duluth       | Omaha 
    | KansasCity | Dallas     | Houston   | NewOrleans | LittleRock | SaintLouis | Chicago      | SautltStMarie 
    | Toronto    | Pittsburch | Nashville | Alanta     | Miami      | Charleston | Washington   | NewYork 
    | Boston     | Raleigh    | Montreal  | OklahomaCity
    static member fromString s = 
        match s with
        | "Vancouver" -> Vancouver
        | "Seattle" -> Seattle
        | "Portland" -> Portland
        | "Calgary" -> Calgary
        | "Winnipec" -> Winnipec
        | "Helena" -> Helena
        | "SaltLakeCity" -> SaltLakeCity
        | "SanFranciso" -> SanFranciso
        | "LasVegas" -> LasVegas
        | "LosAngeles" -> LosAngeles
        | "Phoenix" -> Phoenix
        | "ElPaso" -> ElPaso
        | "SantaFe" -> SantaFe
        | "Denver" -> Denver
        | "Duluth" -> Duluth
        | "Omaha" -> Omaha
        | "KansasCity" -> KansasCity
        | "Dallas" -> Dallas
        | "Houston" -> Houston
        | "NewOrleans" -> NewOrleans
        | "LittleRock" -> LittleRock
        | "SaintLouis" -> SaintLouis
        | "Chicago" -> Chicago
        | "SautltStMarie" -> SautltStMarie
        | "Toronto" -> Toronto
        | "Pittsburch" -> Pittsburch
        | "Nashville" -> Nashville
        | "Alanta" -> Alanta
        | "Miami" -> Miami
        | "Charleston" -> Charleston
        | "Washington" -> Washington
        | "NewYork" -> NewYork
        | "Boston" -> Boston
        | "OklahomaCity" -> OklahomaCity
        | "Raleigh" -> Raleigh
        | "Montreal" -> Montreal
        | _ -> failwith "Not a city!"

type Colour = 
   | Grey | White | Black | Blue | Yellow | Orange | Red | Pink | Green
   static member fromString s = 
        match s with
        | "Grey" -> Grey
        | "White" -> White
        | "Black" -> Black
        | "Blue" -> Blue
        | "Yellow" -> Yellow
        | "Orange" -> Orange
        | "Red" -> Red
        | "Pink" -> Pink
        | "Green" -> Green
        | _ -> failwith "Not a colour!"

//Sigh, Multiple edges between nodes can't work with the current AdjacencyGraph implementation.
let tree = getResult <| parse  @"graph board {

    Vancouver -- Seattle[cost=1,color=Grey]
    Vancouver -- Seattle[cost=1,color=Grey]
    Vancouver -- Calgary[cost=3,color=Grey]
    Seattle -- Calgary[cost=4,color=Grey]
    Seattle -- Portland[cost=1,color=Grey]
    Seattle -- Portland[cost=1,color=Grey]
    Portland -- SanFranciso[cost=5,color=Green]
    Portland -- SanFranciso[cost=5,color=Pink]
    SanFranciso -- LosAngeles[cost=3,color=Yellow]
    SanFranciso -- LosAngeles[cost=3,color=Pink]
    LosAngeles -- LasVegas[cost=2,color=Grey]
    LasVegas -- SaltLakeCity[cost=3,color=Orange]
    SaltLakeCity -- Portland[cost=6,color=Blue]
    Seattle -- Helena[cost=6,color=Yellow]
    Helena -- Calgary[cost=4,color=Grey]
    Helena -- Denver[cost=4,color=Green]
    Helena -- SaltLakeCity[cost=3,color=Pink]
    Helena -- Duluth[cost=5,color=Orange]
    Helena -- Omaha[cost=5,color=Red]
    Calgary -- Winnipec[cost=6,color=White]
    Helena -- Winnipec[cost=4,color=Blue]
    Denver -- SaltLakeCity[cost=3,color=Red]
    Denver -- SaltLakeCity[cost=3,color=Yellow]
    Denver -- Phoenix[cost=6,color=White]
    Phoenix -- LosAngeles[cost=3,color=Grey]
    Phoenix -- ElPaso[cost=3,color=Grey]
    LosAngeles -- ElPaso[cost=6,color=Black]
    Phoenix -- SantaFe[cost=3,color=Grey]
    SantaFe -- Denver[cost=2,color=Grey]
    Denver -- Omaha[cost=4,color=Pink]
    Denver -- OklahomaCity[cost=4,color=Red]
    Denver -- KansasCity[cost=4,color=Black]
    Denver -- KansasCity[cost=4,color=Orange]
    KansasCity -- OklahomaCity[cost=2,color=Grey]
    KansasCity -- OklahomaCity[cost=2,color=Grey]
    OklahomaCity -- ElPaso[cost=6,color=Grey]
    Houston -- ElPaso[cost=6,color=Green]
    OklahomaCity -- SantaFe[cost=3,color=Blue]
    OklahomaCity -- Dallas[cost=2,color=Grey]
    OklahomaCity -- Dallas[cost=2,color=Grey]
    OklahomaCity -- LittleRock[cost=2,color=Grey]
    Dallas -- LittleRock[cost=2,color=Grey]
    Dallas -- Houston[cost=2,color=Grey]
    Dallas -- Houston[cost=2,color=Grey]
    Dallas -- ElPaso[cost=4,color=Red]
    Houston -- NewOrleans[cost=2,color=Grey]
    NewOrleans -- LittleRock[cost=3,color=Green]
    Omaha -- Duluth[cost=2,color=Grey]
    Omaha -- Duluth[cost=2,color=Grey]
    Miami -- NewOrleans[cost=6,color=Red]
    Miami -- Alanta[cost=5,color=Blue]
    Miami -- Charleston[cost=4,color=Pink]
    NewOrleans -- Alanta[cost=4,color=Yellow]
    NewOrleans -- Alanta[cost=4,color=Orange]
    Charleston -- Alanta[cost=2,color=Grey]
    Nashville -- Alanta[cost=1,color=Grey]
    Nashville -- LittleRock[cost=3,color=White]
    SaintLouis -- Nashville[cost=2,color=Grey]
    SaintLouis -- LittleRock[cost=2,color=Grey]
    SaintLouis -- KansasCity[cost=2,color=Blue]
    SaintLouis -- KansasCity[cost=2,color=Pink]
    Chicago -- Duluth[cost=3,color=Red]
    Chicago -- Omaha[cost=3,color=Blue]
    Chicago -- SaintLouis[cost=2,color=Green]
    Chicago -- SaintLouis[cost=2,color=White]
    Chicago -- Pittsburch[cost=3,color=Orange]
    Chicago -- Pittsburch[cost=3,color=Black]
    Chicago -- Toronto[cost=4,color=White]
    Pittsburch -- Toronto[cost=2,color=Grey]
    Duluth -- Toronto[cost=6,color=Pink]
    Pittsburch -- SaintLouis[cost=5,color=Green]
    Pittsburch -- Nashville[cost=4,color=Yellow]
    Pittsburch -- Raleigh[cost=2,color=Grey]
    Pittsburch -- Washington[cost=2,color=Grey]
    Pittsburch -- NewYork[cost=2,color=White]
    Pittsburch -- NewYork[cost=2,color=Green]
    Nashville -- Raleigh[cost=3,color=Black]
    Charleston -- Raleigh[cost=2,color=Grey]
    Charleston -- Washington[cost=2,color=Grey]
    Charleston -- Washington[cost=2,color=Grey]
    Washington -- NewYork[cost=2,color=Orange]
    Washington -- NewYork[cost=2,color=Black]
    NewYork -- Boston[cost=2,color=Yellow]
    NewYork -- Boston[cost=2,color=Red]
    Montreal -- Boston[cost=2,color=Grey]
    Montreal -- Boston[cost=2,color=Grey]
    Montreal -- NewYork[cost=3,color=Blue]
    Montreal -- Toronto[cost=3,color=Grey]
    SautltStMarie -- Toronto[cost=2,color=Grey]
    SautltStMarie -- Montreal[cost=5,color=Black]
    SautltStMarie -- Duluth[cost=3,color=Grey]
    SautltStMarie -- Winnipec[cost=6,color=Grey]
}"


let syntaxTree = getResult <| parse  @"graph board {

    edge[cost=5];
    BallRoom -- Hallway148;
    BallRoom -- Hallway163;
    BilliardRoom -- Hallway135;
    Conservatory -- Lounge;
    Hallway11 -- BallRoom;
    Hallway133 -- Library;
    Kitchen -- Hallway17;
    Kitchen -- Study;
    Library -- Hallway120;
    Study -- Hallway107;
    Hallway94 -- Hall;
    Hallway88 -- Hall;
    Hallway89 -- Hall;
    Hallway71 -- Lounge;
    Hallway53 -- DiningRoom;
    Hallway40 -- DiningRoom;
    Hallway154 -- BilliardRoom;

    edge[cost=1];
    GreenStart -- Hallway170;
    Hallway1 -- Hallway11;
    Hallway10 -- Hallway11;
    Hallway10 -- Hallway14;
    Hallway100 -- Hallway98;
    Hallway101 -- Hallway100;
    Hallway101 -- Hallway107;
    Hallway101 -- Hallway95;
    Hallway102 -- Hallway101;
    Hallway102 -- Hallway113;
    Hallway103 -- Hallway102;
    Hallway103 -- Hallway93;
    Hallway104 -- Hallway103;
    Hallway105 -- Hallway104;
    Hallway106 -- Hallway105;
    Hallway106 -- Hallway121;
    Hallway106 -- Hallway91;
    Hallway108 -- Hallway107;
    Hallway108 -- Hallway114;
    Hallway109 -- Hallway108;
    Hallway109 -- Hallway115;
    Hallway11 -- Hallway12;
    Hallway110 -- Hallway109;
    Hallway110 -- Hallway111;
    Hallway110 -- Hallway116;
    Hallway111 -- Hallway117;
    Hallway112 -- Hallway111;
    Hallway113 -- Hallway107;
    Hallway113 -- Hallway124;
    Hallway114 -- Hallway113;
    Hallway114 -- Hallway115;
    Hallway115 -- Hallway116;
    Hallway116 -- Hallway117;
    Hallway118 -- Hallway112;
    Hallway118 -- Hallway117;
    Hallway118 -- PlumStart;
    Hallway12 -- Hallway13;
    Hallway120 -- Hallway105;
    Hallway121 -- Hallway120;
    Hallway122 -- Hallway120;
    Hallway122 -- Hallway125;
    Hallway123 -- Hallway121;
    Hallway123 -- Hallway122;
    Hallway124 -- Hallway103;
    Hallway125 -- Hallway126;
    Hallway125 -- Hallway129;
    Hallway126 -- Hallway123;
    Hallway126 -- Hallway130;
    Hallway127 -- Hallway125;
    Hallway128 -- Hallway127;
    Hallway128 -- Hallway129;
    Hallway128 -- Hallway158;
    Hallway129 -- Hallway157;
    Hallway130 -- Hallway129;
    Hallway130 -- Hallway141;
    Hallway131 -- Hallway128;
    Hallway132 -- Hallway131;
    Hallway132 -- Hallway133;
    Hallway133 -- Hallway134;
    Hallway134 -- Hallway135;
    Hallway136 -- Hallway35;
    Hallway137 -- Hallway136;
    Hallway137 -- Hallway138;
    Hallway137 -- Hallway145;
    Hallway139 -- Hallway138;
    Hallway139 -- Hallway140;
    Hallway139 -- Hallway147;
    Hallway14 -- Hallway12;
    Hallway14 -- Hallway15;
    Hallway141 -- Hallway142;
    Hallway141 -- Hallway157;
    Hallway142 -- Hallway143;
    Hallway142 -- Hallway156;
    Hallway143 -- Hallway155;
    Hallway144 -- Hallway136;
    Hallway144 -- Hallway33;
    Hallway145 -- Hallway144;
    Hallway145 -- Hallway146;
    Hallway146 -- Hallway138;
    Hallway147 -- Hallway146;
    Hallway148 -- Hallway140;
    Hallway148 -- Hallway147;
    Hallway149 -- Hallway148;
    Hallway15 -- Hallway13;
    Hallway15 -- Hallway16;
    Hallway150 -- Hallway140;
    Hallway150 -- Hallway143;
    Hallway150 -- Hallway149;
    Hallway150 -- Hallway153;
    Hallway151 -- Hallway149;
    Hallway151 -- Hallway152;
    Hallway151 -- Hallway161;
    Hallway153 -- Hallway151;
    Hallway153 -- Hallway154;
    Hallway154 -- Hallway152;
    Hallway155 -- Hallway153;
    Hallway155 -- Hallway156;
    Hallway155 -- Hallway160;
    Hallway157 -- Hallway156;
    Hallway157 -- Hallway158;
    Hallway159 -- Hallway156;
    Hallway159 -- Hallway158;
    Hallway16 -- Hallway17;
    Hallway160 -- Hallway154;
    Hallway160 -- Hallway159;
    Hallway161 -- Hallway162;
    Hallway161 -- Hallway178;
    Hallway162 -- Hallway163;
    Hallway163 -- Hallway164;
    Hallway164 -- Hallway165;
    Hallway164 -- Hallway175;
    Hallway165 -- Hallway174;
    Hallway166 -- Hallway165;
    Hallway167 -- Hallway166;
    Hallway167 -- Hallway172;
    Hallway168 -- Hallway167;
    Hallway168 -- Hallway169;
    Hallway17 -- Hallway18;
    Hallway17 -- Hallway25;
    Hallway170 -- Hallway169;
    Hallway173 -- Hallway166;
    Hallway173 -- Hallway172;
    Hallway174 -- Hallway173;
    Hallway175 -- Hallway174;
    Hallway175 -- Hallway176;
    Hallway175 -- Hallway179;
    Hallway176 -- Hallway163;
    Hallway176 -- Hallway180;
    Hallway177 -- Hallway162;
    Hallway177 -- Hallway176;
    Hallway177 -- Hallway178;
    Hallway178 -- Hallway152;
    Hallway179 -- Conservatory;
    Hallway179 -- Hallway180;
    Hallway18 -- Hallway24;
    Hallway181 -- Hallway177;
    Hallway181 -- Hallway180;
    Hallway182 -- Hallway180;
    Hallway182 -- Hallway183;
    Hallway183 -- Hallway184;
    Hallway184 -- Hallway185;
    Hallway184 -- Hallway189;
    Hallway185 -- PeacockStart;
    Hallway187 -- Hallway181;
    Hallway187 -- Hallway182;
    Hallway187 -- Hallway188;
    Hallway188 -- Hallway183;
    Hallway189 -- Hallway188;
    Hallway189 -- Hallway190;
    Hallway19 -- Hallway18;
    Hallway19 -- Hallway23;
    Hallway190 -- Hallway185;
    Hallway191 -- Hallway9;
    Hallway2 -- Hallway1;
    Hallway2 -- Hallway10;
    Hallway20 -- Hallway19;
    Hallway20 -- Hallway21;
    Hallway20 -- Hallway22;
    Hallway23 -- Hallway22;
    Hallway24 -- Hallway23;
    Hallway24 -- Hallway25;
    Hallway25 -- Hallway26;
    Hallway26 -- Hallway16;
    Hallway26 -- Hallway27;
    Hallway26 -- Hallway31;
    Hallway27 -- Hallway15;
    Hallway27 -- Hallway30;
    Hallway28 -- Hallway13;
    Hallway28 -- Hallway27;
    Hallway28 -- Hallway29;
    Hallway28 -- Hallway32;
    Hallway29 -- Hallway34;
    Hallway3 -- Hallway2;
    Hallway30 -- Hallway29;
    Hallway30 -- Hallway31;
    Hallway32 -- Hallway33;
    Hallway32 -- Hallway34;
    Hallway33 -- BallRoom;
    Hallway33 -- Hallway35;
    Hallway34 -- Hallway36;
    Hallway35 -- Hallway34;
    Hallway35 -- Hallway37;
    Hallway36 -- Hallway37;
    Hallway37 -- Hallway39;
    Hallway38 -- Hallway36;
    Hallway38 -- Hallway40;
    Hallway39 -- Hallway38;
    Hallway39 -- Hallway41;
    Hallway4 -- Hallway1;
    Hallway4 -- Hallway3;
    Hallway40 -- Hallway41;
    Hallway40 -- Hallway42;
    Hallway42 -- Hallway44;
    Hallway43 -- Hallway41;
    Hallway43 -- Hallway42;
    Hallway43 -- Hallway45;
    Hallway44 -- Hallway45;
    Hallway46 -- Hallway44;
    Hallway46 -- Hallway47;
    Hallway47 -- Hallway45;
    Hallway48 -- Hallway52;
    Hallway5 -- Hallway4;
    Hallway5 -- Hallway6;
    Hallway50 -- Hallway46;
    Hallway51 -- Hallway47;
    Hallway51 -- Hallway48;
    Hallway51 -- Hallway50;
    Hallway52 -- Hallway50;
    Hallway52 -- Hallway69;
    Hallway53 -- Hallway50;
    Hallway53 -- Hallway54;
    Hallway54 -- Hallway67;
    Hallway55 -- Hallway54;
    Hallway55 -- Hallway66;
    Hallway56 -- Hallway55;
    Hallway57 -- Hallway56;
    Hallway57 -- Hallway58;
    Hallway58 -- Hallway59;
    Hallway59 -- Hallway60;
    Hallway6 -- Hallway3;
    Hallway60 -- Hallway61;
    Hallway60 -- MustardStart;
    Hallway63 -- Hallway58;
    Hallway63 -- Hallway60;
    Hallway64 -- Hallway57;
    Hallway64 -- Hallway63;
    Hallway64 -- Hallway74;
    Hallway65 -- Hallway56;
    Hallway65 -- Hallway64;
    Hallway66 -- Hallway65;
    Hallway67 -- Hallway66;
    Hallway67 -- Hallway71;
    Hallway68 -- Hallway52;
    Hallway68 -- Hallway53;
    Hallway68 -- Hallway67;
    Hallway68 -- Hallway70;
    Hallway69 -- Hallway76;
    Hallway7 -- Hallway5;
    Hallway7 -- Hallway8;
    Hallway70 -- Hallway69;
    Hallway70 -- Hallway71;
    Hallway71 -- Hallway72;
    Hallway72 -- Hallway66;
    Hallway72 -- Hallway73;
    Hallway73 -- Hallway65;
    Hallway73 -- Hallway74;
    Hallway74 -- Hallway75;
    Hallway75 -- Hallway61;
    Hallway75 -- Hallway63;
    Hallway76 -- Hallway77;
    Hallway76 -- Hallway85;
    Hallway78 -- Hallway77;
    Hallway78 -- Hallway79;
    Hallway78 -- Hallway83;
    Hallway79 -- Hallway82;
    Hallway80 -- Hallway79;
    Hallway80 -- Hallway81;
    Hallway82 -- Hallway81;
    Hallway82 -- Hallway83;
    Hallway84 -- Hallway77;
    Hallway84 -- Hallway83;
    Hallway84 -- Hallway85;
    Hallway85 -- Hallway70;
    Hallway87 -- Hallway48;
    Hallway87 -- Hallway88;
    Hallway89 -- Hallway88;
    Hallway89 -- Hallway90;
    Hallway9 -- Hallway8;
    Hallway91 -- Hallway90;
    Hallway92 -- Hallway104;
    Hallway92 -- Hallway106;
    Hallway93 -- Hallway92;
    Hallway93 -- Hallway94;
    Hallway94 -- Hallway102;
    Hallway95 -- Hallway94;
    Hallway96 -- Hallway95;
    Hallway96 -- Hallway97;
    Hallway98 -- Hallway97;
    Hallway98 -- Hallway99;
    ScarlettStart -- Hallway81;
    WhiteStart -- Hallway191;
}"

let gameBoard = syntaxTree |> createGraph |> toAdjancencyGraph

let printRoute (cost,route) = 
    route 
    |> List.ofSeq 
    |> List.map (fun node -> printfn "%A - %A" node gameBoard.[node] ) 
    |> ignore

let route = shortestPathBetween gameBoard "WhiteStart" "BallRoom";;

route
do printRoute route


