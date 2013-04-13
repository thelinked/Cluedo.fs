﻿#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#r @"..\packages\FSharpx.Core.1.8.3.0\lib\40\FSharpx.Core.dll"
#load "dot.fs"
#load "graph.fs"

open Cluedo.Dot
open Cluedo.Graph

let g: char Graph = 
        (['b';'c';'d';'f';'g';'h';'k'], 
         [('b','c');
          ('b','f');
          ('c','f');
          ('f','k');
          ('g','h')])  

let ga: char AdjacencyGraph = 
        [('b',['c'; 'f']); 
         ('c',['b'; 'f']); 
         ('d',[]); 
         ('f',['b'; 'c'; 'k']); 
         ('g',['h']); 
         ('h',['g']); ('k',['f'])]

let result = getResult (parse  @"graph graphname {
     a -- b -- c;
     b -- z;
     b -- e;
     f -- e;
     f -- aaa;
     aaa-- aab;
     aab -- aac;
 }")

let graph: string Graph  = createGraph result
let adjGraph = toAdjancencyGraph graph
let p = depthFirst "a" "aac" adjGraph


let syntaxTree = getResult (parse  @"graph board {
    BallRoom -- Hallway148;
    BallRoom -- Hallway163;
    BilliardRoom -- Hallway135;
    Conservatory -- Lounge;
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
    Hallway11 -- BallRoom;
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
    Hallway133 -- Library;
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
    Hallway154 -- BilliardRoom;
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
    Hallway40 -- DiningRoom;
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
    Hallway53 -- DiningRoom;
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
    Hallway71 -- Lounge;
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
    Hallway88 -- Hall;
    Hallway89 -- Hall;
    Hallway89 -- Hallway88;
    Hallway89 -- Hallway90;
    Hallway9 -- Hallway8;
    Hallway91 -- Hallway90;
    Hallway92 -- Hallway104;
    Hallway92 -- Hallway106;
    Hallway93 -- Hallway92;
    Hallway93 -- Hallway94;
    Hallway94 -- Hall;
    Hallway94 -- Hallway102;
    Hallway95 -- Hallway94;
    Hallway96 -- Hallway95;
    Hallway96 -- Hallway97;
    Hallway98 -- Hallway97;
    Hallway98 -- Hallway99;
    Kitchen -- Hallway17;
    Kitchen -- Study;
    Library -- Hallway120;
    ScarlettStart -- Hallway81;
    Study -- Hallway107;
    WhiteStart -- Hallway191;
}")


let gameBoard = syntaxTree |> createGraph |> toAdjancencyGraph
let route = depthFirst "WhiteStart" "BallRoom" gameBoard

let getCost (a,b) = 
    1

route |> Seq.pairwise |> Seq.map getCost |> Seq.reduce (+)
