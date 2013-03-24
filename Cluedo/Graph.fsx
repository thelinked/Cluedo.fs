#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#load "dot.fs"
#load "graph.fs"

open Cluedo.Dot
open Cluedo.Graph

let result = getResult (parse  @"graph graphname {
     a -- b -- c;
     b -- z;
     b -- e;
     f -- e;
 }")


let lol: string Graph  = createGraph result
