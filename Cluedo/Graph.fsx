#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
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