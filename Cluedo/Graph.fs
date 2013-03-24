namespace Cluedo
module Graph =
    open FParsec
    open Cluedo.Dot

    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list

    let g: char Graph = 
            (['b';'c';'d';'f';'g';'h';'k'], 
             [('b','c');
              ('b','f');
              ('c','f');
              ('f','k');
              ('g','h')])  

    type 'a Node = 'a * 'a list
    type 'a AdjacencyGraph = 'a Node list

    let ga: char AdjacencyGraph = 
        [('b',['c'; 'f']); 
         ('c',['b'; 'f']); 
         ('d',[]); 
         ('f',['b'; 'c'; 'k']); 
         ('g',['h']); 
         ('h',['g']); ('k',['f'])]

    //Helper functions to be used with parser results
    let getType = function | (t,_,_) -> t
    let getName = function | (_,n,_) -> n
    let getEdgeSmts = function | (_,_,e) -> e

    //['b';'c';'d';] -> [('b','c');('c','d');]
    let rec pairs list = 
        let rec pairsHelper acc list = 
            match list with
            | x::y::tail -> pairsHelper ((x,y)::acc) (y::tail)
            | _ -> acc
        pairsHelper [] list

    let rec collapseListToSet list =
        let union l r =  List.append l r |> Seq.distinct |> List.ofSeq
        
        match list with 
        | hd::tl -> union hd (collapseListToSet tl)
        | [] -> []

    let generateEdgesForUndirectedGraph gtype edges = 
        let reverseEdges = List.map (fun (x,y) -> (y,x))
        
        match gtype with
        | Graph -> List.append (reverseEdges edges) edges
        | Digraph -> edges

    let createGraph dotTree = 
        let edgeSmts = getEdgeSmts dotTree
        let gtype = getType dotTree
        let set = collapseListToSet edgeSmts

        let edges = List.fold List.append [] (List.map pairs edgeSmts) 
                    |> generateEdgesForUndirectedGraph gtype
        set,edges