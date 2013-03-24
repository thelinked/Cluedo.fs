namespace Cluedo
module Graph =
    open FParsec
    open Cluedo.Dot

    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list

    type 'a Node = 'a * 'a list
    type 'a AdjacencyGraph = 'a Node list

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

    let rec collapseListListToSet listlist =
        let union l r =  List.append l r |> Seq.distinct |> List.ofSeq
        ([],listlist) ||> List.fold (fun nm acc -> union nm acc) 

    let createGraph dotTree = 
        let edgeSmts = getEdgeSmts dotTree
        let nodes = collapseListListToSet edgeSmts
        let edges = (List.map pairs edgeSmts) |> List.fold List.append [] 

        nodes,edges

    let toAdjancencyGraph ((ns, es): 'a Graph) : 'a AdjacencyGraph = 
        let nodeMap = ns |> List.map (fun n -> n,[]) |> Map.ofList
        (nodeMap,es)
        ||> List.fold (fun map (a,b) -> map 
                                        |> Map.add a (b::map.[a]) 
                                        |> Map.add b (a::map.[b]))
        |> Map.toList

    let toGraph (ns: 'a AdjacencyGraph): 'a Graph = 
        let sort ((a,b) as e) = if a > b then (b,a) else e
        let nodes = ns |> List.map fst
        let edges = (Set.empty, ns)
                    ||> List.fold(fun set (a,ns) -> (set,ns) ||> List.fold(fun s b -> s |> Set.add (sort (a,b))))
                    |> Set.toSeq
                    |> Seq.sort
                    |> Seq.toList
        nodes,edges