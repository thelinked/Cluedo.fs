namespace Cluedo
module Graph =
    open FSharpx.Collections
    open FParsec
    open Cluedo.Dot

    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list

    type 'a Node = 'a * 'a list
    type 'a AdjacencyGraph = 'a Node list

    //['b';'c';'d';] -> [('b','c');('c','d');]
    let pairs list = 
        let rec pairsHelper acc list = 
            match list with
            | x::y::tail -> pairsHelper ((x,y)::acc) (y::tail)
            | _ -> acc
        pairsHelper [] list

    let rec collapseListListToSet listlist =
        listlist 
        |> List.fold (fun nm acc -> List.append nm acc) []
        |> Seq.distinct |> List.ofSeq

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
                    ||> List.fold (fun set (a,ns) -> (set,ns) ||> List.fold(fun s b -> s |> Set.add (sort (a,b))))
                    |> Set.toSeq |> Seq.sort |> Seq.toList
        nodes,edges

    let depthFirst start finish (g: 'a AdjacencyGraph) = 
        let lookup = g |> Map.ofList

        let rec loop (queue:Queue<string>) route visited = 
            if not queue.IsEmpty then
                if queue.Head = finish then 
                    List.rev (queue.Head::route)
                else
                    let adj = lookup.[queue.Head] |> List.filter (fun n ->  not (Set.contains n visited))
                    let newQueue = (queue.Tail,adj) ||> List.fold (fun acc next -> Queue.conj next acc)
                    let newRoute = queue.Head::route
                    let newVisited = (Set.add queue.Head visited)

                    loop newQueue newRoute newVisited
            else failwith "Can not reach destination"

        loop (Queue.conj start Queue.empty) [] (Set.singleton start)