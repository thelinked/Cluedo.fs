namespace Cluedo
module Graph =
    open FSharpx.Collections
    open FParsec
    open Cluedo.Dot

    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list

    type 'a Node = 'a * 'a list
    type 'a AdjacencyGraph = 'a Node list
        
    let visitStatements collector empty ((_,_,smts):DotAST) =
        smts
        |> List.fold (fun acc nm -> 
            match nm with 
            | EdgeStatement(s,_) -> collector s acc
            | NodeStatement(s,_) -> acc) empty
        |> Seq.distinct 
        |> List.ofSeq

    let getNodes ((_,_,smts):DotAST) = visitStatements List.append []

    let getEdges ((_,_,smts):DotAST) =
        visitStatements (fun s acc -> Seq.append (Seq.pairwise s) acc) Seq.empty

    let createGraph (dotTree:DotAST) = 
        (getNodes dotTree,getEdges dotTree)

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

        let rec loop (queue:Queue<'a>) route visited = 
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