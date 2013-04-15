namespace Cluedo
module Graph =
    open FSharpx.Collections
    open FParsec
    open Cluedo.Dot

    type 'a Edge = 'a * 'a
    type 'a Graph = 'a list * 'a Edge list

    type 'a Node = 'a * 'a list
    type 'a AdjacencyGraph = 'a Node list

    type 'a Cost when 'a : comparison = Map<'a*'a,int>

    let foldStatements collectAttribs collectNodes collectEdges empty ((_,_,smts):DotAST) = 
        smts
        |> List.fold (fun acc nm -> 
            match nm with 
            | AttributeStatement(t,attribs) -> collectAttribs t attribs acc
            | NodeStatement(s,a) -> collectNodes s a acc
            | EdgeStatement(s,a) -> collectEdges s a acc) empty

    let foldEdges collecter empty dotAST = 
        foldStatements (fun _ _ acc -> acc) (fun _ _ acc -> acc) collecter empty dotAST
        |> Seq.distinct 
        |> List.ofSeq

    let createGraph dotTree :string Graph = 
        let nodes = foldEdges (fun s a acc -> s @ acc) [] dotTree
        let edges = foldEdges (fun s a acc -> Seq.append (Seq.pairwise s) acc) Seq.empty dotTree
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
        let rec loop (queue:Queue<'a>) route visited = 
            if not queue.IsEmpty then
                if queue.Head = finish then 
                    (queue.Head::route) |> List.rev |> Seq.ofList
                else
                    let adj = lookup.[queue.Head] |> List.filter (fun n ->  not (Set.contains n visited))
                    let queue' = (queue.Tail,adj) ||> List.fold (fun acc next -> Queue.conj next acc)
                    let route' = queue.Head::route
                    let visited' = (Set.add queue.Head visited)
                    loop queue' route' visited'
            else failwith "Can not reach destination"

        loop (Queue.conj start Queue.empty) [] (Set.singleton start)
