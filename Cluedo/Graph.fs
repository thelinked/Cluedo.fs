namespace Cluedo
module Graph =
    open FSharpx.Collections
    open FParsec
    open Cluedo.Dot

    type 'a Edge = 'a * 'a * int
    type 'a Graph = 'a list * 'a Edge list
    type 'a AdjacencyGraph when 'a : comparison = Map<'a, Map<'a, int>>

    let foldStatements collectAttribs collectNodes collectEdges empty ((_,_,smts):DotAST) = 
        smts
        |> List.fold (fun acc nm -> 
            match nm with 
            | AttributeStatement(t,attribs) -> collectAttribs t attribs acc
            | NodeStatement(s,a) -> collectNodes s a acc
            | EdgeStatement(s,a) -> collectEdges s a acc) empty

    let foldEdges collecter empty dotAST = 
        foldStatements (fun _ _ acc -> acc) (fun _ _ acc -> acc) collecter empty dotAST
        |> Seq.distinct |> List.ofSeq

    let getEdges dotTree = 
        let findCost defaultCost = function
            | AllEdges, hd::_ when fst hd = "cost" -> System.Int32.Parse(snd hd)
            | _ -> defaultCost
            
        let collectAttributes t attributes (c,m) = 
            (findCost c (t,attributes), m)

        let collectEdges nodeList (attributes: Attribute list option) (c,m) = 
            let cost = match attributes with
                       | Some(a) -> findCost c (AllEdges,a)
                       | None -> c

            let m' = nodeList |> Seq.pairwise |> Seq.map (fun (f,s) -> f,s,cost) |> Seq.append m
            (c, m')

        foldStatements collectAttributes (fun _ _ acc -> acc) collectEdges (1,Seq.empty) dotTree 
        |> snd |> Seq.distinct |> List.ofSeq

    let createGraph dotTree :string Graph = 
        let nodes = foldEdges (fun s a acc -> s @ acc) [] dotTree
        let edges = getEdges dotTree
        nodes,edges

    let toAdjancencyGraph ((ns, es): 'a Graph) : 'a AdjacencyGraph = 
        let nodeMap = ns |> List.map (fun n -> n,Map.empty) |> Map.ofList
        (nodeMap,es)
        ||> List.fold (fun acc (a,b,c) -> acc 
                                         |> Map.add a ( acc.[a] |> Map.add b c) 
                                         |> Map.add b ( acc.[b] |> Map.add a c))

    let shortestPathBetween (distancesBetween:Map<'a,Map<'a,int>>) start finish =
      let rec searchForShortestPath current distanceSoFar visitedNodes accMap =
        let visitDestinations m =
          (m, distancesBetween.[current])
          ||> Map.fold
              (fun acc node distance ->
                  searchForShortestPath node (distance + distanceSoFar) (visitedNodes @ [node]) acc)

        match Map.tryFind current accMap with
        | None -> accMap |> Map.add current (distanceSoFar, visitedNodes) |> visitDestinations
        | Some x ->
            let (shortestKnownPath, _) = x
            if distanceSoFar < shortestKnownPath then
              accMap |> Map.add current (distanceSoFar, visitedNodes) |> visitDestinations
            else accMap

      let shortestPaths = searchForShortestPath start 0 [] Map.empty
      shortestPaths.TryFind finish

    let stripNodes (nodes:'a list) (graph:Map<'a,Map<'a,int>>)= 
        graph
        |> Map.removeMany nodes
        |> Map.map (fun name connections -> 
            Map.filter (fun n cost -> 
                List.exists ((=)n) nodes |> not) connections)

    let shortestPathBetweenBlocked (distancesBetween:Map<'a,Map<'a,int>>) start finish (blocked:'a list) =
        shortestPathBetween (distancesBetween |> stripNodes blocked) start finish