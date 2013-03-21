#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#load "Dot.fs"

open FParsec
open Cluedo.Dot

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


test ID "_lol"
test ID @"""jsdhfki\""sdjbgf"""
test ID "-45"
test ID "<lol>"


let undirected_edge_smt = edgeStatement "--"
test undirected_edge_smt "a -- b;"
test undirected_edge_smt "4 -- b -- c;"
test (many undirected_edge_smt) @"a -- b -- c -- d;
        4 -- b -- c;"


test dot  @"digraph graphname {
     a -> b -> c;
     b -> d;
 }"

parse  @"graph graphname {
     a -- b -- c;
     b -- d;
 }"