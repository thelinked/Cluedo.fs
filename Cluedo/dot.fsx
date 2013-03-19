#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#load "Dot.fs"

open FParsec
open Dot

test ID "_lol"
test ID @"""jsdhfki\""sdjbgf"""
test ID "-45"
test ID "<lol>"



test undirected_edge_smt "a -- b;"
test undirected_edge_smt "4 -- b -- c;"
test (many undirected_edge_smt) @"a -- b -- c -- d;
        4 -- b -- c;"


test directed_graph  @"digraph graphname {
     a -> b -> c;
     b -> d;
 }"

test undirected_graph  @"graph graphname {
     a -- b -- c;
     b -- d;
 }"