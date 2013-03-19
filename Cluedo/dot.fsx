#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsecCS.dll"
#r @"..\packages\FParsec.0.9.2.0\lib\net40\FParsec.dll"
#load "Dot.fs"

open FParsec
open Dot

test ID "_lol"
test ID @"""jsdhfki\""sdjbgf"""
test ID "-45"
test ID "<lol>"



