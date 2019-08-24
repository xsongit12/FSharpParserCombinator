#load "CombinatorialParser.fs"
#load "jsonParser.fs"



open jsonParser.JSONParser
open CombinatorialParser.ParserTypes
open CombinatorialParser.ParserCombinators

run (parseChar 'a' >>. parseChar 'b') "ab"