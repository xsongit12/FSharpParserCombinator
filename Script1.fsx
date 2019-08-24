#load "C:/Users/hoxus/source/repos/FShParser/CombinatorialParser.fs"
#load "C:/Users/hoxus/source/repos/FShParser/jsonParser.fs"



open jsonParser.JSONParser
open CombinatorialParser.ParserTypes
open CombinatorialParser.ParserCombinators



run parseJObject "{\"apple\": 12, \"jews\": \"22 Million\"}"