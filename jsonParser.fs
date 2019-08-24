
module jsonParser


module JSONParser = 

    open CombinatorialParser
    open CombinatorialParser.ParserCombinators

    type JValue = 
        | JString of string
        | JNum of float
        | JObject of Map<JValue, JValue>
        | JBool of bool
        | JNull
        | JArray of JValue list


    // Parse the null string
    let parseJNull = (StringParsers.parseString "null") |>> (fun x -> JNull) <?> "null"

    // JBoolean Parser

    let parseTrue =
       StringParsers.parseString "true" |>> (fun x -> JBool true)

    let parseFalse = 
        StringParsers.parseString "false" |>> (fun x -> JBool false)

    let parseJBool = 
        parseTrue <|> parseFalse <?> "bool"

    // JNum Parser

    let parseInteger = 
        NumericalParsers.parseInt |>> (float) 
        
    let parseJNum = 
        (NumericalParsers.parseFloat) <|> parseInteger 

    let parseExponent = 
        (parseChar 'e' <|> parseChar 'E') >>. parseJNum

    let parseTotalJNum =
        //Parser Returns the base and an opt exponent
        //Function converts the two to the float representation
        let tupleToFloat (nbase, nexp) = 
            match nexp with 
            |Some n -> nbase ** n
            |None -> nbase

        (parseJNum .>> WhiteSpaceParsers.spaces) .>>. (opt parseExponent)
        |>> tupleToFloat |>> (JNum)
    
        
    // JString Parser


    let parseJNormalChar = 
        let label = "char"
        satisfy (fun ch -> ch <> '\\' && ch <> '\"') label 


    let parseEscapeChar = 
        let label = "escape char"
        let escapeChars = 
           [
           ("\\\"",'\"')      // quote
           ("\\\\",'\\')      // reverse solidus
           ("\\/",'/')        // solidus
           ("\\b",'\b')       // backspace
           ("\\f",'\f')       // formfeed
           ("\\n",'\n')       // newline
           ("\\r",'\r')       // cr
           ("\\t",'\t')       // tab
           ]

        let mappingfn (key, value) = 
            StringParsers.parseString key |>> (fun _ -> value)

        List.map  mappingfn escapeChars 
        |> choose <?> label


    let parseJString = 
        let bookends = parseChar '"'
        let parseString = many (parseJNormalChar <|> parseEscapeChar) |>> StringParsers.charListToString
        bookends >>. parseString .>> bookends |>> JString <?> "string"

    // Parse JArray

    //So arrays can nest infinitely inside the starting array:

    //IE: [1, [[],[]], 2] is perfectly valid
    //So we need a mutually recursive defn
    //Namely, to parse an array, we need to know how to parse an array

    let forwardReferenceParser<'A>() = 
        let dummyParser:ParserTypes.Parser<'A*Input.InputState> = 
            let innerfn input = failwith "Unforwarded Parser!"
            {parseFn = innerfn; label = "unknown"}
        
        let parserRef = ref dummyParser

        let innerFn input =
            ParserTypes.runOnInp !parserRef input

        let wrapperParser = {ParserTypes.parseFn = innerFn; ParserTypes.label = "unknown"}
        wrapperParser, parserRef

    let jValue,jValueRef = forwardReferenceParser<JValue>()

    jValueRef := parseTotalJNum

    let parseJArray = 
        let left = parseChar '[' .>>. WhiteSpaceParsers.spaces
        let right = parseChar ']' .>> WhiteSpaceParsers.spaces
        let comma = parseChar ',' .>> WhiteSpaceParsers.spaces
        let value = jValue
        
        //Need to create a forward reference so we can parse an array before
        //defining how to parse an array

        let values = sepby1 comma value
        left >>. values .>> right |>> JArray


    let parseJObject = 
        
        //Syntax Parsers
        let left = parseChar '{' .>> WhiteSpaceParsers.spaces
        let right = parseChar '}' .>> WhiteSpaceParsers.spaces
        let parseKey = parseJString .>> WhiteSpaceParsers.spaces
        let parseValue = jValue .>> WhiteSpaceParsers.spaces
        let parsecomma = parseChar ',' .>> WhiteSpaceParsers.spaces
        let parsecolon = parseChar ':' .>> WhiteSpaceParsers.spaces
        
        let parseKeyValue = parseKey .>> parsecolon .>>. parseValue
        
        let parseKeyValues = sepby1 parsecomma parseKeyValue

        left >>. parseKeyValues .>> right  |>> Map.ofList |>> JObject


    jValueRef := [
        parseTotalJNum
        parseJArray
        parseJBool
        parseJString
        parseJNull
        parseJObject
        ] |> choose
