module CombinatorialParser 

    open System

    module Input = 
        type Position = {
            line:int
            column:int
        }

        type InputState = {
            text:string[]
            position:Position
        }

        //Process Input

        let initialPosition = {line = 0; column = 0}

        let incColumn position = 
            match position with 
                |{Position.column = col; Position.line = ln} -> {column = col + 1; line = ln}

        let incLine position = 
            match position with 
                |{Position.column = _; Position.line = ln} -> {column = 0; line = ln + 1}

        let inputFromStr rawtext =
            if String.IsNullOrEmpty(rawtext) then 
                {text = [||]; position = initialPosition}
            else
                let seperators = [|"\r\n";"\n"|]
                let lines = rawtext.Split(seperators, StringSplitOptions.None)
                {text = lines; position = initialPosition}

        //Handling the Input Stream

        let getLine inputPos = 
            let linenum = inputPos.position.line

            if linenum >= inputPos.text.Length then
                "EOF"
            else
                let line = inputPos.text.[linenum]
                line

        let nextChar inputPos =
            //Takes an inputPos: InputPosition
            //Returns: (option char, new InputPosition)
            let linepos = inputPos.position.line
            let colpos = inputPos.position.column
            let totallines = inputPos.text.Length
            //Case where the linepos is greater than the total number of lines
            //This is the EOF case
            if linepos >= totallines then 
                None, inputPos
            else
                let currLine = getLine inputPos
                let currLineLength = currLine.Length
                if colpos < currLineLength then
                //Everything works here, we return the character and return the new position
                    let retChar = currLine.[colpos]
                    let newPos = incColumn inputPos.position
                    let newIPos = {inputPos with position=newPos}
                    Some retChar, newIPos
                else
                    let retChar = '\n'
                    let newPos = incLine inputPos.position
                    let newIPos = {inputPos with position = newPos}
                    Some retChar, newIPos


    module ParserTypes =

        open Input

        type ParserPosition = {
            currLine: string
            parsePosition:Position
        }

        type ParseLabel = string

        type ParseFailure = ParseLabel * string * ParserPosition

        type Output<'A> = 
            |Failure of ParseFailure
            |Success of 'A

        type Parser<'A> = {
            label: ParseLabel
            parseFn: InputState -> Output<'A> 
        }


        //Pretty Printing Errors

        let printResult result =
            match result with
            | Success (value,input) -> 
                printfn "%A" value
            | Failure (label, error, parsepos) -> 
                printfn "Error with Parser: %s \n %s \n at %A '%s'" label error parsepos.parsePosition parsepos.currLine

        //Run Parser
    
        let runOnInp parser input = 
            // call inner function on input
            parser.parseFn input
    
        let run parser inputStr = 
            runOnInp parser (inputFromStr inputStr)


        // Convert InputState into ParserPosition
        let parsePositionFromState inputState = 
            let (pos:Position) = inputState.position
            {ParserPosition.currLine = getLine inputState; ParserPosition.parsePosition = pos}





    //Testing charachter processing

    (*
    let rec readAllChars input =
        [
            let charOpt, remainingInput = nextChar input 
            match charOpt with
            | None -> 
                // end of input
                ()
            | Some ch -> 
                // return first character
                yield ch
                // return the remaining characters
                yield! readAllChars remainingInput
        ]

    *)



    module ParserCombinators = 
    
        open ParserTypes
        open Input

        //Primitive Parsers

        let satisfy predicate label = 
             //predicate is a function that takes a char and returns a boolean
             //label is the label to attach to this primitive parser
             //Satisfy works on a charachter by charachter basis
             let innerFn (input:InputState) = 
                 let someChar, remainingInput  = nextChar input
                 match someChar with
                 | None -> Failure (label, "No More Input", parsePositionFromState input)
                 | Some ch -> 
                     if predicate ch then 
                         Success (ch, remainingInput)
                     else
                         Failure(label, sprintf "Unexpected '%c'" ch, parsePositionFromState input)
             {parseFn = innerFn; label = label}
 
        let parseChar matchChar = 
            let predicate (x:char):bool = (matchChar = x)
            let label = sprintf "%c" matchChar
            satisfy predicate label


        //Changing Labeling

        //Basically, run the parser, and then when it fails, replace the label
        let changeLabel parser newlabel = 
            let inner inputState =
                let output = runOnInp parser inputState
                match output with
                | Success s -> Success s
                | Failure (oldlabel, err, pos) -> Failure (newlabel, err, pos)
            {parseFn = inner; label=newlabel}
    
        let (<?>) p label = changeLabel p label

        //Monadic Structures

        let bindP f parser = 
            //parser: InputState -> Output(A)
            //f: A -> Parser (B) 
            let label = "unknown"
            let inner inputState = 
                let result = runOnInp parser inputState
                match result with 
                    |Failure (label, err, pos) -> Failure (label, err, pos)
                    |Success(token, remaining) -> runOnInp (f token) remaining
            {parseFn = inner; label = label}

        let (>>=) p f = bindP f p

        let returnP obj = 
            let innerFn inputState =
                Success(obj, inputState)
            {parseFn = innerFn; label = "Ident"}


        let mapP f p =
            //p: InputState -> Output<A> 
            //f: A -> B 
            bindP (f >> returnP) p
        
        // f x >> g x = g(f(x))
        // a -> b -> Parser (b)

        let (<!>) = mapP
        let (|>>) x f = mapP f x

    //Some Fundamental Parser Combinators
    
        let andThen parser1 parser2 =
            let innerFn inputState = 
                let output = runOnInp parser1 inputState
                match output with
                    |Failure fail -> Failure fail
                    |Success (ch, remainingState) -> 
                        let output2 = runOnInp parser2 remainingState
                        match output2 with 
                            |Failure fail -> Failure fail
                            |Success (nextCh, endState) -> Success ((ch,nextCh), endState)
            {parseFn = innerFn; label = parser1.label + " AndThen " + parser2.label}

        let andThen2 parser1 parser2 = 
            parser1 >>= (fun result1 -> 
                parser2 >>= (fun result2 -> 
                    returnP (result1, result2) ))

        let (.>>.) p1 p2 = andThen2 p1 p2

        let (.>>) p1 p2 = mapP (fun (x,_) -> x) (andThen2 p1 p2)

        let (>>.) p1 p2 = mapP (fun (_,y) -> y) (andThen2 p1 p2)


        let orElse p1 p2 = 
            let innerFn inputState = 
                let output = runOnInp p1 inputState
                match output with 
                    |Success succ -> Success succ
                    |Failure _ -> runOnInp p2 inputState
            {parseFn = innerFn; label = "orElse"}

        let (<|>) p1 p2 = orElse p1 p2

            //Parse Many

        let rec manyRec parser inputStream =
            let output = runOnInp parser inputStream
            match output with 
            | Failure err -> ([], inputStream)
            | Success (ch, endState) -> 
                let nextCh, nextState = manyRec parser endState
                (ch::nextCh, nextState)

        let many parser =
            //Basically, run this parser until it can't be run 
            //If it can't be run, return nothing
            let innerFn inputStream =
               let result = manyRec parser inputStream
               Success result
            {parseFn = innerFn; label = "many" + parser.label}
        
        let many1 parser =
            let innerFn inputState = 
                let firstOutput = runOnInp parser inputState
                match firstOutput with
                | Failure err -> Failure err
                | Success (ch, remaining) -> 
                    let next, nextState = manyRec parser remaining
                    Success ((ch::next),nextState)


            {parseFn = innerFn; label = "many1" + parser.label}

            //Parse Choice

        let choose parserlist = 
            List.reduce orElse parserlist

        let anyOf charList = 
            (List.map parseChar charList)
            |> choose

        //Apply

        //Think of it like function application in parser land

        let applyP fP xP = 
        //fP: Parser(A -> B)
        //xP: Parser (A)
        //Used to transform a parserA into parserB
            (fP .>>. xP)
            |> mapP (fun (f,x) -> f x)


        let (<*>) fP xP = applyP fP xP

        let lift2 f xP yP = 
        //f is a two parameter function
        //use lift2 to lift it into parser space
            (returnP f) <*> xP <*> yP


        //Optional Parser

        let opt parser =
            let some = parser |>> Some
            let none = returnP None
            some <|> none <?> ("opt " + parser.label)


        let sepby1 parseSep parser = 
            //Basically, lets us alternate these parsers
            let sepThenp = parseSep >>. parser
            parser .>>.  (many sepThenp) 
            |>> (fun (x, xs) -> x::xs)

    module StringParsers =
    
        open ParserCombinators

        let rec sequence parserList  =
            let cons first remaining = first::remaining
            let consP = lift2 cons
            match parserList with
            |[] -> returnP []
            |x::xs -> consP x (sequence xs)

        let charListToString charList = 
            String(List.toArray charList)

        let parseString string =   
            Seq.toList string 
            |> List.map parseChar
            |> sequence
            |> mapP charListToString


        let parseAnyChar = 
            let predicate = Char.IsLetter
            satisfy predicate "anyChar"

    module NumericalParsers = 

        open ParserCombinators
        // Numerical Parsers

        let charListToString charList = 
            String(List.toArray charList)

        let parseDigit =
            let predicate = Char.IsDigit
            satisfy predicate "Digit Parser"

        let parseMinus = 
            parseChar '-'

        let parseInt = 
    
            let convInt (sign, digits) =
                let value = digits |> int
                match sign with
                |None -> value
                |Some ch -> -1 * value

            convInt  <!>  ((opt parseMinus) .>>. ((many1 parseDigit) |>> charListToString))


            //Parse Float needs a real float.
            //No partials, ie. 100. does not parse
            //Nor does 100 or .12 

        let parseFloat = 
            let label = "Float Parser" 
   
            let cleanOutput (((sign,digits1),point),digits2) = 
                let value = sprintf "%s.%s" digits1 digits2 |> float
                match sign with
                    | Some ch -> -value  
                    | None -> value

            let parseDigitsAndClean = (many1 parseDigit) |>> charListToString

            let parser = (opt parseMinus) .>>. (parseDigitsAndClean) .>>. (opt (parseChar '.')) .>>. (parseDigitsAndClean)
            parser |>> cleanOutput <?> label 

    module WhiteSpaceParsers =
    
        open ParserCombinators
    
        let space = 
            let predicate = Char.IsWhiteSpace
            satisfy predicate "whitespace"

        let spaces = 
            many space

        let spaces1 = 
            many1 space