open System

type Output<'A> = 
    | Success of 'A 
    | Failure of string

type Parser<'A> = Parser of (string -> Output<'A>) 

//Type Inference OP. It's pretty nifty how this works
//Via type inference, we can give a wrapper type 'Parser' without actually specifying the type parameter

let parseChar (matchChar:char) = 
    let innerfn stream = 
        if String.IsNullOrEmpty(stream) then Failure "Input is Empty or Null"
        else
            let token = stream.[0]
            if token = matchChar then 
                let remainder = stream.[1..]
                Success(token, remainder)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" matchChar token
                Failure msg
    Parser innerfn

//Might think we need to specify type of innerfn, but Fsharp infers the type and it exists

let run parser (input:string) = 
    let (Parser innerfn) = parser
    innerfn input
    
let andThen parser1 parser2 = 
    let (Parser inner1) = parser1
    let (Parser inner2) = parser2
    let innerfn stream = 
        let output1 = inner1 stream 
        match output1 with 
        | Failure err -> Failure err
        | Success (resA, remainder1) ->
            let output2 = inner2 remainder1
            match output2 with
            | Failure err -> Failure err
            | Success (resB, remainder2) ->
                let result = (resA, resB)
                let remainder = remainder2
                Success(result, remainder)
    Parser innerfn

let (.>>.) = andThen

let orElse parser1 parser2 = 
    let (Parser inner1) = parser1
    let (Parser inner2) = parser2
    let innerfn stream = 
        let output1 = inner1 stream
        match output1 with
        |Success (result, remainder) -> Success (result, remainder)
        |Failure err1 -> 
            let output2 = inner2 stream
            match output2 with 
                |Success (result, remainder) -> Success (result, remainder)
                |Failure err2 -> Failure (err1 + err2)
    Parser innerfn

let (<|>) = orElse

let choose parserList = 
    List.reduce orElse parserList 


//Pipelining the list into the map function then composing it into a giant OR to get one parser
let anyOf charList = 
    charList 
    |> List.map parseChar 
    |> choose

//mapP basically takes a function and then applies it to the delayed result of the parser
//IE. mapP 'A'+ parseA 'AT' gives Success ('AA',T) 
let mapP f parser = 
    let innerfn stream = 
        let result = run parser stream
        match result with 
            |Success (value, remainder) ->
                let newvalue = f value
                Success (newvalue, remainder)
            |Failure err ->
                Failure err
    Parser innerfn

let (<!>) = mapP
let (|>>) x f = mapP f x

//      Test CODE Section       //

let parseDigit = anyOf ['0'..'9']

let parseThreeDigitsAsStr =
    let tupleParser =
        parseDigit .>>. parseDigit .>>. parseDigit

    let transformTuple ((c1, c2), c3) =
        String [| c1; c2; c3 |]
    mapP transformTuple tupleParser

//      Test CODE End           //

//The Usual Monadic Return and Apply 

let returnP x = 
    let innerfn stream =
        Success(x, stream)
    Parser innerfn

let applyP fP xP =
    //parser that runs the function then the parser
    (fP .>>. xP)
    // map the pair by applying f to x
    |> mapP (fun (f,x) -> f x)

let (<*>) = applyP

let lift2 f xP yP = 
    //Assumes f is a two parameter function
    //This function lifts it into Parser World
    returnP f <*> xP <*> yP

let rec sequence parserList = 
    let cons head tail = head::tail
    let consP = lift2 cons
    match parserList with
        |[] -> returnP []
        |x::xs -> consP x (sequence xs)


//Function that takes  a list of chars into a string
//Basically for pretty printing when we look at output
let charListToStr charList =
    String(List.toArray charList)

let parseString (matchstring:string) =
    //Convert string to a list of chars
    Seq.toList matchstring 
    //Convert each char into a parseChar
    |> List.map parseChar
    //Tie them all together with sequence
    |> sequence
    //Pretty Print
    |> mapP charListToStr

//function that trys to match a parser 0 or more times
let rec manyRec parser stream =
    let output = run parser stream
    match output with 
        | Failure err -> ([], stream)
        | Success (result, remainder) -> 
            let (next_output, next_remainder) = manyRec parser remainder
            let acc = result::next_output
            (acc, next_remainder)

let many parser = 
    let inner stream = 
        let tuple = manyRec parser stream
        Success tuple
    Parser inner


//Many1 makes sure that there is atleast one match. 
let many1 parser = 
    let rec inner stream = 
        let first = run parser stream
        match first with
            |Failure err -> Failure err
            |Success (first_result, first_remaining) -> 
                let (next_results, remainder) = manyRec parser first_remaining
                let final_result = first_result::next_results
                Success (final_result, remainder)
    Parser inner

//Optional Parser
let opt parser = 
    //some has the type Parser <Some 'A * string>
    let some = parser |>> Some
    //Therefore, to use the <|> 'Or' operator, we need to promote the None into the minimum context
    //Ergo we use returnP to wrap it in a Parser type
    let none = returnP None
    some <|> none


//Binary Parser Combinator that Discards one of the outputs of the parsers while still consuming a token from the stream

let (.>>) parser1 parser2 =
    let true_parser = parser1 .>>. parser2
    (fun (a,b) -> a) <!> true_parser

let (>>.) parser1 parser2 = 
    (fun(a,b) -> b) <!> (parser1 .>>. parser2)



//Parse Int Example


let parseInt =
    
    let resultToInt (sign, digitList) =
           let integer = String(List.toArray digitList) |> int
           match sign with
           |Some _ -> -1 * integer  
           |None -> integer 

    let inner stream = 
        let parser = (opt (parseChar '-')) .>>. (many1 parseDigit)
        let output = run (resultToInt <!> parser) stream 
        output 
    Parser inner


// End Example

//Test to parse a number, then semi colon

let parse_number_statement = 
    parseInt .>> (opt (parseChar ';'))

let between p1 p2 p3 = 
    p1 >>. p2 .>> p3

let bindP f parser = 
    //f takes a x and returns an Parser(y)
    //parser is a Parser of type x
    let inner stream = 
        let result = run parser stream
        match result with
            |Failure err -> Failure err
            |Success(token, remaining) -> run (f token) remaining
    inner


