open System

let lexNGram nGram cLst =
    /// Function to consume a (zero/one) character if it's in chars.
    /// Returns None otherwise
    let takeSingleLst atLeastOne chars (acc,lst) =
        match lst with
        | hd::tl when List.contains hd chars -> Some(hd::acc,tl)
        | _ -> 
            if atLeastOne
            then None
            else Some(acc,lst)
    
    /// Function to consume (zero/one) or more characters if in chars.
    /// Uses takeIfOneLst
    let takeMoreLst atLeastOne chars gapBuf =
        let charIsValid = (fun x -> List.contains x chars)
        let rec takeWhileInLst' opt =
            match opt with
            | Some(acc,hd::tl) when charIsValid hd ->
                Some(hd::acc,tl) |> takeWhileInLst'
            | _ -> 
                opt
        if atLeastOne
        then takeSingleLst true chars gapBuf
        else (Some gapBuf)
        |> takeWhileInLst'

    let folder state (charsLst,canRep) =
        /// Select which function to use and partially apply char list.
        let consumeCharsFunc =
            match canRep with
            | '=' -> takeSingleLst true
            | '+' -> takeMoreLst true
            | '?' -> takeSingleLst false
            | '*' -> takeMoreLst false
            | _ -> takeSingleLst false // should not reach
            <| charsLst 

            // (if canRep=1 then takeIfOneOrMoreLst else (if canRep=0 then takeIfOneLst else takeIfMaybeLst)) charsLst
        match state with  // or 'Option.map consumeCharsFunc state'
        | Some gapBuf -> consumeCharsFunc gapBuf
        | None -> None

    List.fold folder (Some([],cLst)) nGram
    |> Option.map (fun (token, rest) -> List.rev token, rest) // Reverse token

[<EntryPoint>]
let main argv = 

    // Literals
    let floatLit =
        [['-'],'?'
         ['0'..'9'],'+'
         ['.';','],'='
         ['0'..'9'],'+']
    let integerLit =
        [['-'],'?'
         ['0'..'9'],'+'
         ['+'],'*']
    let stringLit =
        [['\"'],'='
         ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],'*'
         ['\"'],'=']

    // Brackets
    let lSqBra = [['['],'=']
    let rSqBra = [[']'],'=']
    let lRdBra = [['('],'=']
    let rRdBra = [[')'],'=']

    // Arith ops
    let plus = [['+'],'=']
    let minus = [['-'],'=']
    let mult = [['*'],'=']
    let div = [['/'],'=']
    let rem = [['%'],'=']
    let exp = 
        [['*'],'='
         ['*'],'=']

    // Arith Comparison ops
    let equal = [['='],'=']
    let notEqual = 
        [['<'],'='
         ['>'],'=']
    let greaterThan = [['<'],'=']
    let greaterThanOrEq = 
        [['<'],'='
         ['='],'=']
    let lessThan = [['>'],'=']
    let lessThanOrEq = 
        [['>'],'='
         ['='],'=']
    let div = [['/'],'=']

    // Boolean ops
    let boolNot = [['!'],'=']
    let boolAnd = 
        [['&'],'='
         ['&'],'=']
    let boolOr = 
        [['|'],'='
         ['|'],'=']

    // Bitwise ops
    let bitwiseAnd = 
        [['&'],'='
         ['&'],'='
         ['&'],'=']
    let bitwiseOr = 
        [['|'],'='
         ['|'],'='
         ['|'],'=']
    let bitwiseXor = 
        [['^'],'='
         ['^'],'='
         ['^'],'=']
    let bitwiseNot = 
        [['~'],'='
         ['~'],'='
         ['~'],'=']
    let bitwiseLshift = 
        [['<'],'='
         ['<'],'='
         ['<'],'=']
    let bitwiseRshift = 
        [['>'],'='
         ['>'],'='
         ['>'],'=']

    // Active patterns
    let lActivePattern = 
        [['('],'='
         ['|'],'=']
    let rActivePattern = 
        [['|'],'='
         [')'],'=']

    // Functions
    let arrow = 
        [['-'],'='
         ['>'],'=']
    let memberAccess = [['.'],'=']
    let typeSeperator = [[':'],'=']
    let refAssign = 
        [[':'],'='
         ['>'],'=']
    let listSeperator = 
        [[':'],'='
         [':'],'=']

    // Seperator
    let seperator = [[';'],'=']

    // Optional
    let seperator = [['?'],'=']

    // At
    let seperator = [['@'],'=']

    // Range
    let range = 
        [['.'],'='
         ['.'],'=']

    // Comments
    let singleComm = 
        [['/'],'='
         ['/'],'=']
    let lMultiComm = 
        [['('],'='
         ['*'],'=']
    let rMultiComm = 
        [['*'],'='
         [')'],'=']

    let print x = printfn "%A" x

    print (lexNGram integerLit (Seq.toList "here"))
    print (lexNGram integerLit (Seq.toList "1"))
    print (lexNGram integerLit (Seq.toList "-12414"))
    print (lexNGram integerLit (Seq.toList "-12414++++++"))
    print (lexNGram integerLit (Seq.toList "12414"))
    print (lexNGram integerLit (Seq.toList "-12414.12++++++"))
    print (lexNGram integerLit (Seq.toList "-12414.12"))
    print (lexNGram integerLit (Seq.toList "12414.12"))

    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code