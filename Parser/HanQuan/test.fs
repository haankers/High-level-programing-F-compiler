open System
open SubParser

let test parser tokens =
    printf "Input: %A\n Output: %A" tokens (parser tokens) 

[<EntryPoint>]
let main args =
    test SubParser.parser Subparser.testTokenList
    0