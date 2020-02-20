open System

module rec secondparser =

    type Token = 
        | NumberLit of string
        | Identifier of string
        | Let 
        | Fun 
        | EqualTo // =
        | Arrow // ->
        | LRdBra // (
        | RRdBra // )
        | LSqBra // [
        | RSqBra // ]
        | LCBra //  {
        | RCBra  // }
        | If   
        | Then
        | Else
        | GreaterThan // >
        | GreaterOrEqTo // >=
        | LessThan    // <
        | LessOrEqTo  // <=
        | Add          // + 
        | Minus         // -
        | Mult          // *
        | Divide        // /
        | Endf          // ;
        | Other of string

    type PAST =
        | FunLst of PAST * PAST
        | FunDef of PAST
        | NamedFun of PAST * PAST * PAST
        | Lambda of PAST * PAST
        | ParaLst of PAST * PAST
        | BlkItm of PAST * PAST
        | AddExp 
        | Id of String


    let (|BLOCK|_|) toklst =
        match toklst with
        | _ -> (Some << Ok) (AddExp)

    let (|IDENT|_|) toklst =
        match toklst with
        |Identifier x -> (Some << Ok) (Id x)
        |_ -> None // Should be an error message? 

    
   

open secondparser
[<EntryPoint>]
let main argv =
    let tokenlist = [Let;Identifier "x"; EqualTo ; LCBra; NumberLit "10"; Endf;RCBra]
 
    printfn "Tokens: %A" tokenlist

    Console.ReadKey() |> ignore
    0 
