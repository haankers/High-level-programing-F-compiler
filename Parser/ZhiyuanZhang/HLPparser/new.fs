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
        | PARTTWOTEST

    type PAST =
        | FunLst of PAST * PAST
        | FunDef of PAST
        | NamedFun of PAST * PAST * PAST
        | Lambda of PAST * PAST
        | ParaLst of PAST * PAST
        | BlkItm of PAST 
        | Blks of PAST * PAST
        | AddExp 
        | Id of string
        | NULL 

    let (|TM|_|) lit (inp:Result<Token list, string>) =
        match inp with
        | Ok (ch :: inp') when lit = ch -> Some(Ok inp')
        | _ -> None

    let (|BLOCK|_|) (inp:Result<Token list, string>)=
        match inp with
        // correct cases
        |FUNLST(Some funclst, TM PARTTWOTEST ( Ok tail) ) -> Some(Blks(funclst, AddExp)), Ok tail
        |TM PARTTWOTEST ( Ok tail) -> Some (Blks(NULL, AddExp)), Ok tail// should be expressions, implemented by Han
        //error case
        |FUNLST(Some _,  Ok _) -> None, Error ("function need to have a result, define a driver for the subfunctions")
        //let errors pass from bottom
        |FUNLST(None, Error msg ) -> None, Error msg
        //default
        |_->None,Error("Undefined error at block")
        |> Some
      

    let (|IDENT|_|) (inp:Result<Token list, string>) =
        match inp with
        |Ok(Identifier x::tail) -> Some (Id x) , Ok tail
        |_ -> None, Error("undefined error at identifier")
        |> Some

    let (|PARAMLST|_|) (inp:Result<Token list, string>) = 
        match inp with
        // correct cases
        |IDENT (Some id1, PARAMLST (Some paras, Ok tail)) -> Some (ParaLst(id1,paras)), Ok tail 
        |IDENT (Some id1, Ok tail) -> Some (ParaLst(id1,NULL)), Ok tail 
        //let errors pass from bottom
        |IDENT (Some _, PARAMLST (None, Error msg)) -> None, Error msg
        |IDENT (None, Error msg) -> None, Error msg
        //default
        |_ -> None, Error ("undefined error at parameters")
        |> Some

    let (|CURLY|_|) (inp:Result<Token list, string>) = 
        let (|LH|_|) = (|TM|_|) LCBra
        let (|RH|_|) = (|TM|_|) RCBra
        match inp with
        // correct case
        | LH (BLOCK (Some e, RH (Ok inp'))) -> Some (BlkItm e), Ok inp'
        // error cases
        | BLOCK (Some _, RH (Ok _)) -> None, Error(" \"{\"  missing")
        | LH(BLOCK (Some _, Ok _)) -> None, Error(" \"}\"  missing")
        //let errors pass from bottom
        | LH (BLOCK (None, Error msg)) -> None, Error msg
        //default
        | _ ->  None, Error ("undefined error at scope of function body")
        |> Some

    let (|LAMBDA|_|) (inp: Result<Token list, string>) =
        let (|LAM|_|) = (|TM|_|) Fun
        let (|ARR|_|) = (|TM|_|) Arrow
        match inp with
        // correct case
        |LAM (PARAMLST (Some paras, ARR ( CURLY(Some additive, Ok tail))))-> Some (Lambda(paras,additive)),Ok tail
        // error cases
        |PARAMLST (Some _, ARR ( CURLY(Some _, Ok _)))-> None, Error("need \"fun\" when defining lambda function")
        |LAM (PARAMLST (Some _,  CURLY(Some _, Ok _)))-> None, Error("need \"->\" between parameters and function body")
        //let errors pass from bottom
        |LAM (PARAMLST (Some _, ARR ( CURLY(None, Error msg))))-> None, Error msg
        |LAM (PARAMLST (None, Error msg))-> None, Error msg
        //default
        | _ -> None, Error("undefined error when parsing lambda functions")
        |> Some



    let (|NAMED|_|) (inp: Result<Token list, string>) =
        let (|LET|_|) = (|TM|_|) Let
        let (|EQL|_|) = (|TM|_|) EqualTo
        match inp with
        // correct cases
        |LET (IDENT (Some name,PARAMLST (Some paras, EQL ( CURLY(Some additive, Ok tail)))) )-> Some (NamedFun(name,paras,additive)),Ok tail
        |LET (IDENT (Some name, EQL(CURLY (Some additive, Ok tail))))-> Some (NamedFun(name,ParaLst(NULL, NULL),additive)),Ok tail
        // error cases
        |LET (IDENT (Some _,PARAMLST (Some _,  ( CURLY(Some _, Ok _)))) )-> None, Error("need \"=\" between parameters and function body")
        |LET (IDENT (Some _,PARAMLST (Some _,  EQL( CURLY(Some _, Ok _)))) )-> None, Error("need \"let\" when defining a function")
        | (IDENT (Some _, EQL(CURLY (Some _, Ok _))))-> None, Error("need \"let\" when defining a function")
        |LET (IDENT (Some _, (CURLY (Some _, Ok _))))-> None, Error("need \"=\" between parameters and function body")
        //let errors pass from bottom
        |LET (IDENT (Some _,PARAMLST (Some _, EQL ( CURLY(None, Error msg)))) )-> None, Error msg
        |LET (IDENT (Some _,PARAMLST (None, Error msg)) )-> None, Error msg
        |LET (IDENT (None, Error msg) )-> None, Error msg
        |LET (IDENT (Some _, EQL(CURLY (None,Error msg))))-> None,Error msg
        |LET (IDENT (None,Error msg))-> None,Error msg
        //default
        | _ -> None, Error("undefined error when parsing user-defined functions")
        |> Some

    let (|FUNDEF|_|) (inp: Result<Token list, string>) = 
        match inp with
        // correct cases
        |NAMED(Some func, Ok tail) -> Some (FunDef func), Ok tail
        |LAMBDA(Some func, Ok tail) -> Some (FunDef func), Ok tail
        //let errors pass from bottom
        |NAMED(None, Error msg) -> None, Error msg
        |LAMBDA(None, Error msg) -> None, Error msg
        //default
        |_ ->None, Error ("undefined error when parsing function definition")
        |>Some

    let (|FUNLST|_|) (inp: Result<Token list, string>) =
        match inp with
        // correct cases
        |FUNDEF(Some func,  FUNLST(Some funclst, Ok tail)) -> Some (FunLst(func,funclst)), Ok tail
        |FUNDEF(Some func, Ok tail) -> Some (FunLst(func,NULL)), Ok tail
        //let errors pass from bottom
        |FUNDEF(None, Error msg) -> None, Error msg
        //default
        |_ -> None, Error("undefined error when parsing function lst")
        |> Some 


    let SubParsing inp = 
         (|FUNLST|_|) (Ok inp)


open secondparser
[<EntryPoint>]
let main argv =
    let tokenlist = [Let;Identifier "x"; EqualTo ; LCBra; Fun;Identifier "y"; Arrow ; LCBra; PARTTWOTEST ;RCBra;PARTTWOTEST;RCBra]
    printfn "Tokens: %A" tokenlist
    let ast = SubParsing tokenlist
    printfn "ast: %A" ast
    
    //printfn "Id: %A" ((|IDENT|_|) (Ok [Identifier "x"]))
    //printfn "Block: %A" ((|BLOCK|_|) (Ok [PARTTWOTEST]))
    //printfn "Parameters: %A" ((|PARAMLST|_|) (Ok [Identifier "x"]))
    //printfn "Parameters: %A" ((|PARAMLST|_|) (Ok [Identifier "x"; Identifier "y"]))
    //printfn "curly: %A" ((|CURLY|_|) (Ok [LCBra;PARTTWOTEST;RCBra]))
    //printfn "curly: %A" ((|CURLY|_|) (Ok [PARTTWOTEST;RCBra]))
    //printfn "curly: %A" ((|CURLY|_|) (Ok [LCBra;PARTTWOTEST]))
    //printfn "Lambda function: %A" ((|LAMBDA|_|) (Ok [ Fun;Identifier "y"; Arrow ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "Lambda function: %A" ((|LAMBDA|_|) (Ok [ Identifier "y"; Arrow ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "Lambda function: %A" ((|LAMBDA|_|) (Ok [ Fun;Identifier "y"; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "Named function: %A" ((|NAMED|_|) (Ok [ Let;Identifier "y"; EqualTo ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "Named function: %A" ((|NAMED|_|) (Ok [ Let;Identifier "y" ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "Named function: %A" ((|NAMED|_|) (Ok [ Identifier "y"; EqualTo ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "function definition: %A" ((|FUNDEF|_|) (Ok [ Let;Identifier "y"; EqualTo ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "function definition: %A" ((|FUNDEF|_|) (Ok [ Let;Identifier "y" ; LCBra; PARTTWOTEST ;RCBra]))
    //printfn "function definition: %A" ((|FUNDEF|_|) (Ok [ Identifier "y"; EqualTo ; LCBra; PARTTWOTEST ;RCBra]))



    Console.ReadKey() |> ignore
    0 
