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
        | BlkItm of PAST 
        | Blks of PAST * PAST
        | AddExp 
        | Id of string
        | NULL 


    let (|BLOCK|_|) toklst =
        match toklst with
        |FUNLST(Ok(x,((_)::tail))) -> (Some << Ok) (Blks(x, AddExp),tail)
        | _::tail -> (Some << Ok) (Blks(NULL,AddExp),tail)
        |_ -> None

    let (|IDENT|_|) toklst =
        match toklst with
        |Identifier x::tail  -> (Some << Ok) (Id x, tail)
        |_ -> None // Should be an error message? 

    let (|PARAMLST|_|) toklst = 
        match toklst with
        | IDENT (Ok( x, PARAMLST (Ok(y, tail)))) -> (Some << Ok) (ParaLst(x,y), tail)
        | IDENT (Ok( x, tail)) -> (Some << Ok) (ParaLst(x,NULL), tail)
        |_ -> None
    
    let (|CURLY|_|) toklst = 
        match toklst with
        | LCBra::( BLOCK (Ok (e,(RCBra)::tail))) -> (Some << Ok) (BlkItm e, tail)
        | _ -> None


    let (|LAMBDA|_|) toklst =
        match toklst with
        | Fun:: (PARAMLST(Ok(para, (Arrow)::CURLY(Ok(additive,tail))))) -> (Some << Ok) (Lambda(para, additive), tail)
        |_-> None

    let (|NAMED|_|) toklst =
        match toklst with 
        | Let:: IDENT(Ok(name, (PARAMLST(Ok(para,(EqualTo)::CURLY(Ok(additive ,tail)) ))))) -> (Some << Ok) (NamedFun(name,para, additive),tail)
        | Let:: IDENT(Ok(name,(EqualTo)::CURLY(Ok(additive,tail)))) -> (Some << Ok) (NamedFun(name,ParaLst(NULL,NULL), additive),tail)
        | _->None

    let (|FUNDEF|_|) toklst = 
        match toklst with
        |NAMED (Ok(x, tail))-> (Some << Ok) (FunDef x, tail)
        |LAMBDA (Ok(x, tail))-> (Some << Ok) (FunDef x, tail)
        |_ -> None

    let (|FUNLST|_|) toklst = 
        match toklst with
        | FUNDEF(Ok(x,FUNLST(Ok(y,tail)))) ->  Ok (FunLst(x,y), tail)
        | FUNDEF (Ok (x, tail)) -> Ok (FunLst(x,NULL),tail)
        |_->Error(toklst, "cannot parse the function")
        |> Some

// hmmm?
    let SubParsing toklst = 
        (|FUNLST|_|) toklst
    
   

open secondparser
[<EntryPoint>]
let main argv =
    let tokenlist = [Let;Identifier "x"; EqualTo ; LCBra; Fun;Identifier "y"; Arrow ; LCBra; Other "xadad";RCBra;Other "dadasdaf";RCBra]
    printfn "Tokens: %A" tokenlist


    let ast = SubParsing tokenlist
    printfn "ast: %A" ast


    Console.ReadKey() |> ignore
    0 
