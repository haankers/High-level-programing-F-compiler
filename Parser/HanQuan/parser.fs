module rec parser

type Token =
    | NumberLit of string
    | Identifier of string
    | Let
    | Fun
    | EqualTo
    | Arrow
    | LRdBra
    | RRdBra
    | LSqBra
    | RSqBra
    | LCBra
    | RCBra
    | If
    | Then
    | Else
    | GreaterThan
    | GreaterOrEqTo
    | LessThan
    | LessOrEqTo
    | Add
    | Minus
    | Mult
    | Divide
    | SemiColon //originally Endf
    | Other of string
    | Comma //new
    //stub tokens
    | LambdaDefinitionStub
    | CurlyBlockItemStub

type BinaryOperation =
    | AddOp
    | MinusOp
    | MultOp
    | DivideOp
    | GreaterThanOp
    | LessThanOp
    | GreaterOrEqOp
    | LessOrEqOp
    | EqOp

type AST = 
    | Num of string
    | Id of string
    | Lst of AST
    | AppExp of AST * AST        
    | IfExp of AST * AST * AST
    | Pair of AST * AST
    | BinOp of BinaryOperation * AST * AST
    | Null
    | BlockItem //of AST stubbed
    | Lambda //of AST * AST stubbed
    | Bracket of AST //not returned in final ast, used by parser to correct associativity only

//omg this is so ugly help - huge use of stack (non-tail recursive)
let makeLeftAssoc (ast:AST) : AST =
    let rec convert ast' =
        match ast' with
        //fix
        | AppExp (ast1, AppExp(ast2, ast3)) -> AppExp(AppExp(convert ast1, convert ast2), convert ast3)
        | AppExp (ast1, ast2) -> AppExp(convert ast1, convert ast2)

        //propogate
        | Bracket ast -> convert ast
        | Lst ast -> Lst(convert ast)
        | BinOp (op, ast1, ast2) -> BinOp (op, convert ast1, convert ast2)
        | IfExp (ast1, ast2, ast3) -> IfExp (convert ast1, convert ast2, convert ast3)
        | Pair (ast1, ast2) -> Pair (convert ast1, convert ast2)

        //return single arg constructor types
        | ast -> ast
    convert ast;

let (|ITEM|_|) inp =
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | Ok (NumberLit number :: inp') -> Some (Some (Num number), Ok inp')
    | Ok (Identifier identifier :: inp') -> Some (Some (Id identifier), Ok inp')
    | LAMBDA (Some lambda, Ok inp') -> Some (Some lambda, Ok inp')
    | RDBRA_ADD (Some braAddExp, Ok inp') -> Some (Some braAddExp, Ok inp')
    | IFEXP (Some ifExp, Ok inp') -> Some (Some ifExp, Ok inp')
    | LIST (Some list, Ok inp') -> Some (Some list, Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing item", inp'))

let (|TM|_|) (token: Token) (inp: Result<Token list, string * Token List>) =
    match inp with
    | Error (error, inp') -> Some (Error (error, inp'))
    | Ok (tok :: inp') when tok = token -> Some (Ok inp')
    | _ -> None

let (|RDBRA_ADD|_|) inp = 
    let (|LH|_|) = (|TM|_|) LRdBra;
    let (|RH|_|) = (|TM|_|) RRdBra;
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | LH (ADDEXP (Some ast, RH (Ok inp'))) -> Some (Some (Bracket ast), Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing round bracket", inp'))

let (|COMPEXP|_|) inp = 
    let (|GT|_|) = (|TM|_|) GreaterThan
    let (|LT|_|) = (|TM|_|) LessThan
    let (|GTE|_|) = (|TM|_|) GreaterOrEqTo
    let (|LTE|_|) = (|TM|_|) LessOrEqTo
    let (|EQ|_|) = (|TM|_|) EqualTo
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | ADDEXP (Some ast1, GT (COMPEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (GreaterThanOp, ast1, ast2)), Ok inp')
    | ADDEXP (Some ast1, LT (COMPEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (LessThanOp, ast1, ast2)), Ok inp')
    | ADDEXP (Some ast1, GTE (COMPEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (GreaterOrEqOp, ast1, ast2)), Ok inp')
    | ADDEXP (Some ast1, LTE (COMPEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (LessOrEqTo, ast1, ast2)), Ok inp')
    | ADDEXP (Some ast1, EQ (COMPEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (EqOp, ast1, ast2)), Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing compare exp", inp'))

let (|ADDEXP|_|) inp =
    let (|ADD|_|) = (|TM|_|) Add
    let (|MINUS|_|) = (|TM|_|) Minus
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | MULTEXP (Some ast1, ADD (ADDEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (AddOp, ast1, ast2)), Ok inp')
    | MULTEXP (Some ast1, MINUS (ADDEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (MinusOp, ast1, ast2)), Ok inp')
    | MULTEXP (Some ast, Ok inp') -> Some (Some ast, Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing add exp", inp'))

let (|MULTEXP|_|) inp =
    let (|MULT|_|) = (|TM|_|) Mult
    let (|DIV|_|) = (|TM|_|) Divide
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | APPEXP (Some ast1, MULT (MULTEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (MultOp, ast1, ast2)), Ok inp')
    | APPEXP (Some ast1, DIV (MULTEXP (Some ast2, Ok inp'))) -> Some (Some (BinOp (DivideOp, ast1, ast2)), Ok inp')
    | APPEXP (Some ast, Ok inp') -> Some (Some ast, Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing mult exp", inp'))

let (|APPEXP|_|) inp =
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | ITEM (Some ast1, APPEXP (Some ast2, Ok inp')) -> Some (Some (AppExp (ast1, ast2)), Ok inp')
    | ITEM (Some ast, Ok inp') -> Some (Some ast, Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing mult exp", inp'))

let (|LIST|_|) inp =
    let (|LH|_|) = (|TM|_|) LSqBra;
    let (|RH|_|) = (|TM|_|) RSqBra;
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | LH (LIST_ITEMS (Some ast, RH (Ok inp'))) -> Some (Some (Lst ast), Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing list", inp'))

let (|LIST_ITEMS|_|) inp =
    let (|SEP|_|) = (|TM|_|) SemiColon
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | ADDEXP (Some ast1, SEP (LIST_ITEMS (Some ast2, Ok inp'))) -> Some (Some (Pair (ast1, ast2)), Ok inp')
    | ADDEXP (Some ast, Ok inp') -> Some (Some (Pair (ast, Null)), Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing list items", inp'))

let (|IFEXP|_|) inp =
    let (|IF|_|) = (|TM|_|) If
    let (|THEN|_|) = (|TM|_|) Then
    let (|ELSE|_|) = (|TM|_|) Else
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | IF (COMPEXP (Some compExp, THEN (CURLY_BLOCK_ITEM (Some blockItem1, ELSE (CURLY_BLOCK_ITEM (Some blockItem2, Ok inp')))))) ->
        Some (Some (IfExp (compExp, blockItem1, blockItem2)), Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing if exp", inp'))

//stubs
let (|LAMBDA|_|) inp =
    let (|LAMB|_|) = (|TM|_|) LambdaDefinitionStub
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | LAMB (Ok inp') -> Some (Some Lambda, Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing lambda exp", inp'))

let (|CURLY_BLOCK_ITEM|_|) inp =
    let (|BLOCK|_|) = (|TM|_|) CurlyBlockItemStub
    match inp with
    | Error (error, inp') -> Some (None, Error (error, inp'))
    | BLOCK (Ok inp') -> Some (Some BlockItem, Ok inp')
    | Ok inp' -> Some (None, Error ("Error parsing lambda exp", inp'))

let impossible() = failwithf "What? Impossible"

let parser (inp: Token list) : Result<AST, int*string> =
    let getErrorIndex inp inp' = List.length inp - List.length inp'
    match Ok inp with
    | ADDEXP (Some ast, Ok []) -> Ok (makeLeftAssoc ast)
    | ADDEXP (_, Error (error, inp')) -> Error (getErrorIndex inp inp', error)
    | ADDEXP (_, Ok inp') -> Error (getErrorIndex inp inp', "matched nothing")
    | _ -> impossible()

let testTokenList = [
    Identifier("f")
    LRdBra
    NumberLit("1")
    Add
    NumberLit("2")
    RRdBra
    Identifier("h")
    LRdBra
    Identifier("x")
    LRdBra
    Identifier("y")
    Identifier("z")
    RRdBra
    RRdBra
    LSqBra
    NumberLit("10")
    SemiColon
    NumberLit("20")
    SemiColon
    NumberLit("30")
    SemiColon
    NumberLit("40")
    RSqBra
]