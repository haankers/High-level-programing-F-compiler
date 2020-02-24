open System
open System.IO
open FsCheck
open Expecto

type Token = As | Let | Function | Func | Match | With | Type | When | If | Elif | Then | Else | True | False | Null
            | Space | Tab | NewLine
            | Variable of char list
            | FloatLit of char list | IntegerLit of char list | StringLit of char list
            | LSqBra | RSqBra | LRdBra | RRdBra
            | Plus | Minus | Mult | Div | Rem | Exp
            | Equal | NotEqual | GreaterThan | GreaterThanOrEq | LessThan | LessThanOrEq
            | BoolNot | BoolAnd | BoolOr
            | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot | BitwiseLshift | BitwiseRshift
            | LActivePattern | RActivePattern
            | Pipe | Arrow | MemberAccess | TypeSeperator | RefAssign | ListSeperator
            | Seperator | Optional | At | Range
            | SingleComm | LMultiComm | RMultiComm
// key words
let _as =
    [['a'],'='
     ['s'],'='],As
let _let =
    [['l'],'='
     ['e'],'='
     ['t'],'='],Let 
let _function =
    [['f'],'='
     ['u'],'='
     ['n'],'='
     ['c'],'='
     ['t'],'='
     ['i'],'='
     ['o'],'='
     ['n'],'='],Function 
let _func =
    [['f'],'='
     ['u'],'='
     ['n'],'='
     ['c'],'='],Func 
let _match =
    [['m'],'='
     ['a'],'='
     ['t'],'='
     ['c'],'='
     ['h'],'='],Match 
let _with =
    [['w'],'='
     ['i'],'='
     ['t'],'='
     ['h'],'='],With 
let _type =
    [['t'],'='
     ['y'],'='
     ['p'],'='
     ['e'],'='],Type 
let _when =
    [['w'],'='
     ['h'],'='
     ['e'],'='
     ['n'],'='],When 
let _if =
    [['i'],'='
     ['f'],'='],If 
let _elif =
    [['e'],'='
     ['l'],'='
     ['i'],'='
     ['f'],'='],Elif 
let _then =
    [['t'],'='
     ['h'],'='
     ['e'],'='
     ['n'],'='],Then 
let _else =
    [['e'],'='
     ['l'],'='
     ['s'],'='
     ['e'],'='],Else 
let _true =
    [['t'],'='
     ['r'],'='
     ['u'],'='
     ['e'],'='],True 
let _false =
    [['f'],'='
     ['a'],'='
     ['l'],'='
     ['s'],'='
     ['e'],'='],False 
let _null =
    [['n'],'='
     ['u'],'='
     ['l'],'='
     ['l'],'='],Null 

// Spaces
let _space = [[' '],'+'],Space
let _tab = [['\011'],'='],Tab
let _newLine = [['\n'],'+'],NewLine

let _variable =
    [['a'..'z']@['A'..'Z']@['_'],'='
     ['a'..'z']@['A'..'Z']@['_']@['0'..'9'],'*'],Variable ['-']

// Literals
let _floatLit =
    [['0'..'9'],'+'
     ['.';','],'='
     ['0'..'9'],'+'],FloatLit ['-']
let _integerLit =
    [['0'..'9'],'+'
     ['+'],'*'],IntegerLit ['-']
let _stringLit =
    [['\"'],'='
     ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],'*'
     ['\"'],'='],StringLit ['-']

// Brackets
let _lSqBra = [['['],'='],LSqBra
let _rSqBra = [[']'],'='],RSqBra
let _lRdBra = [['('],'='],LRdBra
let _rRdBra = [[')'],'='],RRdBra

// Arith ops
let _plus = [['+'],'='],Plus
let _minus = [['-'],'='],Minus
let _mult = [['*'],'='],Mult
let _div = [['/'],'='],Div
let _rem = [['%'],'='],Rem
let _exp = 
    [['*'],'='
     ['*'],'='],Exp

// Arith Comparison ops
let _equal = [['='],'='],Equal
let _notEqual = 
    [['<'],'='
     ['>'],'='],NotEqual
let _greaterThan = [['<'],'='],GreaterThan
let _greaterThanOrEq = 
    [['<'],'='
     ['='],'='],GreaterThanOrEq
let _lessThan = [['>'],'='],LessThan
let _lessThanOrEq = 
    [['>'],'='
     ['='],'='],LessThanOrEq

// Boolean ops
let _boolNot = [['!'],'='],BoolNot
let _boolAnd = 
    [['&'],'='
     ['&'],'='],BoolAnd
let _boolOr = 
    [['|'],'='
     ['|'],'='],BoolOr

// Bitwise ops
let _bitwiseAnd = 
    [['&'],'='
     ['&'],'='
     ['&'],'='],BitwiseAnd
let _bitwiseOr = 
    [['|'],'='
     ['|'],'='
     ['|'],'='],BitwiseOr
let _bitwiseXor = 
    [['^'],'='
     ['^'],'='
     ['^'],'='],BitwiseXor
let _bitwiseNot = 
    [['~'],'='
     ['~'],'='
     ['~'],'='],BitwiseNot
let _bitwiseLshift = 
    [['<'],'='
     ['<'],'='
     ['<'],'='],BitwiseLshift
let _bitwiseRshift = 
    [['>'],'='
     ['>'],'='
     ['>'],'='],BitwiseRshift

// Active patterns
let _lActivePattern = 
    [['('],'='
     ['|'],'='],LActivePattern
let _rActivePattern = 
    [['|'],'='
     [')'],'='],RActivePattern

// Functions
let _pipe = [['|'],'='],Pipe
let _arrow = 
    [['-'],'='
     ['>'],'='],Arrow
let _memberAccess = [['.'],'='],MemberAccess
let _typeSeperator = [[':'],'='],TypeSeperator
let _refAssign = 
    [[':'],'='
     ['>'],'='],RefAssign
let _listSeperator = 
    [[':'],'='
     [':'],'='],ListSeperator

// Seperator
let _seperator = [[';'],'='],Seperator

// Optional
let _optional = [['?'],'='],Optional

// At
let _at = [['@'],'='],At

// Range
let _range = 
    [['.'],'='
     ['.'],'='],Range

// Comments
let _singleComm = 
    [['/'],'='
     ['/'],'='],SingleComm
let _lMultiComm = 
    [['('],'='
     ['*'],'='],LMultiComm
let _rMultiComm = 
    [['*'],'='
     [')'],'='],RMultiComm

let lexNGram cLst (nGram,nGramType) =
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
    |> Option.map (fun (token, rest) -> 
    match nGramType with 
    // | str as tokenType -> tokenType of str
    // | tokenType,str -> str,List.rev token, rest
    | Variable(str) -> Variable (List.rev token), rest
    | StringLit(str) -> StringLit (List.rev token), rest
    | IntegerLit(str) -> IntegerLit (List.rev token), rest
    | FloatLit(str) -> FloatLit (List.rev token), rest
    | tokenType -> tokenType, rest)


// type tokens = stringLit of string
// start of the lexer
let nGrams = [_as; _let; _function; _func; _match; _with; _type; _when; _if; _elif; _then; _else; _true; _false; _null;
            _space;_tab;
            _floatLit; _integerLit; _stringLit;
            _bitwiseAnd; _bitwiseOr; _bitwiseXor; _bitwiseNot; _bitwiseLshift; _bitwiseRshift;
            _boolNot; _boolAnd; _boolOr;
            _singleComm; _lMultiComm; _rMultiComm;
            _exp; _plus; _minus; _mult; _div; _rem;
            _notEqual; _greaterThanOrEq; _greaterThan; _lessThanOrEq; _lessThan; _equal;
            _lActivePattern; _rActivePattern;
            _arrow; _memberAccess; _typeSeperator; _refAssign; _listSeperator;
            _seperator; _optional; _at; _range;
            _lSqBra; _rSqBra; _lRdBra; _rRdBra;
            _variable;]

let lexer cLst =
    let mapper lst = List.map (lexNGram lst) nGrams |>
                            (List.find Option.isSome) |>
                            (Option.get)
    let rec lexerRec lst = 
        match lst with 
        |   [] -> []
        |   lst' -> 
                let tokenType, remaining = mapper lst'
                if tokenType <> Space then
                    [tokenType]@(lexerRec remaining)
                else
                    (lexerRec remaining)
    lexerRec cLst

let fileToArray fileName = Seq.toList (File.ReadAllText fileName)
    
[<Tests>]
let tests =
  testList "Single Elements" [
    test "one test" {
      Expect.equal (lexer (Seq.toList "let =")) [Let;Equal] "equal"
    }

    test "one test2" {
      Expect.equal (lexer (fileToArray "./Lexer/Test1.fs")) [Let;Equal] "equal"
    }
  ]
  |> testLabel "one Element"

[<EntryPoint>]
let main argv = 
    runTestsInAssembly defaultConfig [||] |> ignore
    
    let print x = printfn "%A" x
    print (Seq.toList "=         =")
    print (lexer (Seq.toList "=         ="))

    print "finished"
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code