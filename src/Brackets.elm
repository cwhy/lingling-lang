module Brackets exposing
    ( Expr(..)
    , evaluate
    , parse
    )

import Parser exposing (..)



-- EXPRESSIONS


type SysOp
    = AddOp


operator : Parser (Expr -> Expr -> Expr)
operator =
    oneOf
        [ map (\_ -> SysExpr AddOp) (symbol "+")
        ]


type Expr
    = SysExpr SysOp Expr Expr
    | Word Char
    | Words (List Char)
    | Empty


evaluate : Expr -> String
evaluate expr =
    case expr of
        Word c ->
            String.fromChar c

        SysExpr AddOp a b ->
            evaluate a ++ evaluate b

        Words list ->
            list |> String.fromList

        Empty ->
            ""


parse : String -> Result (List DeadEnd) Expr
parse string =
    run term string



-- PARSER


getDigit : String -> Parser Expr
getDigit str =
    case String.toList str of
        [] ->
            problem "Unknown pattern"

        char :: [] ->
            case Char.isDigit char of
                True ->
                    succeed <| Word char

                False ->
                    problem "Note should be digits"

        digits ->
            succeed <| Words digits


digit : Parser Expr
digit =
    andThen getDigit <|
        getChompedString <|
            succeed ()
                |. chompIf Char.isAlphaNum
                |. chompWhile Char.isAlphaNum



--digits : Parser (List Char)
--digits =
--    sequence
--        { start = "{"
--        , separator = ";"
--        , end = "}"
--        , spaces = spaces
--        , item = digit
--        , trailing = Mandatory -- demand a trailing semi-colon
--        }


term : Parser Expr
term =
    oneOf
        [ digit
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> term)
            |. spaces
            |. symbol ")"
        , succeed identity
            |. spaces
            |= operator
            |. spaces
            |= lazy (\_ -> term)
            |. spaces
            |= lazy (\_ -> term)
            |. spaces
        ]
