module Syntax exposing
    ( Expr(..)
    , LetOp(..)
    , SysOp(..)
    , parse
    )

import Parser exposing (..)
import Set



-- EXPRESSIONS


type SysOp
    = Add


type LetOp
    = M1


operator : Parser (Expr -> Expr -> Expr)
operator =
    oneOf
        [ map (\_ -> Sys Add) (symbol "+")
        ]


letOps : Parser LetOp
letOps =
    oneOf
        [ map (\_ -> M1) (symbol "<-")
        ]


type Expr
    = Sys SysOp Expr Expr
    | Let String LetOp Expr
    | Note Char
    | Identifier String
    | Notes (List Char)
    | Todo (List Expr)
    | Empty


parse : String -> Result (List DeadEnd) Expr
parse string =
    run
        (oneOf
            [ commands
            , term
            ]
        )
        string



-- PARSER


getDigit : String -> Parser Expr
getDigit str =
    case String.toList str of
        [] ->
            problem "Unknown pattern"

        char :: [] ->
            case Char.isDigit char of
                True ->
                    succeed <| Note char

                False ->
                    problem "Note should be digits"

        digits ->
            succeed <| Notes digits


digit : Parser Expr
digit =
    andThen getDigit <|
        getChompedString <|
            succeed ()
                |. chompIf Char.isAlphaNum
                |. chompWhile Char.isAlphaNum


identifier : Parser String
identifier =
    oneOf
        [ variable
            { start = \c -> c == '~'
            , inner = \c -> Char.isAlphaNum c || c == '_'
            , reserved = Set.fromList [ "<-" ]
            }
        ]



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
        , andThen (\str -> succeed <| Identifier str) identifier
        ]


commands : Parser Expr
commands =
    oneOf
        [ succeed Let
            |. spaces
            |= identifier
            |. spaces
            |= letOps
            |. spaces
            |= lazy (\_ -> term)
            |. spaces
        ]
