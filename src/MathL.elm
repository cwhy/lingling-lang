module MathL exposing
    ( Expr
    , evaluate
    , parse
    )

import Parser exposing (..)



-- EXPRESSIONS


type Expr
    = Integer Int
    | Floating Float
    | Add Expr Expr
    | Mul Expr Expr
    | Word Char


evaluate : Expr -> Float
evaluate expr =
    case expr of
        Integer n ->
            toFloat n

        Floating n ->
            n

        Word _ ->
            42

        Add a b ->
            evaluate a + evaluate b

        Mul a b ->
            evaluate a * evaluate b


parse : String -> Result (List DeadEnd) Expr
parse string =
    run expression string



-- PARSER


{-| We want to handle integers, hexadecimal numbers, and floats. Octal numbers
like `0o17` and binary numbers like `0b01101100` are not allowed.
-}
digits : Parser Expr
digits =
    number
        { int = Just Integer
        , hex = Just Integer
        , octal = Nothing
        , binary = Nothing
        , float = Just Floating
        }


getDigit : String -> Parser Expr
getDigit str =
    let
        result =
            String.uncons str
    in
    case result of
        Just ( char, "" ) ->
            case Char.isUpper char of
                True ->
                    succeed <| Word char

                False ->
                    problem "Note should be upper case"

        Nothing ->
            problem "Unknown pattern"

        _ ->
            problem "Note should be only 1 char"


digit : Parser Expr
digit =
    andThen getDigit <|
        getChompedString <|
            succeed ()
                |. chompIf Char.isAlphaNum
                |. chompWhile Char.isAlphaNum


{-| A term is a standalone chunk of math, like `4` or `(3 + 4)`. We use it as
a building block in larger expressions.
-}
term : Parser Expr
term =
    oneOf
        [ digits

        -- , digit
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        ]


{-| Every expression starts with a term. After that, it may be done, or there
may be a `+` or `*` sign and more math.
-}
expression : Parser Expr
expression =
    term
        |> andThen (expressionHelp [])


{-| Once you have parsed a term, you can start looking for `+` and \`\* operators.
I am tracking everything as a list, that way I can be sure to follow the order
of operations (PEMDAS) when building the final expression.
In one case, I need an operator and another term. If that happens I keep
looking for more. In the other case, I am done parsing, and I finalize the
expression.
-}
expressionHelp : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelp (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


type Operator
    = AddOp
    | MulOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AddOp) (symbol "+")
        , map (\_ -> MulOp) (symbol "*")
        ]


{-| We only have `+` and `*` in this parser. If we see a `MulOp` we can
immediately group those two expressions. If we see an `AddOp` we wait to group
until all the multiplies have been taken care of.
This code is kind of tricky, but it is a baseline for what you would need if
you wanted to add `/`, `-`, `==`, `&&`, etc. which bring in more complex
associativity and precedence rules.
-}
finalize : List ( Expr, Operator ) -> Expr -> Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, MulOp ) :: otherRevOps ->
            finalize otherRevOps (Mul expr finalExpr)

        ( expr, AddOp ) :: otherRevOps ->
            Add (finalize otherRevOps expr) finalExpr
