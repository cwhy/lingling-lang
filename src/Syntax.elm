module Syntax exposing (Tet12Expr(..))

import Dict
import List
import Parser exposing (..)
import String exposing (replace)


type Operator
    = GlobalLetOp

Dict.Dict String (Tet12Expr -> Tet12Expr)


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> GlobalLetOp) (symbol "let")
        ]


type Tet12Expr
    = Note String
    | Notes (List String)
    | ExprList (List Tet12Expr)
    | GlobalLet Tet12Expr Tet12Expr


parse : String -> Result (List DeadEnd) Tet12Expr
parse string =
    run expression string

node : Parser Tet12Expr
node =
    case

term : Parser Tet12Expr
term =
    oneOf
        [  node
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        ]


expression : Parser Tet12Expr
expression =
    term
        |> andThen (expressionHelp [])


expressionHelp : List ( Tet12Expr, Operator ) -> Tet12Expr -> Parser Tet12Expr
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



finalize : List ( Tet12Expr, Operator ) -> Tet12Expr -> Tet12Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, GlobalLetOp ) :: otherRevOps ->
            GlobalLet (finalize otherRevOps expr) finalExpr
