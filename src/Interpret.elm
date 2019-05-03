module Interpret exposing (interpret)

import Dict
import Syntax exposing (Expr(..), LetOp(..), SysOp(..))
import ViewExpr exposing (printExpr)


type alias RunTime a =
    { a
        | vars : Dict.Dict String Expr
        , ast : Expr
        , output : String
        , error : String
    }


pass1 : RunTime a -> RunTime a
pass1 runtime =
    let
        expr =
            runtime.ast

        pack =
            \expr_ -> { runtime | ast = expr_ }

        pass =
            pack >> pass1

        transform : Expr -> Expr
        transform =
            pass >> .ast
    in
    case expr of
        Sys Add a b ->
            pack <| Todo [ transform a, transform b ]

        Identifier id ->
            case Dict.get id runtime.vars of
                Just expr_ ->
                    pass expr_

                Nothing ->
                    { runtime
                        | error =
                            "Runtime error: Variable \""
                                ++ id
                                ++ "\" is not defined"
                    }

        Let a M1 b ->
            { runtime
                | vars = Dict.insert a (transform b) runtime.vars
                , output =
                    "Successfully assigned "
                        ++ printExpr (transform b)
                        ++ " to "
                        ++ a
                        ++ "."
            }

        a ->
            pack <| a


interpret =
    pass1
