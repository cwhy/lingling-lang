module ViewExpr exposing (printExpr, viewVars)

import Dict
import Element exposing (..)
import Element.Font as Font
import List
import Syntax exposing (Expr(..), LetOp(..), SysOp(..))


printExpr : Expr -> String
printExpr expr =
    case expr of
        Note c ->
            String.fromChar c

        Sys Add a b ->
            "ToDo: add [" ++ printExpr a ++ ", " ++ printExpr b ++ "]"

        Notes list ->
            list |> String.fromList

        Empty ->
            ""

        Let a M1 b ->
            "ToDo: Let " ++ a ++ " <- " ++ printExpr b

        Identifier id ->
            id

        Todo list ->
            "ToDo: (" ++ (String.join ", " <| List.map printExpr list) ++ ")"


type alias Var =
    { name : String
    , expr : String
    }


processVar : ( String, Expr ) -> Var
processVar ( str, expr ) =
    { name = str
    , expr = printExpr expr
    }


viewVars : Dict.Dict String Expr -> Element msg
viewVars varDict =
    let
        vars : List Var
        vars =
            List.map processVar (Dict.toList varDict)
    in
    Element.table
        [ Element.spacing 5
        , Element.padding 10
        , Font.color (rgba 0 0.3 0 0.7)
        ]
        { data = vars
        , columns =
            [ { header = Element.text "Var Name"
              , width = px 200
              , view =
                    Element.text << .name
              }
            , { header = Element.text "Expression"
              , width = fill
              , view =
                    Element.text << .expr
              }
            ]
        }
