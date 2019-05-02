module Main exposing (main)

import Brackets exposing (Expr(..), evaluate, parse)
import Browser
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { text : String
    , ast : Expr
    , output : String
    , error : String
    }


init : Model
init =
    { text = "( + 12 4) 12"
    , ast = Empty
    , output = ""
    , error = ""
    }


type Msg
    = SetText String
    | UpdateAst
    | Evaluate


getAst : Model -> Model
getAst model =
    case parse model.text of
        Err err ->
            { model | error = Debug.toString err, ast = Empty }

        Ok expr ->
            { model | ast = expr, error = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetText string ->
            { model | text = string }

        UpdateAst ->
            getAst model

        Evaluate ->
            { model | output = evaluate model.ast }


options string =
    { onChange = SetText
    , text = string
    , placeholder = Just (Input.placeholder [] (text ""))
    , label = Input.labelAbove [] none
    , spellcheck = False
    }


printAst : Model -> String
printAst =
    Debug.toString << .ast


printErr : Model -> String
printErr =
    Debug.toString << .error


view model =
    Element.layout [ width fill, height fill ] <|
        row [ width fill, height fill ]
            [ column
                [ height fill
                , width <| fillPortion 2
                , paddingXY 10 20
                ]
                [ Input.multiline
                    [ height fill
                    , Font.family
                        [ Font.typeface "Fira Code"
                        , Font.monospace
                        ]
                    ]
                    (options model.text)
                ]
            , column
                [ height fill
                , width <| fillPortion 2
                ]
                [ row
                    [ paddingXY 20 20
                    , height <| fillPortion 1
                    , width fill
                    , spacing 20
                    ]
                    [ Input.button
                        [ paddingXY 20 20
                        , centerX
                        , centerY
                        , Background.color (rgba 0 0 0 0.1)
                        ]
                        { onPress = Just UpdateAst
                        , label = text "Get AST"
                        }
                    , Input.button
                        [ paddingXY 20 20
                        , centerX
                        , centerY
                        , Background.color (rgba 0 0 0 0.1)
                        ]
                        { onPress = Just Evaluate
                        , label = text "Evaluate"
                        }
                    ]
                , paragraph
                    [ height <| fillPortion 4
                    , centerX
                    , padding 50
                    ]
                    [ text (printAst model) ]
                , paragraph
                    [ height <| fillPortion 4
                    , centerX
                    , padding 50
                    , Font.color <| rgb255 50 50 200
                    ]
                    [ text model.output ]
                , paragraph
                    [ height <| fillPortion 4
                    , centerX
                    , padding 50
                    , Font.color <| rgb255 200 50 50
                    ]
                    [ text (printErr model) ]
                ]
            ]
