module Main exposing (main)

import Browser
import Debug
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (form)
import Html.Events exposing (onSubmit)
import Interpret exposing (interpret)
import Syntax exposing (Expr(..), parse)
import ViewExpr exposing (printExpr, viewVars)


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
    , vars : Dict.Dict String Expr
    }


init : Model
init =
    { text = "~var <- (+ 4 12)"
    , ast = Empty
    , output = ""
    , error = ""
    , vars = Dict.empty
    }


type Msg
    = SetText String
    | UpdateAst
    | Interpret


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

        Interpret ->
            model |> getAst |> interpret


inputOptions string =
    { onChange = SetText
    , text = string
    , placeholder = Just (Input.placeholder [] (text ""))
    , label = Input.labelAbove [] none
    }


printAst : Model -> String
printAst =
    printExpr << .ast


printErr : Model -> String
printErr =
    Debug.toString << .error


viewLabels : String -> Element msg
viewLabels str =
    el
        [ Font.family
            [ Font.typeface "Charter"
            , Font.typeface "Georgia"
            , Font.serif
            ]
        ]
    <|
        text str


view : Model -> Html.Html Msg
view model =
    form [ onSubmit Interpret ]
        [ Element.layout
            [ width fill, height fill ]
          <|
            row [ width fill, height fill ]
                [ column
                    [ height fill
                    , width <| fillPortion 2
                    , paddingXY 30 30
                    , spacing 10
                    ]
                    [ viewLabels "Commands:"
                    , Input.text
                        [ height (fillPortion 1 |> maximum 100)
                        , Font.family
                            [ Font.typeface "Fira Code"
                            , Font.monospace
                            ]
                        ]
                        (inputOptions model.text)
                    , viewLabels "Runtime Info:"
                    , el
                        [ height <| fillPortion 4
                        ]
                        (viewVars model.vars)
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
                            { onPress = Just Interpret
                            , label = text "Interpret"
                            }
                        ]
                    , viewLabels "AST: "
                    , paragraph
                        [ height <| fillPortion 4
                        , centerX
                        , padding 50
                        ]
                        [ text (printAst model) ]
                    , viewLabels "Output: "
                    , paragraph
                        [ height <| fillPortion 4
                        , centerX
                        , padding 50
                        , Font.color <| rgb255 50 50 200
                        ]
                        [ text model.output ]
                    , viewLabels "Errors: "
                    , paragraph
                        [ height <| fillPortion 4
                        , centerX
                        , padding 50
                        , Font.color <| rgb255 200 50 50
                        ]
                        [ text (printErr model) ]
                    ]
                ]
        ]
