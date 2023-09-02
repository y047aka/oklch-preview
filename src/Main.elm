port module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled
import Oklch
import VividPicker


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : { monoSteps : List Okhsl, colorSteps : List (List Okhsl) } -> Cmd msg


port messageReceiver : ({ monoSteps : List (List Float), colorSteps : List (List (List Float)) } -> msg) -> Sub msg



-- INIT


type alias Model =
    { monoSteps : List Color
    , colorSteps : List (List Color)
    }


type alias Okhsl =
    { h : Float, s : Float, l : Float }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { monoSteps = [], colorSteps = [] }
    , sendMessage
        { monoSteps =
            reverseRange 0 1 0.005
                |> List.map (\l -> Okhsl 0 0 l)
        , colorSteps =
            List.range 0 359
                |> List.map (\hue -> List.map (\l -> Okhsl (toFloat hue) 1 l) (reverseRange 0 1 0.005))
        }
    )



-- UPDATE


type Msg
    = Recv { monoSteps : List (List Float), colorSteps : List (List (List Float)) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Recv d ->
            let
                rgbFromList steps =
                    case steps of
                        [ r, g, b ] ->
                            rgb (Basics.round r) (Basics.round g) (Basics.round b)

                        _ ->
                            rgb 0 0 0
            in
            ( { model
                | monoSteps = List.map rgbFromList d.monoSteps
                , colorSteps = List.map (\colorSteps -> List.map rgbFromList colorSteps) d.colorSteps
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Oklch Preview"
    , body =
        [ VividPicker.oklch
        , VividPicker.okhsl
        , Oklch.view
        ]
            |> List.map Html.Styled.toUnstyled
    }



-- HELPERS


range : Float -> Float -> Float -> List Float
range lo hi step =
    rangeHelp lo hi step []


rangeHelp : Float -> Float -> Float -> List Float -> List Float
rangeHelp lo hi step list =
    if lo <= hi then
        rangeHelp lo (hi - step) step (hi :: list)

    else
        list


reverseRange : Float -> Float -> Float -> List Float
reverseRange lo hi step =
    range lo hi step
        |> List.reverse
