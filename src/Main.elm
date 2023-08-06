module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Oklch Preview"
    , body =
        [ div
            [ css
                [ property "display" "grid"
                , property "grid-template-columns" "repeat(11, 1fr)"
                , property "gap" "10px"
                ]
            ]
            (List.map (\l -> Oklch l 0 0 |> tile) (range 0 1))
        ]
            |> List.map Html.Styled.toUnstyled
    }


tile : Oklch -> Html msg
tile { luminance, chroma, hue } =
    div
        [ css
            [ padding (px 20)
            , borderRadius (px 10)
            , backgroundColor (oklch luminance chroma hue)
            , fontFamily sansSerif
            ]
        ]
        [ text "Oklch" ]



-- OKLCH


type alias Oklch =
    { luminance : Float, chroma : Float, hue : Float }


oklch : Float -> Float -> Float -> Color
oklch luminance chroma hue =
    let
        valuesList =
            [ numericalPercentageToString luminance
            , String.fromFloat chroma
            , String.fromFloat hue
            ]
    in
    Css.rgb 0 0 0
        |> (\color -> { color | value = "oklch(" ++ String.join " " valuesList ++ ")" })


numericalPercentageToString : Float -> String
numericalPercentageToString value =
    String.fromFloat (value * 100) ++ "%"



-- HELPERS


range : Float -> Float -> List Float
range lo hi =
    rangeHelp lo hi 0.1 []


rangeHelp : Float -> Float -> Float -> List Float -> List Float
rangeHelp lo hi step list =
    if lo <= hi then
        rangeHelp lo (hi - step) step (hi :: list)

    else
        list
