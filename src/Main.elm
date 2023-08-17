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
                , property "grid-template-columns" "repeat(13, 1fr)"
                ]
            ]
            (List.map (div [])
                [ List.map card monoSteps
                , List.map card (oklchSteps { hue = 0 })
                , List.map card (oklchSteps { hue = 30 })
                , List.map card (oklchSteps { hue = 60 })
                , List.map card (oklchSteps { hue = 90 })
                , List.map card (oklchSteps { hue = 120 })
                , List.map card (oklchSteps { hue = 150 })
                , List.map card (oklchSteps { hue = 180 })
                , List.map card (oklchSteps { hue = 210 })
                , List.map card (oklchSteps { hue = 240 })
                , List.map card (oklchSteps { hue = 270 })
                , List.map card (oklchSteps { hue = 300 })
                , List.map card (oklchSteps { hue = 330 })
                ]
            )
        ]
            |> List.map Html.Styled.toUnstyled
    }


oklchSteps : { hue : Float } -> List Oklch
oklchSteps { hue } =
    let
        chromaSteps =
            [ 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.3, 0.3, 0.25, 0.2, 0.2 ]
    in
    List.map2 (\l c -> Oklch l c hue) (List.reverse <| range 0 1 0.1) chromaSteps


monoSteps : List Oklch
monoSteps =
    List.map (\l -> Oklch l 0 0) (range 0 1 0.1)
        |> List.reverse


card : Oklch -> Html msg
card { luminance, chroma, hue } =
    div
        [ css
            [ padding (px 20)
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


range : Float -> Float -> Float -> List Float
range lo hi step =
    rangeHelp lo hi step []


rangeHelp : Float -> Float -> Float -> List Float -> List Float
rangeHelp lo hi step list =
    if lo <= hi then
        rangeHelp lo (hi - step) step (hi :: list)

    else
        list
