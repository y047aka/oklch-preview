module VividPicker exposing (hsl, oklch)

import Css exposing (..)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Oklch


hsl : Html msg
hsl =
    let
        monoSteps =
            reverseRange 0 1 0.005
                |> List.map (\l -> Css.hsl 0 0 l)

        hslSteps { hue } =
            reverseRange 0 1 0.005
                |> List.map (\l -> Css.hsl hue 1 l)
    in
    vividPicker
        { monoSteps = monoSteps
        , toColorSteps = hslSteps
        }


oklch : Html msg
oklch =
    let
        monoSteps =
            reverseRange 0 1 0.005
                |> List.map (\l -> Oklch.oklch l 0 0)

        toColorSteps { hue } =
            reverseRange 0 1 0.005
                |> List.map (\l -> Oklch.oklch l 0.2 hue)
    in
    vividPicker
        { monoSteps = monoSteps
        , toColorSteps = toColorSteps
        }


vividPicker : { monoSteps : List Color, toColorSteps : { hue : Float } -> List Color } -> Html msg
vividPicker { monoSteps, toColorSteps } =
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "repeat(361, 1fr)"
            ]
        ]
        (List.map (div []) <|
            List.map cell monoSteps
                :: List.map (\hue -> List.map cell (toColorSteps { hue = toFloat hue * 1 })) (List.range 0 359)
        )


cell : ColorValue compatible -> Html msg
cell color =
    div
        [ css
            [ padding2 (px 1) zero
            , backgroundColor color
            , fontFamily sansSerif
            ]
        ]
        []



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
