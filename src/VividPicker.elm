module VividPicker exposing (hsl, oklch)

import Css exposing (..)
import HSL exposing (hslSteps)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Oklch exposing (Oklch)



-- HSL


hsl : Html msg
hsl =
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "repeat(361, 1fr)"
            ]
        ]
        (List.map (div []) <|
            List.map cell_hsl HSL.monoSteps
                :: List.map (\hue -> List.map cell_hsl (hslSteps { hue = toFloat hue * 1 })) (List.range 0 359)
        )


cell_hsl : Color -> Html msg
cell_hsl hslColor =
    div
        [ css
            [ padding2 (px 1) zero
            , backgroundColor hslColor
            , fontFamily sansSerif
            ]
        ]
        []



-- OKLCH


oklch : Html msg
oklch =
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "repeat(361, 1fr)"
            ]
        ]
        (List.map (div []) <|
            List.map cell_oklch monoSteps_oklch
                :: List.map (\hue -> List.map cell_oklch (oklchSteps { hue = toFloat hue * 1 })) (List.range 0 359)
        )


cell_oklch : Oklch -> Html msg
cell_oklch { luminance, chroma, hue } =
    div
        [ css
            [ padding2 (px 1) zero
            , backgroundColor (Oklch.oklch luminance chroma hue)
            , fontFamily sansSerif
            ]
        ]
        []


oklchSteps : { hue : Float } -> List Oklch
oklchSteps { hue } =
    List.map (\l -> Oklch l 0.2 hue) (List.reverse <| range 0 1 0.005)


monoSteps_oklch : List Oklch
monoSteps_oklch =
    List.map (\l -> Oklch l 0 0) (range 0 1 0.005)
        |> List.reverse



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
