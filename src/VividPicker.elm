module VividPicker exposing (hsl, hsluv, okhsl, okhsl_port, oklch)

{-| A color picker for the Vivid color space.
-}

import Css exposing (..)
import HSLuv
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Okhsl exposing (Okhsl)
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
        , colorGrid = List.map (\hue -> hslSteps { hue = toFloat hue }) (List.range 0 359)
        }


hsluv : Html msg
hsluv =
    let
        monoSteps =
            reverseRange 0 1 0.005
                |> List.map (\l -> hsluvToRgba ( 0, 0, l * 100 ))

        hslSteps { hue } =
            reverseRange 0 1 0.005
                |> List.map (\l -> hsluvToRgba ( hue, 100, l * 100 ))

        hsluvToRgba =
            HSLuv.hsluvToRgb
                >> (\( red, green, blue ) ->
                        Css.rgba (Basics.round <| red * 256) (Basics.round <| green * 256) (Basics.round <| blue * 256) 1
                   )
    in
    vividPicker
        { monoSteps = monoSteps
        , colorGrid = List.map (\hue -> hslSteps { hue = toFloat hue }) (List.range 0 359)
        }


oklch : Html msg
oklch =
    let
        monoSteps =
            reverseRange 0 1 0.005
                |> List.map (\l -> Oklch.oklch l 0 0)

        oklchSteps { hue } =
            reverseRange 0 1 0.005
                |> List.map (\l -> Oklch.oklch l 0.2 hue)
    in
    vividPicker
        { monoSteps = monoSteps
        , colorGrid = List.map (\hue -> oklchSteps { hue = toFloat hue }) (List.range 0 359)
        }


okhsl : Html msg
okhsl =
    let
        monoSteps =
            reverseRange 0 1 0.005
                |> List.map (\l -> Okhsl 0 0 l 1 |> Okhsl.toCssColor)

        okhslSteps { hue } =
            reverseRange 0 1 0.005
                |> List.map (\l -> Okhsl (hue / 360) 1 l 1 |> Okhsl.toCssColor)
    in
    vividPicker
        { monoSteps = monoSteps
        , colorGrid = List.map (\hue -> okhslSteps { hue = toFloat hue }) (List.range 0 359)
        }


okhsl_port : List Color -> List (List Color) -> Html msg
okhsl_port monoSteps colorGrid =
    vividPicker { monoSteps = monoSteps, colorGrid = colorGrid }


vividPicker : { monoSteps : List Color, colorGrid : List (List Color) } -> Html msg
vividPicker { monoSteps, colorGrid } =
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "repeat(361, 1fr)"
            ]
        ]
        (List.map (div []) (List.map (List.map cell) (monoSteps :: colorGrid)))


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
