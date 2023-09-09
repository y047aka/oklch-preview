module VividPicker exposing (hsl, hsluv, okhsl, okhsl_port, oklch)

{-| A color picker for the Vivid color space.
-}

import Css exposing (..)
import HSLuv
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Okhsl exposing (Okhsl)
import Oklch


hsl : { hueSteps : Int, lightnessSteps : Int } -> Html msg
hsl { hueSteps, lightnessSteps } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat lightnessSteps)

        toHslSteps hue =
            List.map (\l -> Css.hsl hue 1 l) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> Css.hsl 0 0 l) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toHslSteps
        }


hsluv : { hueSteps : Int, lightnessSteps : Int } -> Html msg
hsluv { hueSteps, lightnessSteps } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat lightnessSteps)

        toHslSteps hue =
            List.map (\l -> hsluvToRgba ( hue, 100, l * 100 )) lSteps

        hsluvToRgba =
            HSLuv.hsluvToRgb
                >> (\( red, green, blue ) ->
                        Css.rgba (Basics.round <| red * 256) (Basics.round <| green * 256) (Basics.round <| blue * 256) 1
                   )
    in
    vividPicker
        { monoSteps = List.map (\l -> hsluvToRgba ( 0, 0, l * 100 )) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toHslSteps
        }


oklch : { hueSteps : Int, luminanceSteps : Int } -> Html msg
oklch { hueSteps, luminanceSteps } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat luminanceSteps)

        toOklchSteps hue =
            List.map (\l -> Oklch.oklch l 0.2 hue) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> Oklch.oklch l 0 0) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toOklchSteps
        }


okhsl : { hueSteps : Int, luminanceSteps : Int } -> Html msg
okhsl { hueSteps, luminanceSteps } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat luminanceSteps)

        toOkhslSteps hue =
            List.map (\l -> Okhsl (hue / 360) 1 l 1 |> Okhsl.toCssColor) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> Okhsl 0 0 l 1 |> Okhsl.toCssColor) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toOkhslSteps
        }


okhsl_port : List Color -> List (List Color) -> Html msg
okhsl_port monoSteps colorGrid =
    vividPicker { monoSteps = monoSteps, colorGrid = colorGrid }


vividPicker : { monoSteps : List Color, colorGrid : List (List Color) } -> Html msg
vividPicker { monoSteps, colorGrid } =
    let
        grid =
            monoSteps :: colorGrid

        gridTemplateColumns =
            "repeat(" ++ (String.fromInt <| List.length grid) ++ ", 1fr)"
    in
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" gridTemplateColumns
            ]
        ]
        (List.map (div []) (List.map (List.map cell) grid))


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
