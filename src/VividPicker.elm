module VividPicker exposing (hsl, hsluv, okhsl, okhsl_port, oklch)

{-| A color picker for the Vivid color space.
-}

import Css exposing (..)
import HSLuv
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Okhsl exposing (Okhsl)
import Oklch


hsl :
    { hueSteps : Int
    , lightnessSteps : Int
    , label : Color -> String
    }
    -> Html msg
hsl { hueSteps, lightnessSteps, label } =
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
        , label = label
        }


hsluv :
    { hueSteps : Int
    , lightnessSteps : Int
    , label : Color -> String
    }
    -> Html msg
hsluv { hueSteps, lightnessSteps, label } =
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
        , label = label
        }


oklch :
    { hueSteps : Int
    , luminanceSteps : Int
    , label : Color -> String
    }
    -> Html msg
oklch { hueSteps, luminanceSteps, label } =
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
        , label = label
        }


okhsl :
    { hueSteps : Int
    , luminanceSteps : Int
    , label : Color -> String
    }
    -> Html msg
okhsl { hueSteps, luminanceSteps, label } =
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
        , label = label
        }


okhsl_port :
    { monoSteps : List Color
    , colorGrid : List (List Color)
    , label : Color -> String
    }
    -> Html msg
okhsl_port { monoSteps, colorGrid, label } =
    vividPicker
        { monoSteps = monoSteps
        , colorGrid = colorGrid
        , label = label
        }


vividPicker :
    { monoSteps : List Color
    , colorGrid : List (List Color)
    , label : Color -> String
    }
    -> Html msg
vividPicker { monoSteps, colorGrid, label } =
    let
        grid =
            monoSteps :: colorGrid

        gridTemplateColumns =
            "repeat(" ++ (String.fromInt <| List.length grid) ++ ", 1fr)"

        gridTemplateRows =
            "repeat(" ++ (String.fromInt <| List.length monoSteps) ++ ", 1fr)"
    in
    div
        [ css
            [ minHeight (px 400)
            , property "display" "grid"
            , property "grid-template-columns" gridTemplateColumns
            , property "grid-template-rows" gridTemplateRows
            , property "grid-auto-flow" "column"
            ]
        ]
        (List.map (cell label) (List.concat grid))


cell : (ColorValue compatible -> String) -> ColorValue compatible -> Html msg
cell label color_ =
    div
        [ css
            [ backgroundColor color_
            , pseudoClass "not(:empty)"
                [ padding2 (px 15) zero
                , displayFlex
                , alignItems center
                , justifyContent center
                , fontFamily sansSerif
                , fontSize (em 0.8)
                ]
            ]
        ]
        [ text (label color_) ]



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
