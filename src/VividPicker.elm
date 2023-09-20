module VividPicker exposing (hsl, hsluv, okhsl, okhsl_port, oklch)

{-| A color picker for the Vivid color space.
-}

import Color exposing (Color(..), toCssColor, toStringIf)
import Css exposing (alignItems, backgroundColor, center, displayFlex, em, fontFamily, fontSize, justifyContent, minHeight, padding2, property, pseudoClass, px, sansSerif, zero)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)


hsl :
    { hueSteps : Int
    , lightnessSteps : Int
    , showLabel : Bool
    }
    -> Html msg
hsl { hueSteps, lightnessSteps, showLabel } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat lightnessSteps)

        toHslSteps hue =
            List.map (\l -> HSL hue 1 l) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> HSL 0 0 l) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toHslSteps
        , showLabel = showLabel
        }


hsluv :
    { hueSteps : Int
    , lightnessSteps : Int
    , showLabel : Bool
    }
    -> Html msg
hsluv { hueSteps, lightnessSteps, showLabel } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat lightnessSteps)

        toHslSteps hue =
            List.map (\l -> HSLuv hue 1 l) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> HSLuv 0 0 l) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toHslSteps
        , showLabel = showLabel
        }


oklch :
    { hueSteps : Int
    , luminanceSteps : Int
    , showLabel : Bool
    }
    -> Html msg
oklch { hueSteps, luminanceSteps, showLabel } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat luminanceSteps)

        toOklchSteps hue =
            List.map (\l -> Oklch l 0.2 hue) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> Oklch l 0 0) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toOklchSteps
        , showLabel = showLabel
        }


okhsl :
    { hueSteps : Int
    , luminanceSteps : Int
    , showLabel : Bool
    }
    -> Html msg
okhsl { hueSteps, luminanceSteps, showLabel } =
    let
        lSteps =
            reverseRange 0 1 (1 / toFloat luminanceSteps)

        toOkhslSteps hue =
            List.map (\l -> Okhsl (hue / 360) 1 l) lSteps
    in
    vividPicker
        { monoSteps = List.map (\l -> Okhsl 0 0 l) lSteps
        , colorGrid =
            range 0 359 (360 / toFloat hueSteps)
                |> List.map toOkhslSteps
        , showLabel = showLabel
        }


okhsl_port :
    { monoSteps : List Color
    , colorGrid : List (List Color)
    , showLabel : Bool
    }
    -> Html msg
okhsl_port =
    vividPicker


vividPicker :
    { monoSteps : List Color
    , colorGrid : List (List Color)
    , showLabel : Bool
    }
    -> Html msg
vividPicker { monoSteps, colorGrid, showLabel } =
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
        (List.concat grid
            |> List.map (\c -> cell (toStringIf showLabel c) (toCssColor c))
        )


cell : String -> Css.Color -> Html msg
cell label color =
    div
        [ css
            [ backgroundColor color
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
        [ text label ]



-- HELPERS


range : Float -> Float -> Float -> List Float
range lo hi step =
    rangeHelp lo hi step []


rangeHelp : Float -> Float -> Float -> List Float -> List Float
rangeHelp lo hi step list =
    if lo <= hi then
        rangeHelp (lo + step) hi step (lo :: list)

    else
        List.reverse list


reverseRange : Float -> Float -> Float -> List Float
reverseRange lo hi step =
    range lo hi step
        |> List.reverse
