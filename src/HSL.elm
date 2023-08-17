module HSL exposing (vividPicker)

import Css exposing (..)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)


vividPicker : Html msg
vividPicker =
    div
        [ css
            [ property "display" "grid"
            , property "grid-template-columns" "repeat(361, 1fr)"
            ]
        ]
        (List.map (div []) <|
            List.map cell monoSteps
                :: List.map (\hue -> List.map cell (hslSteps { hue = toFloat hue * 1 })) (List.range 0 359)
        )


hslSteps : { hue : Float } -> List Color
hslSteps { hue } =
    List.map (\l -> hsl hue 1 l) (List.reverse <| range 0 1 0.005)


monoSteps : List Color
monoSteps =
    List.map (\l -> hsl 0 0 l) (range 0 1 0.005)
        |> List.reverse


cell : Color -> Html msg
cell hslColor =
    div
        [ css
            [ padding2 (px 1) zero
            , backgroundColor hslColor
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
