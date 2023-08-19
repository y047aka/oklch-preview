module VividPicker exposing (view)

import Css exposing (..)
import HSL exposing (hslSteps, monoSteps)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)


view : Html msg
view =
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
