module Color.Oklch exposing (Oklch, oklch)

import Css exposing (Color)


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
