module HSL exposing (hslSteps, monoSteps)

import Css exposing (Color, hsl)


hslSteps : { hue : Float } -> List Color
hslSteps { hue } =
    List.map (\l -> hsl hue 1 l) (List.reverse <| range 0 1 0.005)


monoSteps : List Color
monoSteps =
    List.map (\l -> hsl 0 0 l) (range 0 1 0.005)
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
