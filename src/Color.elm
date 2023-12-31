module Color exposing (Color(..), toCssColor, toString, toStringIf)

import Color.Okhsl as Okhsl
import Color.Oklch as Oklch
import Css
import HSLuv


type Color
    = HSL Hue Saturation Lightness
    | HSLuv Hue Saturation Lightness
    | Oklch Luminance Chroma Hue
    | OkhslWithRGB Hue Saturation Lightness
    | OkhslWithOklab Hue Saturation Lightness


type alias Hue =
    Float


type alias Saturation =
    Float


type alias Lightness =
    Float


type alias Luminance =
    Float


type alias Chroma =
    Float


toCssColor : Color -> Css.Color
toCssColor color =
    case color of
        HSL h s l ->
            Css.hsl h s l

        HSLuv h s l ->
            HSLuv.hsluvToRgb ( h, s * 100, l * 100 )
                |> (\( red, green, blue ) ->
                        Css.rgba (Basics.round <| red * 256) (Basics.round <| green * 256) (Basics.round <| blue * 256) 1
                   )

        Oklch l c h ->
            Oklch.oklch l c h

        OkhslWithRGB h s l ->
            Okhsl.Okhsl h s l 1
                |> Okhsl.toCssColor_RGB

        OkhslWithOklab h s l ->
            Okhsl.Okhsl h s l 1
                |> Okhsl.toCssColor_Oklab


toString : Color -> String
toString color =
    let
        toString_ =
            Basics.round >> String.fromInt
    in
    case color of
        HSL h s l ->
            "hsl (" ++ toString_ h ++ ", " ++ (toString_ <| s * 100) ++ "%, " ++ (toString_ <| l * 100) ++ "%)"

        HSLuv h s l ->
            "hsluv (" ++ toString_ h ++ ", " ++ (toString_ <| s * 100) ++ "%, " ++ (toString_ <| l * 100) ++ "%)"

        Oklch l c h ->
            "oklch (" ++ (toString_ <| l * 100) ++ "%, " ++ String.fromFloat c ++ ", " ++ toString_ h ++ ")"

        OkhslWithRGB h s l ->
            "okhsl (" ++ (toString_ <| h * 360) ++ ", " ++ (toString_ <| s * 100) ++ "%, " ++ (toString_ <| l * 100) ++ "%)"

        OkhslWithOklab h s l ->
            "okhsl (" ++ (toString_ <| h * 360) ++ ", " ++ (toString_ <| s * 100) ++ "%, " ++ (toString_ <| l * 100) ++ "%)"


toStringIf : Bool -> Color -> String
toStringIf bool color =
    if bool then
        toString color

    else
        ""
