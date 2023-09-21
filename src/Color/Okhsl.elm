module Color.Okhsl exposing (Okhsl, okhslToSrgb, srgbToOkhsl, toCssColor_Oklab, toCssColor_RGB)

import Color.ColorConversion as ColorConversion
import Css exposing (Color)


type alias Okhsl =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


okhslToSrgb : ( Float, Float, Float ) -> ( Float, Float, Float )
okhslToSrgb =
    ColorConversion.okhslToSrgb


srgbToOkhsl : ( Float, Float, Float ) -> ( Float, Float, Float )
srgbToOkhsl =
    ColorConversion.srgbToOkhsl


okhslToOklab : ( Float, Float, Float ) -> ( Float, Float, Float )
okhslToOklab =
    ColorConversion.okhslToOklab


toCssColor_RGB : Okhsl -> Color
toCssColor_RGB { hue, saturation, lightness, alpha } =
    okhslToSrgb ( hue, saturation, lightness )
        |> (\( r, g, b ) -> Css.rgba (Basics.round r) (Basics.round g) (Basics.round b) alpha)


toCssColor_Oklab : Okhsl -> Color
toCssColor_Oklab { hue, saturation, lightness, alpha } =
    okhslToOklab ( hue, saturation, lightness )
        |> (\( l, a, b ) -> oklab l a b)


oklab : Float -> Float -> Float -> Color
oklab luminance a b =
    let
        valuesList =
            [ numericalPercentageToString luminance
            , String.fromFloat a
            , String.fromFloat b
            ]
    in
    Css.rgb 0 0 0
        |> (\color -> { color | value = "oklab(" ++ String.join " " valuesList ++ ")" })


numericalPercentageToString : Float -> String
numericalPercentageToString value =
    String.fromFloat (value * 100) ++ "%"
