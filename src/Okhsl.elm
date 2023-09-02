module Okhsl exposing (Okhsl, okhslToSrgb, srgbToOkhsl, toCssColor)

import ColorConversion
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


toCssColor : Okhsl -> Color
toCssColor { hue, saturation, lightness, alpha } =
    okhslToSrgb ( hue, saturation, lightness )
        |> (\( r, g, b ) -> Css.rgba (Basics.round r) (Basics.round g) (Basics.round b) alpha)
