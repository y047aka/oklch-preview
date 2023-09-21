module Color.ColorConversion exposing (okhslToSrgb, srgbToOkhsl)


srgbTransferFunction : Float -> Float
srgbTransferFunction a =
    if 0.0031308 >= a then
        12.92 * a

    else
        1.055 * (a ^ 0.4166666666666667) - 0.055


srgbTransferFunctionInv : Float -> Float
srgbTransferFunctionInv a =
    if 0.04045 < a then
        ((a + 0.055) / 1.055) ^ 2.4

    else
        a / 12.92


linearSrgbToOklab : ( Float, Float, Float ) -> ( Float, Float, Float )
linearSrgbToOklab ( r, g, b ) =
    let
        ( l, m, s ) =
            ( 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b
            , 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b
            , 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b
            )

        ( l_, m_, s_ ) =
            ( l ^ (1 / 3)
            , m ^ (1 / 3)
            , s ^ (1 / 3)
            )
    in
    ( 0.2104542553 * l_ + 0.793617785 * m_ - 0.0040720468 * s_
    , 1.9779984951 * l_ - 2.428592205 * m_ + 0.4505937099 * s_
    , 0.0259040371 * l_ + 0.7827717662 * m_ - 0.808675766 * s_
    )


oklabToLinearSrgb : ( Float, Float, Float ) -> ( Float, Float, Float )
oklabToLinearSrgb ( lightness, a, b ) =
    let
        ( l_, m_, s_ ) =
            ( lightness + 0.3963377774 * a + 0.2158037573 * b
            , lightness - 0.1055613458 * a - 0.0638541728 * b
            , lightness - 0.0894841775 * a - 1.291485548 * b
            )

        ( l, m, s ) =
            ( l_ * l_ * l_
            , m_ * m_ * m_
            , s_ * s_ * s_
            )
    in
    ( 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
    , -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
    , -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s
    )


toe : Float -> Float
toe x =
    let
        ( k_1, k_2 ) =
            ( 0.206, 0.03 )

        k_3 =
            (1 + k_1) / (1 + k_2)
    in
    0.5 * (k_3 * x - k_1 + sqrt ((k_3 * x - k_1) * (k_3 * x - k_1) + 4 * k_2 * k_3 * x))


toeInv : Float -> Float
toeInv x =
    let
        ( k_1, k_2 ) =
            ( 0.206, 0.03 )

        k_3 =
            (1 + k_1) / (1 + k_2)
    in
    (x * x + k_1 * x) / (k_3 * (x + k_2))


{-|

    Finds the maximum saturation possible for a given hue that fits in sRGB
    Saturation here is defined as S = C/L
    a and b must be normalized so a^2 + b^2 == 1

-}
computeMaxSaturation : Float -> Float -> Float
computeMaxSaturation a b =
    --  Max saturation will be when one of r, g or b goes below zero.
    -- Select different coefficients depending on which component goes below zero first
    let
        ( ( k0, k1, k2 ), ( k3, k4 ), ( wl, wm, ws ) ) =
            if -1.88170328 * a - 0.80936493 * b > 1 then
                ( ( 1.19086277, 1.76576728, 0.59662641 )
                , ( 0.75515197, 0.56771245 )
                , ( 4.0767416621, -3.3077115913, 0.2309699292 )
                )

            else if 1.81444104 * a - 1.19445276 * b > 1 then
                ( ( 0.73956515, -0.45954404, 0.08285427 )
                , ( 0.1254107, 0.14503204 )
                , ( -1.2684380046, 2.6097574011, -0.3413193965 )
                )

            else
                ( ( 1.35733652, -0.00915799, -1.1513021 )
                , ( -0.50559606, 0.00692167 )
                , ( -0.0041960863, -0.7034186147, 1.707614701 )
                )

        -- Approximate max saturation using a polynomial:
        saturation_ =
            k0 + k1 * a + k2 * b + k3 * a * a + k4 * a * b

        -- Do one step Halley's method to get closer
        -- this gives an error less than 10e6, except for some blue hues where the dS/dh is close to infinite
        -- this should be sufficient for most applications, otherwise do two/three steps
        ( k_l, k_m, k_s ) =
            ( 0.3963377774 * a + 0.2158037573 * b
            , -0.1055613458 * a - 0.0638541728 * b
            , -0.0894841775 * a - 1.291485548 * b
            )

        saturation =
            let
                ( l_, m_, s_ ) =
                    ( 1 + saturation_ * k_l
                    , 1 + saturation_ * k_m
                    , 1 + saturation_ * k_s
                    )

                ( l, m, s ) =
                    ( l_ * l_ * l_
                    , m_ * m_ * m_
                    , s_ * s_ * s_
                    )

                ( l_dS, m_dS, s_dS ) =
                    ( 3 * k_l * l_ * l_
                    , 3 * k_m * m_ * m_
                    , 3 * k_s * s_ * s_
                    )

                ( l_dS2, m_dS2, s_dS2 ) =
                    ( 6 * k_l * k_l * l_
                    , 6 * k_m * k_m * m_
                    , 6 * k_s * k_s * s_
                    )

                ( f, f1, f2 ) =
                    ( wl * l + wm * m + ws * s
                    , wl * l_dS + wm * m_dS + ws * s_dS
                    , wl * l_dS2 + wm * m_dS2 + ws * s_dS2
                    )
            in
            saturation_ - (f * f1) / (f1 * f1 - 0.5 * f * f2)
    in
    saturation


findCusp : Float -> Float -> ( Float, Float )
findCusp a b =
    let
        -- First, find the maximum saturation (saturation S = C/L)
        s_cusp =
            computeMaxSaturation a b

        -- Convert to linear sRGB to find the first point where at least one of r,g or b >= 1:
        rgb_at_max =
            oklabToLinearSrgb ( 1, s_cusp * a, s_cusp * b )

        l_cusp =
            rgb_at_max
                |> (\( ram_0, ram_1, ram_2 ) ->
                        (1 / max (max ram_0 ram_1) ram_2) ^ (1 / 3)
                   )

        c_cusp =
            l_cusp * s_cusp
    in
    ( l_cusp, c_cusp )


{-| Finds intersection of the line defined by
L = L0 \* (1 - t) + t \* L1;
C = t \* C1;
a and b must be normalized so a^2 + b^2 == 1
-}
findGamutIntersection : Float -> Float -> Float -> Float -> Float -> Maybe ( Float, Float ) -> Float
findGamutIntersection a b l1 c1 l0 maybeCusp =
    let
        ( cusp_0, cusp_1 ) =
            case maybeCusp of
                Just cusp ->
                    cusp

                Nothing ->
                    -- Find the cusp of the gamut triangle
                    findCusp a b
    in
    -- Find the intersection for upper and lower half seprately
    if ((l1 - l0) * cusp_1 - (cusp_0 - l0) * c1) <= 0 then
        -- Lower half
        (cusp_1 * l0) / (c1 * cusp_0 + cusp_1 * (l0 - l1))

    else
        -- Upper half
        let
            -- First intersect with triangle
            t_ =
                (cusp_1 * (l0 - 1)) / (c1 * (cusp_0 - 1) + cusp_1 * (l0 - l1))

            t =
                -- Then one step Halley's method
                let
                    ( dL, dC ) =
                        ( l1 - l0
                        , c1
                        )

                    ( k_l, k_m, k_s ) =
                        ( 0.3963377774 * a + 0.2158037573 * b
                        , -0.1055613458 * a - 0.0638541728 * b
                        , -0.0894841775 * a - 1.291485548 * b
                        )

                    ( l_dt, m_dt, s_dt ) =
                        ( dL + dC * k_l
                        , dL + dC * k_m
                        , dL + dC * k_s
                        )

                    -- If higher accuracy is required, 2 or 3 iterations of the following block can be used:
                    ( lightness, chroma ) =
                        ( l0 * (1 - t_) + t_ * l1
                        , t_ * c1
                        )

                    ( l_, m_, s_ ) =
                        ( lightness + chroma * k_l
                        , lightness + chroma * k_m
                        , lightness + chroma * k_s
                        )

                    ( l, m, s ) =
                        ( l_ * l_ * l_
                        , m_ * m_ * m_
                        , s_ * s_ * s_
                        )

                    ( ldt, mdt, sdt ) =
                        ( 3 * l_dt * l_ * l_
                        , 3 * m_dt * m_ * m_
                        , 3 * s_dt * s_ * s_
                        )

                    ( ldt2, mdt2, sdt2 ) =
                        ( 6 * l_dt * l_dt * l_
                        , 6 * m_dt * m_dt * m_
                        , 6 * s_dt * s_dt * s_
                        )

                    ( r, r1, r2 ) =
                        ( 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s - 1
                        , 4.0767416621 * ldt - 3.3077115913 * mdt + 0.2309699292 * sdt
                        , 4.0767416621 * ldt2 - 3.3077115913 * mdt2 + 0.2309699292 * sdt2
                        )

                    u_r =
                        r1 / (r1 * r1 - 0.5 * r * r2)

                    ( g, g1, g2 ) =
                        ( -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s - 1
                        , -1.2684380046 * ldt + 2.6097574011 * mdt - 0.3413193965 * sdt
                        , -1.2684380046 * ldt2 + 2.6097574011 * mdt2 - 0.3413193965 * sdt2
                        )

                    u_g =
                        g1 / (g1 * g1 - 0.5 * g * g2)

                    ( b_, b1, b2 ) =
                        ( -0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s - 1
                        , -0.0041960863 * ldt - 0.7034186147 * mdt + 1.707614701 * sdt
                        , -0.0041960863 * ldt2 - 0.7034186147 * mdt2 + 1.707614701 * sdt2
                        )

                    u_b =
                        b1 / (b1 * b1 - 0.5 * b_ * b2)

                    t_r =
                        if u_r >= 0 then
                            -r * u_r

                        else
                            1.0e6

                    t_g =
                        if u_g >= 0 then
                            -g * u_g

                        else
                            1.0e6

                    t_b =
                        if u_b >= 0 then
                            -b_ * u_b

                        else
                            1.0e6
                in
                t_ + min t_r (min t_g t_b)
        in
        t


getSTMax : Float -> Float -> Maybe ( Float, Float ) -> ( Float, Float )
getSTMax a_ b_ maybeCusp =
    let
        ( lightness, chroma ) =
            case maybeCusp of
                Just cusp ->
                    cusp

                Nothing ->
                    findCusp a_ b_
    in
    ( chroma / lightness, chroma / (1 - lightness) )


getCs : ( Float, Float, Float ) -> ( Float, Float, Float )
getCs ( lightness, a_, b_ ) =
    let
        cusp =
            findCusp a_ b_

        ( c_max, st_max ) =
            ( findGamutIntersection a_ b_ lightness 1 lightness (Just cusp)
            , getSTMax a_ b_ (Just cusp)
            )

        s_mid =
            0.11516993
                + (1
                    / (7.4477897
                        + (4.1590124 * b_)
                        + a_
                        * (-2.19557347
                            + (1.75198401 * b_)
                            + a_
                            * (-2.13704948
                                - (10.02301043 * b_)
                                + (a_ * (-4.24894561 + 5.38770819 * b_ + 4.69891013 * a_))
                              )
                          )
                      )
                  )

        t_mid =
            0.11239642
                + (1
                    / (1.6132032
                        - (0.68124379 * b_)
                        + a_
                        * (0.40370612
                            + (0.90148123 * b_)
                            + a_
                            * (-0.27087943
                                + (0.6122399 * b_)
                                + (a_ * (0.00299215 - 0.45399568 * b_ - 0.14661872 * a_))
                              )
                          )
                      )
                  )

        k =
            let
                ( st_max_0, st_max_1 ) =
                    st_max
            in
            c_max / min (lightness * st_max_0) ((1 - lightness) * st_max_1)

        c_mid =
            let
                ( c_a, c_b ) =
                    ( lightness * s_mid
                    , (1 - lightness) * t_mid
                    )
            in
            0.9 * k * sqrt (sqrt (1 / (1 / (c_a * c_a * c_a * c_a) + 1 / (c_b * c_b * c_b * c_b))))

        c_0 =
            let
                ( c_a, c_b ) =
                    ( lightness * 0.4
                    , (1 - lightness) * 0.8
                    )
            in
            sqrt (1 / (1 / (c_a * c_a) + 1 / (c_b * c_b)))
    in
    ( c_0, c_mid, c_max )


okhslToSrgb : ( Float, Float, Float ) -> ( Float, Float, Float )
okhslToSrgb ( h, s, l ) =
    if l == 1 then
        ( 255, 255, 255 )

    else if l == 0 then
        ( 0, 0, 0 )

    else
        let
            ( a_, b_, lightness ) =
                ( cos (2 * pi * h)
                , sin (2 * pi * h)
                , toeInv l
                )

            ( c_0, c_mid, c_max ) =
                getCs ( lightness, a_, b_ )

            ( t, k_0, k_1 ) =
                if s < 0.8 then
                    ( 1.25 * s
                    , 0
                    , 0.8 * c_0
                    )

                else
                    ( 5 * (s - 0.8)
                    , c_mid
                    , (0.2 * c_mid * c_mid * 1.25 * 1.25) / c_0
                    )

            k_2 =
                if s < 0.8 then
                    1 - k_1 / c_mid

                else
                    1 - k_1 / (c_max - c_mid)

            chroma =
                k_0 + (t * k_1) / (1 - k_2 * t)

            ( r, g, b ) =
                oklabToLinearSrgb ( lightness, chroma * a_, chroma * b_ )
        in
        ( 255 * srgbTransferFunction r
        , 255 * srgbTransferFunction g
        , 255 * srgbTransferFunction b
        )


srgbToOkhsl : ( Float, Float, Float ) -> ( Float, Float, Float )
srgbToOkhsl ( r, g, b ) =
    let
        ( lab_0, lab_1, lab_2 ) =
            linearSrgbToOklab
                ( srgbTransferFunctionInv (r / 255)
                , srgbTransferFunctionInv (g / 255)
                , srgbTransferFunctionInv (b / 255)
                )

        chroma =
            sqrt (lab_1 * lab_1 + lab_2 * lab_2)

        ( a_, b_ ) =
            ( lab_1 / chroma
            , lab_2 / chroma
            )

        ( lightness, h ) =
            ( lab_0
            , 0.5 + (0.5 * atan2 -lab_2 -lab_1) / pi
            )

        ( c_0, c_mid, c_max ) =
            getCs ( lightness, a_, b_ )

        s =
            if chroma < c_mid then
                let
                    k_0 =
                        0

                    k_1 =
                        0.8 * c_0

                    k_2 =
                        1 - k_1 / c_mid

                    t =
                        (chroma - k_0) / (k_1 + k_2 * (chroma - k_0))
                in
                t * 0.8

            else
                let
                    k_0 =
                        c_mid

                    k_1 =
                        (0.2 * c_mid * c_mid * 1.25 * 1.25) / c_0

                    k_2 =
                        1 - k_1 / (c_max - c_mid)

                    t =
                        (chroma - k_0) / (k_1 + k_2 * (chroma - k_0))
                in
                0.8 + 0.2 * t

        l =
            toe lightness
    in
    ( h, s, l )
