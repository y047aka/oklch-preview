module Main exposing (main)

import Browser exposing (Document)
import Css exposing (Color, backgroundColor)
import Html.Styled exposing (div, text)
import Html.Styled.Attributes exposing (css)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Oklch Preview"
    , body =
        [ div [ css [ backgroundColor (oklch 0.6 0 0) ] ] [ text "Oklch" ] ]
            |> List.map Html.Styled.toUnstyled
    }



-- OKLCH


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
