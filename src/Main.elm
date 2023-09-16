module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (header, input, label, text)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onInput)
import String exposing (toInt)
import VividPicker


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
    { hueSteps : Int
    , lSteps : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hueSteps = 12
      , lSteps = 10
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateHueSteps String
    | UpdateLSteps String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateHueSteps value ->
            ( { model | hueSteps = Maybe.withDefault model.hueSteps (toInt value) }, Cmd.none )

        UpdateLSteps value ->
            ( { model | lSteps = Maybe.withDefault model.lSteps (toInt value) }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view { hueSteps, lSteps } =
    { title = "Oklch Preview"
    , body =
        [ header [ css [ padding2 (px 10) zero, displayFlex, property "column-gap" "1em" ] ]
            [ label [ css [ displayFlex, alignItems center, property "column-gap" "0.5em" ] ]
                [ text "hue", input [ type_ "number", value (String.fromInt hueSteps), onInput UpdateHueSteps ] [] ]
            , label [ css [ displayFlex, alignItems center, property "column-gap" "5px" ] ]
                [ text "lightness / luminance", input [ type_ "number", value (String.fromInt lSteps), onInput UpdateLSteps ] [] ]
            ]
        , VividPicker.oklch { hueSteps = hueSteps, luminanceSteps = lSteps, label = always "Oklch" }
        , VividPicker.okhsl { hueSteps = hueSteps, luminanceSteps = lSteps, label = always "Okhsl" }
        ]
            |> List.map Html.Styled.toUnstyled
    }
