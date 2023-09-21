module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (header, input, label, text)
import Html.Styled.Attributes as Attributes exposing (css, type_, value)
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
    , showLabel : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hueSteps = 12
      , lSteps = 10
      , showLabel = True
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateHueSteps String
    | UpdateLSteps String
    | ToggleLabel String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateHueSteps value ->
            ( { model | hueSteps = Maybe.withDefault model.hueSteps (toInt value) }, Cmd.none )

        UpdateLSteps value ->
            ( { model | lSteps = Maybe.withDefault model.lSteps (toInt value) }, Cmd.none )

        ToggleLabel _ ->
            ( { model | showLabel = not model.showLabel }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view { hueSteps, lSteps, showLabel } =
    { title = "Oklch Preview"
    , body =
        [ header [ css [ padding2 (px 10) zero, displayFlex, property "column-gap" "1em" ] ]
            [ label [ css [ displayFlex, alignItems center, property "column-gap" "0.5em" ] ]
                [ text ("hue : " ++ String.fromInt hueSteps)
                , input [ type_ "range", value (String.fromInt hueSteps), Attributes.min "1", Attributes.max "360", onInput UpdateHueSteps ] []
                ]
            , label [ css [ displayFlex, alignItems center, property "column-gap" "5px" ] ]
                [ text ("L : " ++ String.fromInt lSteps)
                , input [ type_ "range", value (String.fromInt lSteps), Attributes.min "1", Attributes.max "150", onInput UpdateLSteps ] []
                ]
            , label [ css [ displayFlex, alignItems center, property "column-gap" "5px" ] ]
                [ input [ type_ "checkbox", Attributes.checked showLabel, onInput ToggleLabel ] []
                , text "show label"
                ]
            ]

        -- , VividPicker.oklch { hueSteps = hueSteps, luminanceSteps = lSteps, showLabel = showLabel }
        , VividPicker.okhslWithRGB { hueSteps = hueSteps, luminanceSteps = lSteps, showLabel = showLabel }
        , VividPicker.okhslWithOklab { hueSteps = hueSteps, luminanceSteps = lSteps, showLabel = showLabel }
        ]
            |> List.map Html.Styled.toUnstyled
    }
