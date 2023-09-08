module Main exposing (main)

import Browser exposing (Document)
import Css exposing (..)
import Html.Styled
import Oklch
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
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Oklch Preview"
    , body =
        [ VividPicker.oklch { hueSteps = 360 }
        , VividPicker.okhsl { hueSteps = 360 }
        , Oklch.view
        ]
            |> List.map Html.Styled.toUnstyled
    }
