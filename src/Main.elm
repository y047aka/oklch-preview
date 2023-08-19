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
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view _ =
    { title = "Oklch Preview"
    , body =
        [ VividPicker.view
        , Oklch.view
        ]
            |> List.map Html.Styled.toUnstyled
    }
