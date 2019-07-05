module Main exposing (main)

import Browser
import Html exposing (..)


main : Program Flags Model Msg
main =
    Browser.sandbox
        { init = Polar 1 45
        , view = view
        , update = update
        }


type alias Flags =
    ()


type alias Model =
    Polar


type alias Polar =
    { magnitude : Int
    , theta : Int
    }


type Msg
    = Noop


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    text "hello"
