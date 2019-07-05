module Main exposing (main)

import Browser
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


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



-- VIEW


view : Model -> Html Msg
view model =
    Svg.svg [] [ vectorCircle model ]


vectorCircle : Polar -> Svg a
vectorCircle point =
    Svg.circle [ SvgAttr.cx "50", SvgAttr.cy "50", SvgAttr.r "50" ] []
