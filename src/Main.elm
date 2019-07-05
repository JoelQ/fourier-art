module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Json.Decode exposing (Decoder)
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



-- UPDATE


type Msg
    = UserChangedMagnitude Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserChangedMagnitude newMagnitude ->
            { model | magnitude = newMagnitude }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg [] [ vectorCircle model ]
        , magnitudeRange model
        ]


magnitudeRange : Polar -> Html Msg
magnitudeRange point =
    Html.div []
        [ Html.label [] [ Html.text "Magnitude" ]
        , Html.input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.max "10"
            , HtmlAttr.min "1"
            , HtmlAttr.step "1"
            , HtmlAttr.value <| String.fromInt <| point.magnitude
            , onRangeInput UserChangedMagnitude
            ]
            []
        , Html.text <| String.fromInt <| point.magnitude
        ]


onRangeInput : (Int -> msg) -> Html.Attribute msg
onRangeInput tagger =
    Html.Events.on "input" (Json.Decode.map tagger rangeTarget)


rangeTarget : Decoder Int
rangeTarget =
    Html.Events.targetValue
        |> Json.Decode.andThen (decoderFromMaybe << String.toInt)


decoderFromMaybe : Maybe a -> Decoder a
decoderFromMaybe maybe =
    case maybe of
        Just value ->
            Json.Decode.succeed value

        Nothing ->
            Json.Decode.fail "No value"


type Pixel
    = Pixel Int


pixelsPerUnit : Int
pixelsPerUnit =
    5


pixelRadius : Polar -> Int
pixelRadius point =
    pixelsPerUnit * point.magnitude


vectorCircle : Polar -> Svg a
vectorCircle point =
    Svg.circle
        [ SvgAttr.cx "50"
        , SvgAttr.cy "50"
        , SvgAttr.r <| String.fromInt <| pixelRadius point
        , SvgAttr.fill "white"
        , SvgAttr.stroke "black"
        ]
        []
