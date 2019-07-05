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
    { magnitude : Float
    , theta : Float
    }



-- UPDATE


type Msg
    = UserChangedMagnitude Float
    | UserChangedTheta Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserChangedMagnitude newMagnitude ->
            { model | magnitude = newMagnitude }

        UserChangedTheta newTheta ->
            { model | theta = newTheta }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg
            [ HtmlAttr.width <| pixelToInt <| circleViewport.width
            , HtmlAttr.height <| pixelToInt <| circleViewport.height
            ]
            [ vectorCircle model, vectorLine model ]
        , magnitudeRange model
        , thetaRange model
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
            , HtmlAttr.value <| String.fromFloat <| point.magnitude
            , onRangeInput UserChangedMagnitude
            ]
            []
        , Html.text <| String.fromFloat <| point.magnitude
        ]


thetaRange : Polar -> Html Msg
thetaRange point =
    Html.div []
        [ Html.label [] [ Html.text "Direction" ]
        , Html.input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.max "360"
            , HtmlAttr.min "0"
            , HtmlAttr.step "1"
            , HtmlAttr.value <| String.fromFloat <| point.theta
            , onRangeInput UserChangedTheta
            ]
            []
        , Html.text <| String.fromFloat <| point.theta
        ]


onRangeInput : (Float -> msg) -> Html.Attribute msg
onRangeInput tagger =
    Html.Events.on "input" (Json.Decode.map tagger rangeTarget)


rangeTarget : Decoder Float
rangeTarget =
    Html.Events.targetValue
        |> Json.Decode.andThen (decoderFromMaybe << String.toFloat)


decoderFromMaybe : Maybe a -> Decoder a
decoderFromMaybe maybe =
    case maybe of
        Just value ->
            Json.Decode.succeed value

        Nothing ->
            Json.Decode.fail "No value"


type Pixel
    = Pixel Float


type alias ScreenCoord =
    { x : Pixel
    , y : Pixel
    }


type alias Viewport =
    { width : Pixel
    , height : Pixel
    }


toScreenCoords : Viewport -> Polar -> ScreenCoord
toScreenCoords viewport point =
    let
        ( x, y ) =
            fromPolar ( point.magnitude, degrees point.theta )

        (Pixel width) =
            viewport.width

        viewCenterX =
            width / 2

        (Pixel height) =
            viewport.height

        viewCenterY =
            height / 2
    in
    { x = Pixel <| (x * pixelsPerUnit) + viewCenterX
    , y = Pixel <| viewCenterY - (y * pixelsPerUnit)
    }


screenOrigin : ScreenCoord
screenOrigin =
    toScreenCoords circleViewport (Polar 0 0)


circleViewport : Viewport
circleViewport =
    { width = Pixel 100
    , height = Pixel 100
    }


pixelToInt : Pixel -> Int
pixelToInt (Pixel px) =
    round px


pixelToString : Pixel -> String
pixelToString (Pixel px) =
    String.fromFloat px


pixelsPerUnit : Float
pixelsPerUnit =
    5


pixelRadius : Polar -> Float
pixelRadius point =
    pixelsPerUnit * point.magnitude


vectorCircle : Polar -> Svg a
vectorCircle point =
    Svg.circle
        [ SvgAttr.cx <| pixelToString screenOrigin.x
        , SvgAttr.cy <| pixelToString screenOrigin.y
        , SvgAttr.r <| String.fromFloat <| pixelRadius point
        , SvgAttr.fill "white"
        , SvgAttr.stroke "black"
        ]
        []


vectorLine : Polar -> Svg a
vectorLine point =
    Svg.line
        [ SvgAttr.x1 <| pixelToString screenOrigin.x
        , SvgAttr.y1 <| pixelToString screenOrigin.y
        , SvgAttr.x2 <| pixelToString <| .x <| toScreenCoords circleViewport point
        , SvgAttr.y2 <| pixelToString <| .y <| toScreenCoords circleViewport point
        , SvgAttr.stroke "black"
        ]
        []
