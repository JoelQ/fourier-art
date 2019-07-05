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
        { init = [ startingVector ]
        , view = view
        , update = update
        }


type alias Flags =
    ()


type alias Model =
    List Polar


type alias Polar =
    { magnitude : Float
    , theta : Float
    }


startingVector : Polar
startingVector =
    Polar 5 0



-- UPDATE


type VectorId
    = VectorId Int


type Msg
    = UserChangedMagnitude VectorId Float
    | UserChangedTheta VectorId Float
    | UserClickedAddNewVector


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserChangedMagnitude id newMagnitude ->
            updatePoint id (\point -> { point | magnitude = newMagnitude }) model

        UserChangedTheta id newTheta ->
            updatePoint id (\point -> { point | theta = newTheta }) model

        UserClickedAddNewVector ->
            model ++ [ startingVector ]


updatePoint : VectorId -> (Polar -> Polar) -> List Polar -> List Polar
updatePoint (VectorId id) function =
    List.indexedMap
        (\possibleId point ->
            if possibleId == id then
                function point

            else
                point
        )



-- VIEW


view : Model -> Html Msg
view model =
    Html.section [] <|
        [ newVectorButton ]
            ++ List.indexedMap vectorForm model
            ++ [ drawingCanvas model ]


newVectorButton : Html Msg
newVectorButton =
    Html.div []
        [ Html.button [ Html.Events.onClick UserClickedAddNewVector ]
            [ Html.text "Add new vector" ]
        ]


vectorForm : Int -> Polar -> Html Msg
vectorForm index point =
    Html.div []
        [ svgDrawing circleViewport
            [ vectorCircle circleViewport point
            , vectorLine circleViewport origin point
            ]
        , magnitudeRange (VectorId index) point
        , thetaRange (VectorId index) point
        ]


magnitudeRange : VectorId -> Polar -> Html Msg
magnitudeRange id point =
    Html.div []
        [ Html.label [] [ Html.text "Magnitude" ]
        , Html.input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.max "10"
            , HtmlAttr.min "1"
            , HtmlAttr.step "1"
            , HtmlAttr.value <| String.fromFloat <| point.magnitude
            , onRangeInput (UserChangedMagnitude id)
            ]
            []
        , Html.text <| String.fromFloat <| point.magnitude
        ]


thetaRange : VectorId -> Polar -> Html Msg
thetaRange id point =
    Html.div []
        [ Html.label [] [ Html.text "Direction" ]
        , Html.input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.max "360"
            , HtmlAttr.min "0"
            , HtmlAttr.step "1"
            , HtmlAttr.value <| String.fromFloat <| point.theta
            , onRangeInput (UserChangedTheta id)
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


drawingCanvas : List Polar -> Html a
drawingCanvas points =
    svgDrawing drawingCanvasViewport <|
        List.map (vectorLine drawingCanvasViewport origin) points



-- VIEWPORT


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


svgDrawing : Viewport -> List (Svg a) -> Html a
svgDrawing viewport children =
    Svg.svg
        [ HtmlAttr.width <| pixelToInt <| viewport.width
        , HtmlAttr.height <| pixelToInt <| viewport.height
        ]
        children


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


origin : Polar
origin =
    Polar 0 0


circleViewport : Viewport
circleViewport =
    { width = Pixel 100
    , height = Pixel 100
    }


drawingCanvasViewport : Viewport
drawingCanvasViewport =
    { width = Pixel 500
    , height = Pixel 500
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


vectorCircle : Viewport -> Polar -> Svg a
vectorCircle viewport point =
    Svg.circle
        [ SvgAttr.cx <| pixelToString <| .x <| toScreenCoords viewport origin
        , SvgAttr.cy <| pixelToString <| .y <| toScreenCoords viewport origin
        , SvgAttr.r <| String.fromFloat <| pixelRadius point
        , SvgAttr.fill "white"
        , SvgAttr.stroke "black"
        ]
        []


vectorLine : Viewport -> Polar -> Polar -> Svg a
vectorLine viewport start end =
    Svg.line
        [ SvgAttr.x1 <| pixelToString <| .x <| toScreenCoords viewport start
        , SvgAttr.y1 <| pixelToString <| .y <| toScreenCoords viewport start
        , SvgAttr.x2 <| pixelToString <| .x <| toScreenCoords viewport end
        , SvgAttr.y2 <| pixelToString <| .y <| toScreenCoords viewport end
        , SvgAttr.stroke "black"
        ]
        []
