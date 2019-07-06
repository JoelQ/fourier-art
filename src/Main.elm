module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Json.Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    { vectors = [ baseVector ]
    , isDrawing = False
    }
        |> withNoCmd



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isDrawing then
        Browser.Events.onAnimationFrameDelta Tick

    else
        Sub.none



-- MODEL


type alias Model =
    { vectors : List Vector
    , isDrawing : Bool
    }


type alias Polar =
    { magnitude : Float
    , theta : Float
    }


type alias Vector =
    { magnitude : Float
    , theta : Float
    , frequency : Hertz
    }


nextVectors : List Vector -> List Vector
nextVectors vectors =
    let
        max =
            vectorMaxFrequency vectors

        positive =
            { baseVector | frequency = incrementFrequency max }

        negative =
            { baseVector | frequency = negateFrequency <| incrementFrequency max }
    in
    [ positive, negative ]


type Hertz
    = Hertz Float


vectorToPolar : Vector -> Polar
vectorToPolar vector =
    { magnitude = vector.magnitude
    , theta = vector.theta
    }


baseVector : Vector
baseVector =
    { magnitude = 5
    , theta = 0
    , frequency = Hertz 0
    }


maxFrequency : Hertz -> Hertz -> Hertz
maxFrequency (Hertz h1) (Hertz h2) =
    if h1 > h2 then
        Hertz h1

    else
        Hertz h2


vectorMaxFrequency : List Vector -> Hertz
vectorMaxFrequency vectors =
    List.foldl (\vector max -> maxFrequency max vector.frequency) (Hertz 0) vectors


incrementFrequency : Hertz -> Hertz
incrementFrequency (Hertz h) =
    Hertz (h + 1)


negateFrequency : Hertz -> Hertz
negateFrequency (Hertz h) =
    Hertz (negate h)


addPolar : Polar -> Polar -> Polar
addPolar p1 p2 =
    let
        ( p1x, p1y ) =
            fromPolar ( p1.magnitude, degrees p1.theta )

        ( p2x, p2y ) =
            fromPolar ( p2.magnitude, degrees p2.theta )

        ( sumMag, sumTheta ) =
            toPolar ( p1x + p2x, p1y + p2y )
    in
    { magnitude = sumMag
    , theta = radToDegrees sumTheta
    }


radToDegrees : Float -> Float
radToDegrees rad =
    rad * degreesPerRad


degreesPerRad : Float
degreesPerRad =
    360 / (2 * pi)


partialSums : List Polar -> List Polar
partialSums points =
    scanl addPolar origin points


type alias ScanAcc a =
    { rest : List a, last : a }


scanStep : (a -> b -> b) -> a -> ScanAcc b -> ScanAcc b
scanStep function point ({ rest, last } as acc) =
    { acc | last = function point last, rest = last :: rest }


accToList : ScanAcc a -> List a
accToList { rest, last } =
    last :: rest


scanl : (a -> b -> b) -> b -> List a -> List b
scanl function initial items =
    items
        |> List.foldl (scanStep function) { rest = [], last = initial }
        |> accToList
        |> List.reverse



-- UPDATE


type VectorId
    = VectorId Int


type Msg
    = UserChangedMagnitude VectorId Float
    | UserChangedTheta VectorId Float
    | UserClickedAddNewVector
    | UserToggledDrawing Bool
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserChangedMagnitude id newMagnitude ->
            model
                |> updatePoint id (\point -> { point | magnitude = newMagnitude })
                |> withNoCmd

        UserChangedTheta id newTheta ->
            model
                |> updatePoint id (\point -> { point | theta = newTheta })
                |> withNoCmd

        UserClickedAddNewVector ->
            model
                |> addNextVectorPair
                |> withNoCmd

        UserToggledDrawing newDrawingStatus ->
            { model | isDrawing = newDrawingStatus }
                |> withNoCmd

        Tick delta ->
            { model | vectors = List.map (rotateForDelta delta) model.vectors }
                |> withNoCmd


addNextVectorPair : Model -> Model
addNextVectorPair model =
    { model | vectors = model.vectors ++ nextVectors model.vectors }


degreesPerMillisecond : Float
degreesPerMillisecond =
    0.36


rotateForDelta : Float -> Vector -> Vector
rotateForDelta delta vector =
    let
        (Hertz hertz) =
            vector.frequency
    in
    { vector | theta = vector.theta + (delta * degreesPerMillisecond * hertz) }


withNoCmd : a -> ( a, Cmd Msg )
withNoCmd val =
    ( val, Cmd.none )


updatePoint : VectorId -> (Vector -> Vector) -> Model -> Model
updatePoint (VectorId id) function model =
    { model
        | vectors =
            List.indexedMap
                (\possibleId vector ->
                    if possibleId == id then
                        function vector

                    else
                        vector
                )
                model.vectors
    }



-- VIEW


view : Model -> Html Msg
view model =
    Html.section [] <|
        [ newVectorButton ]
            ++ List.indexedMap vectorForm model.vectors
            ++ [ toggleDrawingButton model.isDrawing
               , drawingCanvas <| List.map vectorToPolar model.vectors
               ]


toggleDrawingButton : Bool -> Html Msg
toggleDrawingButton isDrawing =
    case isDrawing of
        True ->
            Html.div []
                [ Html.button [ Html.Events.onClick (UserToggledDrawing False) ]
                    [ Html.text "Stop drawing" ]
                ]

        False ->
            Html.div []
                [ Html.button [ Html.Events.onClick (UserToggledDrawing True) ]
                    [ Html.text "Start drawing" ]
                ]


newVectorButton : Html Msg
newVectorButton =
    Html.div []
        [ Html.button [ Html.Events.onClick UserClickedAddNewVector ]
            [ Html.text "Add new vector pair" ]
        ]


vectorForm : Int -> Vector -> Html Msg
vectorForm index vector =
    Html.div []
        [ svgDrawing circleViewport
            [ vectorCircle circleViewport <| vectorToPolar vector
            , vectorLine circleViewport origin <| vectorToPolar vector
            ]
        , magnitudeRange (VectorId index) vector
        , thetaRange (VectorId index) vector
        , frequencyDisplay vector.frequency
        ]


frequencyDisplay : Hertz -> Html a
frequencyDisplay (Hertz h) =
    Html.dl
        []
        [ Html.dt [] [ Html.text "Frequency" ]
        , Html.dd [] [ Html.text <| String.fromFloat h ++ " Hz" ]
        ]


magnitudeRange : VectorId -> Vector -> Html Msg
magnitudeRange id vector =
    Html.div []
        [ Html.label [] [ Html.text "Magnitude" ]
        , Html.input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.max "10"
            , HtmlAttr.min "1"
            , HtmlAttr.step "1"
            , HtmlAttr.value <| String.fromFloat <| vector.magnitude
            , onRangeInput (UserChangedMagnitude id)
            ]
            []
        , Html.text <| String.fromFloat <| vector.magnitude
        ]


thetaRange : VectorId -> Vector -> Html Msg
thetaRange id vector =
    Html.div []
        [ Html.label [] [ Html.text "Direction" ]
        , Html.input
            [ HtmlAttr.type_ "range"
            , HtmlAttr.max "360"
            , HtmlAttr.min "0"
            , HtmlAttr.step "1"
            , HtmlAttr.value <| String.fromFloat <| vector.theta
            , onRangeInput (UserChangedTheta id)
            ]
            []
        , Html.text <| String.fromFloat <| vector.theta
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
    svgDrawing drawingCanvasViewport <| canvasLineSegments points


canvasLineSegments : List Polar -> List (Svg a)
canvasLineSegments points =
    points
        |> partialSums
        |> consecutivePairs
        |> List.map (\( start, end ) -> vectorLine drawingCanvasViewport start end)


consecutivePairs : List a -> List ( a, a )
consecutivePairs items =
    List.map2 Tuple.pair items (List.drop 1 items)



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
