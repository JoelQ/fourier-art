module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events
import Json.Decode exposing (Decoder)
import Random exposing (Generator)
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
    initialModel
        |> withNoCmd


initialModel : Model
initialModel =
    { vectors = [ baseVector ]
    , isDrawing = False
    , drawPath = []
    , timeElapsed = 0
    }



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
    , drawPath : List Polar
    , timeElapsed : Float
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
    | UserClickedRandomVectors
    | UserToggledDrawing Bool
    | UserClickedReset
    | NewVectorsGenerated (List Vector)
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

        UserClickedRandomVectors ->
            ( model, Random.generate NewVectorsGenerated vectorListGenerator )

        NewVectorsGenerated newVectors ->
            { model | vectors = newVectors }
                |> withNoCmd

        UserClickedReset ->
            initialModel
                |> withNoCmd

        UserToggledDrawing newDrawingStatus ->
            { model | isDrawing = newDrawingStatus }
                |> withNoCmd

        Tick delta ->
            model
                |> stepModel delta
                |> withNoCmd


stepModel : Float -> Model -> Model
stepModel delta model =
    { model
        | vectors = List.map (rotateForDelta delta) model.vectors
        , drawPath = newDrawPath delta model
        , timeElapsed = newTimeElapsed delta model.timeElapsed
    }


newDrawPath : Float -> Model -> List Polar
newDrawPath delta model =
    if newTimeElapsed delta model.timeElapsed == 0 then
        []

    else
        (List.foldl addPolar origin <| List.map vectorToPolar model.vectors) :: model.drawPath


newTimeElapsed : Float -> Float -> Float
newTimeElapsed delta timeElapsed =
    if timeElapsed + delta > (1000 * speedMultiplier) then
        0

    else
        timeElapsed + delta


addNextVectorPair : Model -> Model
addNextVectorPair model =
    { model | vectors = model.vectors ++ nextVectors model.vectors }


speedMultiplier : Float
speedMultiplier =
    10


degreesPerMillisecond : Float
degreesPerMillisecond =
    0.36 / speedMultiplier


rotateForDelta : Float -> Vector -> Vector
rotateForDelta delta vector =
    let
        (Hertz hertz) =
            vector.frequency
    in
    { vector | theta = clampDegrees <| vector.theta + (delta * degreesPerMillisecond * hertz) }


clampDegrees : Float -> Float
clampDegrees n =
    if n > 360 then
        n - 360

    else if n < 0 then
        n + 360

    else
        n


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
    Element.layout [] <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.paddingXY 15 0
            ]
            [ navBar model
            , content model
            ]


navBar : Model -> Element Msg
navBar model =
    Element.row
        [ Element.spacing 5
        , Element.height (Element.px 75)
        , Element.width Element.fill
        ]
        [ newVectorButton
        , randomVectorsButton
        , resetButton
        , toggleDrawingButton model.isDrawing
        ]


content : Model -> Element Msg
content model =
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ vectorDials model.vectors
        , drawingCanvas model
        ]


vectorDials : List Vector -> Element Msg
vectorDials vectors =
    Element.column
        [ Element.width (Element.px 200)
        , Element.height (Element.px 700)
        , Element.scrollbarY
        ]
    <|
        List.indexedMap vectorForm vectors


toggleDrawingButton : Bool -> Element Msg
toggleDrawingButton isDrawing =
    case isDrawing of
        True ->
            actionButton "Stop drawing" (UserToggledDrawing False)

        False ->
            actionButton "Start drawing" (UserToggledDrawing True)


newVectorButton : Element Msg
newVectorButton =
    actionButton "Add new vector pair" UserClickedAddNewVector


randomVectorsButton : Element Msg
randomVectorsButton =
    actionButton "Generate random vectors" UserClickedRandomVectors


resetButton : Element Msg
resetButton =
    actionButton "Reset" UserClickedReset


vectorForm : Int -> Vector -> Element Msg
vectorForm index vector =
    Element.column [ Element.spacing 10 ]
        [ svgDrawing circleViewport
            [ vectorCircle circleViewport <| vectorToPolar vector
            , vectorLine circleViewport origin <| vectorToPolar vector
            ]
        , magnitudeRange (VectorId index) vector
        , thetaRange (VectorId index) vector
        , frequencyDisplay vector.frequency
        ]


frequencyDisplay : Hertz -> Element a
frequencyDisplay (Hertz h) =
    Element.row [ Element.spacing 10 ]
        [ label "Frequency"
        , Element.text <| String.fromFloat h ++ " Hz"
        ]


magnitudeRange : VectorId -> Vector -> Element Msg
magnitudeRange id vector =
    slider
        { onChange = UserChangedMagnitude id
        , labelText =
            "Magnitude: ("
                ++ (String.fromInt <| round <| vector.magnitude)
                ++ ")"
        , min = 0
        , max = 10
        , step = 1
        , value = vector.magnitude
        }


thetaRange : VectorId -> Vector -> Element Msg
thetaRange id vector =
    slider
        { onChange = UserChangedTheta id
        , labelText =
            "Theta: ("
                ++ (String.fromInt <| round <| vector.theta)
                ++ ")"
        , min = 0
        , max = 360
        , step = 1
        , value = vector.theta
        }


drawingCanvas : Model -> Element a
drawingCanvas model =
    svgDrawing drawingCanvasViewport <|
        (canvasLineSegments <| List.map vectorToPolar model.vectors)
            ++ [ renderDrawPath drawingCanvasViewport model.drawPath ]


canvasLineSegments : List Polar -> List (Svg a)
canvasLineSegments points =
    points
        |> partialSums
        |> consecutivePairs
        |> List.map (\( start, end ) -> vectorLine drawingCanvasViewport start end)


consecutivePairs : List a -> List ( a, a )
consecutivePairs items =
    List.map2 Tuple.pair items (List.drop 1 items)


svgPoint : ScreenCoord -> String
svgPoint { x, y } =
    pixelToString x ++ "," ++ pixelToString y


svgPathString : List ScreenCoord -> String
svgPathString points =
    String.join " " (List.map svgPoint points)


renderDrawPath : Viewport -> List Polar -> Svg a
renderDrawPath viewport points =
    Svg.polyline
        [ SvgAttr.points (svgPathString <| List.map (toScreenCoords viewport) points)
        , SvgAttr.stroke "red"
        , SvgAttr.fill "none"
        , SvgAttr.strokeWidth "2px"
        ]
        []



-- GENERIC VIEW HELPERS


slider :
    { min : Float
    , max : Float
    , step : Float
    , value : Float
    , labelText : String
    , onChange : Float -> msg
    }
    -> Element msg
slider { min, max, step, value, labelText, onChange } =
    Element.Input.slider
        [ Element.width (Element.px 100)
        , Element.height (Element.px 20)
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Element.Background.color (Element.rgb 0 0 0)
                , Element.Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = onChange
        , label = Element.Input.labelAbove [] <| label labelText
        , min = min
        , max = max
        , step = Just step
        , value = value
        , thumb = Element.Input.defaultThumb
        }


label : String -> Element a
label text =
    Element.el [ Element.Font.semiBold ] <| Element.text text


actionButton : String -> a -> Element a
actionButton labelText msg =
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.7 0.7 0.7)
        , Element.Border.rounded 12
        , Element.padding 3
        , Element.Border.color (Element.rgb 0 0 0)
        , Element.Border.width 1
        ]
        { label = Element.text labelText
        , onPress = Just msg
        }



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


svgDrawing : Viewport -> List (Svg a) -> Element a
svgDrawing viewport children =
    Element.el [] <|
        Element.html <|
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



-- RANDOM


vectorListGenerator : Generator (List Vector)
vectorListGenerator =
    Random.int 1 20
        |> Random.andThen
            (\n ->
                List.range 1 n
                    |> List.map vectorPairGenerator
                    |> List.foldl (Random.map2 (::)) (Random.constant [])
                    |> Random.map (List.foldl (\( pos, neg ) list -> pos :: neg :: list) [])
            )


vectorPairGenerator : Int -> Generator ( Vector, Vector )
vectorPairGenerator frequency =
    Random.map2 Tuple.pair
        (vectorGenerator frequency)
        (vectorGenerator <| negate frequency)


vectorGenerator : Int -> Generator Vector
vectorGenerator frequency =
    Random.map2
        (\magnitude theta ->
            { magnitude = magnitude
            , theta = theta
            , frequency = Hertz (toFloat frequency)
            }
        )
        magnitudeGenerator
        thetaGenerator


magnitudeGenerator : Generator Float
magnitudeGenerator =
    Random.int 1 10
        |> Random.map toFloat


thetaGenerator : Generator Float
thetaGenerator =
    Random.int 0 360
        |> Random.map toFloat
