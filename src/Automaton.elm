module Automaton exposing (main)

import Complex exposing (Complex, complex)
import Html exposing (Html)
import Scaffold
import Svg exposing (Svg)
import Svg.Attributes as SvgAttribute
import Svg.Path as Path exposing (Command(..))


main : Html msg
main =
    let
        size =
            640

        window =
            3.0

        division =
            0.2

        gridSize =
            ceiling (window / division)

        viewBox =
            [ -1, -1, 2, 2 ]
                |> List.map ((*) window)
                |> List.map String.fromFloat
                |> String.join " "

        scaffoldConfig =
            { lineWidth = 0.01
            , color = "gray"
            }

        dashArray =
            [ 0.02, 0.03 ]

        regionConfig =
            { lineWidth = 0.02
            , radius = 0.05
            , fillColor = "darkgray"
            , lineColor = "black"
            , dashArray = dashArray
            }

        region =
            Union
                [ TwoDimensional (OpenPoint (PointOpen (complex 0.5 0.5)))
                    [ OpenSegment (Segment (OpenPoint (PointOpen (complex -0.5 0.5))))
                    , ClosedSegment (Segment (ClosedPoint (PointClosed (complex -0.5 -0.5))))
                    , ClosedSegment (Segment (OpenPoint (PointOpen (complex 0.5 -0.5))))
                    , OpenSegment (Segment (OpenPoint (PointOpen (complex 0.5 0.5))))
                    ]
                ]
    in
    Svg.svg
        [ SvgAttribute.width (String.fromInt size)
        , SvgAttribute.height (String.fromInt size)
        , SvgAttribute.viewBox viewBox
        , SvgAttribute.transform "scale(1,-1)"
        ]
        [ Scaffold.grid { scaffoldConfig | color = "lightgray" } window -gridSize gridSize division
        , Scaffold.origin scaffoldConfig window
        , Scaffold.unitCircle scaffoldConfig
        , viewRegion regionConfig region
        ]


dashArrayDescription : List Float -> String
dashArrayDescription pattern =
    pattern
        |> List.map String.fromFloat
        |> String.join " "


type Region
    = Union (List ConnectedRegion)


type ConnectedRegion
    = ZeroDimensional PointClosed
    | OneDimensional Point (List Segment)
    | TwoDimensional Point (List Segment)


type Point
    = OpenPoint PointOpen
    | ClosedPoint PointClosed


toComplex : Point -> Complex
toComplex point =
    case point of
        OpenPoint (PointOpen z) ->
            z

        ClosedPoint (PointClosed z) ->
            z


type PointClosed
    = PointClosed Complex


type PointOpen
    = PointOpen Complex


type Segment
    = OpenSegment Edge
    | ClosedSegment Edge


type Edge
    = Segment Point


endPoint : Segment -> Point
endPoint line =
    case line of
        OpenSegment (Segment p) ->
            p

        ClosedSegment (Segment p) ->
            p


viewRegion : { a | lineWidth : Float, radius : Float, fillColor : String, lineColor : String, dashArray : List Float } -> Region -> Svg msg
viewRegion config (Union connectedRegions) =
    Svg.g
        [ SvgAttribute.strokeWidth (String.fromFloat config.lineWidth)
        , SvgAttribute.fill config.fillColor
        , SvgAttribute.stroke config.lineColor
        ]
        (List.map (viewConnectedRegion config) connectedRegions)


viewConnectedRegion : { a | radius : Float, dashArray : List Float } -> ConnectedRegion -> Svg msg
viewConnectedRegion config region =
    case region of
        ZeroDimensional closedPoint ->
            viewPoint config (ClosedPoint closedPoint)

        OneDimensional start contour ->
            viewJordan config start contour

        TwoDimensional start contour ->
            viewFace config start contour


viewPoint : { a | radius : Float, dashArray : List Float } -> Point -> Svg msg
viewPoint config point =
    let
        z =
            toComplex point

        standard =
            [ SvgAttribute.cx (String.fromFloat (Complex.re z))
            , SvgAttribute.cy (String.fromFloat (Complex.im z))
            , SvgAttribute.r (String.fromFloat config.radius)
            ]

        specific =
            case point of
                OpenPoint (PointOpen _) ->
                    [ SvgAttribute.strokeDasharray (dashArrayDescription config.dashArray) ]

                ClosedPoint (PointClosed _) ->
                    []

        attributes =
            List.concat
                [ standard
                , specific
                ]
    in
    Svg.circle attributes []


viewJordan : { a | radius : Float, dashArray : List Float } -> Point -> List Segment -> Svg msg
viewJordan config start contour =
    let
        points =
            contour
                |> List.map endPoint
                |> (::) start
                |> List.map (viewPoint config)
                |> Svg.g []
    in
    Svg.g []
        [ viewEdge config start contour
        , points
        ]


viewEdge : { a | dashArray : List Float } -> Point -> List Segment -> Svg msg
viewEdge config start contour =
    let
        go : List (Svg msg) -> Point -> List Segment -> Svg msg
        go acc from cs =
            case cs of
                [] ->
                    Svg.g [] (List.reverse acc)

                line :: tail ->
                    let
                        to =
                            endPoint line
                    in
                    go (viewLine config from line :: acc) to tail
    in
    go [] start contour


viewLine : { a | dashArray : List Float } -> Point -> Segment -> Svg msg
viewLine config from line =
    let
        start =
            toComplex from

        finish =
            line
                |> endPoint
                |> toComplex

        standard =
            [ SvgAttribute.x1 (String.fromFloat (Complex.re start))
            , SvgAttribute.y1 (String.fromFloat (Complex.im start))
            , SvgAttribute.x2 (String.fromFloat (Complex.re finish))
            , SvgAttribute.y2 (String.fromFloat (Complex.im finish))
            ]

        special =
            case line of
                OpenSegment _ ->
                    [ SvgAttribute.strokeDasharray (dashArrayDescription config.dashArray) ]

                ClosedSegment _ ->
                    []

        attributes =
            List.concat [ standard, special ]
    in
    Svg.line attributes []


viewFace : { a | dashArray : List Float, radius : Float } -> Point -> List Segment -> Svg msg
viewFace config start contour =
    let
        toCommand : Segment -> Command
        toCommand line =
            line
                |> endPoint
                |> lift LineTo

        commands =
            List.concat
                [ List.singleton (lift MoveTo start)
                , List.map toCommand contour
                , List.singleton ClosePath
                ]
    in
    Svg.g []
        [ Svg.path
            [ Path.toPath commands
            , SvgAttribute.stroke "none"
            ]
            []
        , viewJordan config start contour
        ]


lift : (Float -> Float -> Command) -> Point -> Command
lift toCommand point =
    let
        z =
            toComplex point
    in
    toCommand (Complex.re z) (Complex.im z)
