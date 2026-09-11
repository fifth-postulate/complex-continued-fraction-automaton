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

        region =
            Union
                [ TwoDimensional (Open (OpenPoint (complex 0.5 0.5)))
                    [ OpenLine (Segment (Open (OpenPoint (complex -0.5 0.5))))
                    , ClosedLine (Segment (Closed (ClosedPoint (complex -0.5 -0.5))))
                    , ClosedLine (Segment (Open (OpenPoint (complex 0.5 -0.5))))
                    , OpenLine (Segment (Open (OpenPoint (complex 0.5 0.5))))
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
        , viewRegion
            { lineWidth = 0.02
            , radius = 0.05
            , fillColor = "darkgray"
            , lineColor = "black"
            , dashArray = dashArray
            }
            region
        ]


dashArrayDescription : List Float -> String
dashArrayDescription pattern =
    pattern
        |> List.map String.fromFloat
        |> String.join " "


type Region
    = Union (List ConnectedRegion)


type ConnectedRegion
    = ZeroDimensional ClosedPoint
    | OneDimensional Point (List Line)
    | TwoDimensional Point (List Line)


type Point
    = Open OpenPoint
    | Closed ClosedPoint


toComplex : Point -> Complex
toComplex point =
    case point of
        Open (OpenPoint z) ->
            z

        Closed (ClosedPoint z) ->
            z


type ClosedPoint
    = ClosedPoint Complex


type OpenPoint
    = OpenPoint Complex


type Line
    = OpenLine Edge
    | ClosedLine Edge


type Edge
    = Segment Point


endPoint : Line -> Point
endPoint line =
    case line of
        OpenLine (Segment p) ->
            p

        ClosedLine (Segment p) ->
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
            viewPoint config (Closed closedPoint)

        OneDimensional start curve ->
            viewJordan config start curve

        TwoDimensional start curve ->
            viewFace config start curve


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
                Open (OpenPoint _) ->
                    [ SvgAttribute.strokeDasharray (dashArrayDescription config.dashArray) ]

                Closed (ClosedPoint _) ->
                    []

        attributes =
            List.concat
                [ standard
                , specific
                ]
    in
    Svg.circle attributes []


viewJordan : { a | radius : Float, dashArray : List Float } -> Point -> List Line -> Svg msg
viewJordan config start curve =
    let
        points =
            curve
                |> List.map endPoint
                |> (::) start
                |> List.map (viewPoint config)
                |> Svg.g []
    in
    Svg.g []
        [ viewEdge config start curve
        , points
        ]


viewEdge : { a | dashArray : List Float } -> Point -> List Line -> Svg msg
viewEdge config start curve =
    let
        go : List (Svg msg) -> Point -> List Line -> Svg msg
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
    go [] start curve


viewLine : { a | dashArray : List Float } -> Point -> Line -> Svg msg
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
                OpenLine _ ->
                    [ SvgAttribute.strokeDasharray (dashArrayDescription config.dashArray) ]

                ClosedLine _ ->
                    []

        attributes =
            List.concat [ standard, special ]
    in
    Svg.line attributes []


viewFace : { a | dashArray : List Float, radius : Float } -> Point -> List Line -> Svg msg
viewFace config start curve =
    let
        toCommand : Line -> Command
        toCommand line =
            line
                |> endPoint
                |> lift LineTo

        commands =
            List.concat
                [ List.singleton (lift MoveTo start)
                , List.map toCommand curve
                , List.singleton ClosePath
                ]
    in
    Svg.g []
        [ Svg.path
            [ Path.toPath commands
            , SvgAttribute.stroke "none"
            ]
            []
        , viewJordan config start curve
        ]


lift : (Float -> Float -> Command) -> Point -> Command
lift toCommand point =
    let
        z =
            toComplex point
    in
    toCommand (Complex.re z) (Complex.im z)
