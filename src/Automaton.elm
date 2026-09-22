module Automaton exposing (main)

import Complex exposing (Complex, complex)
import Html exposing (Html)
import Scaffold
import Svg exposing (Svg)
import Svg.Attributes as SvgAttribute
import Svg.Path as Path exposing (Command(..), Size(..), Sweep(..))


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

        original =
            Union
                [ TwoDimensional (OpenPoint (PointOpen (complex 0.5 0.5)))
                    [ OpenSegment (Segment (OpenPoint (PointOpen (complex -0.5 0.5))))
                    , ClosedSegment (Circle -0.75 Small (ClosedPoint (PointClosed (complex -0.5 -0.5))))
                    , ClosedSegment (Segment (OpenPoint (PointOpen (complex 0.5 -0.5))))
                    , OpenSegment (Circle 0.75 Small (OpenPoint (PointOpen (complex 0.5 0.5))))
                    ]
                ]

        transformed =
            regionInverse original
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
        , viewRegion regionConfig original
        , viewRegion regionConfig transformed
        ]


dashArrayDescription : List Float -> String
dashArrayDescription pattern =
    pattern
        |> List.map String.fromFloat
        |> String.join " "


type Region
    = Union (List ConnectedRegion)


regionInverse : Region -> Region
regionInverse (Union connectedRegions) =
    connectedRegions
        |> List.map connectedRegionInverse
        |> Union


type ConnectedRegion
    = ZeroDimensional PointClosed
    | OneDimensional Point (List Segment)
    | TwoDimensional Point (List Segment)


connectedRegionInverse : ConnectedRegion -> ConnectedRegion
connectedRegionInverse connectedRegion =
    let
        -- TODO determine right orientation
        toPointClosed : Point -> PointClosed
        toPointClosed p =
            p
                |> toComplex
                |> PointClosed
    in
    case connectedRegion of
        ZeroDimensional point ->
            point
                |> ClosedPoint
                |> pointInverse
                |> toPointClosed
                |> ZeroDimensional

        OneDimensional start contour ->
            contour
                |> List.map segmentInverse
                |> OneDimensional (pointInverse start)

        TwoDimensional start contour ->
            contour
                |> List.map segmentInverse
                |> TwoDimensional (pointInverse start)


type Point
    = OpenPoint PointOpen
    | ClosedPoint PointClosed


pointInverse : Point -> Point
pointInverse point =
    case point of
        OpenPoint (PointOpen z) ->
            z
                |> Complex.inverse
                |> PointOpen
                |> OpenPoint

        ClosedPoint (PointClosed z) ->
            z
                |> Complex.inverse
                |> PointOpen
                |> OpenPoint


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


segmentInverse : Segment -> Segment
segmentInverse segment =
    case segment of
        OpenSegment edge ->
            OpenSegment (edgeInverse edge)

        ClosedSegment edge ->
            ClosedSegment (edgeInverse edge)


toEdge : Segment -> Edge
toEdge segment =
    case segment of
        OpenSegment edge ->
            edge

        ClosedSegment edge ->
            edge


type Edge
    = Segment Point
    | Circle Float Size Point


edgeInverse : Edge -> Edge
edgeInverse edge =
    case edge of
        Segment point ->
            Segment (pointInverse point)

        Circle radius size point ->
            let
                r =
                    radius

                -- TODO calculate correct radius
            in
            Circle r size (pointInverse point)


endPoint : Segment -> Point
endPoint line =
    case line of
        OpenSegment edge ->
            edgeEndPoint edge

        ClosedSegment edge ->
            edgeEndPoint edge


edgeEndPoint : Edge -> Point
edgeEndPoint edge =
    case edge of
        Segment p ->
            p

        Circle _ _ p ->
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
            viewContour config start contour

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


viewContour : { a | radius : Float, dashArray : List Float } -> Point -> List Segment -> Svg msg
viewContour config start contour =
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

                segment :: tail ->
                    let
                        to =
                            endPoint segment
                    in
                    go (viewSegment config from segment :: acc) to tail
    in
    go [] start contour


viewSegment : { a | dashArray : List Float } -> Point -> Segment -> Svg msg
viewSegment config from segment =
    let
        dashed =
            case segment of
                OpenSegment _ ->
                    [ SvgAttribute.strokeDasharray (dashArrayDescription config.dashArray) ]

                ClosedSegment _ ->
                    []

        path =
            [ lift MoveTo from
            , toCommand (toEdge segment)
            ]
                |> Path.toPath
                |> List.singleton

        attributes =
            List.concat [ path, dashed, [ SvgAttribute.fill "none" ] ]
    in
    Svg.path attributes []


toCommand : Edge -> Command
toCommand edge =
    case edge of
        Segment point ->
            lift LineTo point

        Circle radius size point ->
            let
                r =
                    abs radius

                sweep =
                    if radius >= 0.0 then
                        Apple

                    else
                        Pear
            in
            lift (ArcTo r r 0 size sweep) point


viewFace : { a | dashArray : List Float, radius : Float } -> Point -> List Segment -> Svg msg
viewFace config start contour =
    let
        commands =
            List.concat
                [ List.singleton (lift MoveTo start)
                , List.map (toEdge >> toCommand) contour
                , List.singleton ClosePath
                ]
    in
    Svg.g []
        [ Svg.path
            [ Path.toPath commands
            , SvgAttribute.stroke "none"
            ]
            []
        , viewContour config start contour
        ]


lift : (Float -> Float -> Command) -> Point -> Command
lift command point =
    let
        z =
            toComplex point
    in
    command (Complex.re z) (Complex.im z)
