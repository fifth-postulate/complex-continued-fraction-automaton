module Scaffold exposing (grid, origin, unitCircle)

import Svg exposing (Svg)
import Svg.Attributes as Attribute


grid : { lineWidth : Float, color : String } -> Float -> Int -> Int -> Float -> Svg msg
grid config window minimum maximum distance =
    let
        marks =
            List.range minimum maximum
                |> List.map toFloat
                |> List.map ((*) distance)

        horizontalLine mark =
            Svg.line
                [ Attribute.x1 (String.fromFloat mark)
                , Attribute.y1 (String.fromFloat -window)
                , Attribute.x2 (String.fromFloat mark)
                , Attribute.y2 (String.fromFloat window)
                ]
                []

        verticalLine mark =
            Svg.line
                [ Attribute.x1 (String.fromFloat -window)
                , Attribute.y1 (String.fromFloat mark)
                , Attribute.x2 (String.fromFloat window)
                , Attribute.y2 (String.fromFloat mark)
                ]
                []
    in
    Svg.g
        [ Attribute.fill "none"
        , Attribute.stroke config.color
        , Attribute.strokeWidth (String.fromFloat config.lineWidth)
        ]
        (List.concat
            [ List.map horizontalLine marks, List.map verticalLine marks ]
        )


origin : { a | lineWidth : Float, color : String } -> Float -> Svg msg
origin config window =
    Svg.g
        [ Attribute.fill "none"
        , Attribute.stroke config.color
        , Attribute.strokeWidth (String.fromFloat config.lineWidth)
        ]
        [ Svg.line
            [ Attribute.x1 "0"
            , Attribute.y1 (String.fromFloat -window)
            , Attribute.x2 "0"
            , Attribute.y2 (String.fromFloat window)
            ]
            []
        , Svg.line
            [ Attribute.x1 (String.fromFloat -window)
            , Attribute.y1 "0"
            , Attribute.x2 (String.fromFloat window)
            , Attribute.y2 "0"
            ]
            []
        ]


unitCircle : { a | lineWidth : Float, color : String } -> Svg msg
unitCircle config =
    Svg.g
        [ Attribute.fill "none"
        , Attribute.stroke config.color
        , Attribute.strokeWidth (String.fromFloat config.lineWidth)
        ]
        [ Svg.circle
            [ Attribute.x "0"
            , Attribute.y "0"
            , Attribute.r "1"
            ]
            []
        ]
