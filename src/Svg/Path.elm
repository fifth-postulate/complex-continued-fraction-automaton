module Svg.Path exposing (Command(..), Size(..), Sweep(..), toPath)

import Svg exposing (Attribute)
import Svg.Attributes as Attribute


type Path
    = Sequence (List Command)


toPath : List Command -> Attribute msg
toPath commands =
    commands
        |> Sequence
        |> toString
        |> Attribute.d


type Command
    = MoveTo Float Float
    | RelativeMoveTo Float Float
    | LineTo Float Float
    | RelativeLineTo Float Float
    | ArcTo Float Float Float Size Sweep Float Float
    | RelativeArcTo Float Float Float Size Sweep Float Float
    | ClosePath


type Sweep
    = Apple
    | Pear


type Size
    = Large
    | Small


toString : Path -> String
toString (Sequence commands) =
    commands
        |> List.map commandToString
        |> String.join " "


commandToString : Command -> String
commandToString command =
    case command of
        MoveTo x y ->
            [ x, y ]
                |> List.map String.fromFloat
                |> String.join ","
                |> (++) "M"

        RelativeMoveTo dx dy ->
            [ dx, dy ]
                |> List.map String.fromFloat
                |> String.join ","
                |> (++) "m"

        LineTo x y ->
            [ x, y ]
                |> List.map String.fromFloat
                |> String.join ","
                |> (++) "L"

        RelativeLineTo dx dy ->
            [ dx, dy ]
                |> List.map String.fromFloat
                |> String.join ","
                |> (++) "l"

        ArcTo rx ry axis size sweep x y ->
            [ rx, ry, axis, sizeToFloat size, sweepToFloat sweep, x, y ]
                |> List.map String.fromFloat
                |> String.join ","
                |> (++) "A"

        RelativeArcTo rx ry axis size sweep dx dy ->
            [ rx, ry, axis, sizeToFloat size, sweepToFloat sweep, dx, dy ]
                |> List.map String.fromFloat
                |> String.join ","
                |> (++) "a"

        ClosePath ->
            "Z"


sizeToFloat : Size -> Float
sizeToFloat size =
    case size of
        Large ->
            1

        Small ->
            0


sweepToFloat : Sweep -> Float
sweepToFloat sweep =
    case sweep of
        Apple ->
            1

        Pear ->
            0
