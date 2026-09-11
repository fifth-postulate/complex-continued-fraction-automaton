module Svg.Path exposing (Command(..), toPath)

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
    | ClosePath


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

        ClosePath ->
            "Z"
