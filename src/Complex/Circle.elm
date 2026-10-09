module Complex.Circle exposing (Circle, circle, inverse, origin, radius)

import Complex exposing (Complex)


{-| A _generalized_ Complex Circle.

A Circle is defined by three parameters A, B, D.
A and D are real and B is complex.

The circle is defined as the complex points w such that

Aw~w + Bw + ~B~w + D = 0

where ~z is the complex conjugate of z, and B~B - AD >= 0.

-}
type Circle
    = GCircle Float Complex Float


circle : Complex -> Float -> Circle
circle o r =
    let
        a =
            1

        b =
            o
                |> Complex.negate
                |> Complex.conjugate

        d =
            r * r - Complex.negate (Complex.norm b)
    in
    gcircle a b d


gcircle : Float -> Complex -> Float -> Circle
gcircle a b d =
    GCircle a b d


inverse : Circle -> Circle
inverse GCircle a b d =
    GCircle d (Complex.conjugate b) a


epsilon : Float
epsilon =
    0.000000001


radius : Circle -> Maybe Float
radius (GCircle a b d) =
    if abs a < epsilon then
        Nothing

    else
        sqrt (Complex.norm b - a * d)


origin : Circle -> Maybe Complex
origin (GCircle a b d) =
    if abs a < epsilon then
        Nothing

    else
        b
            |> Complex.conjugate
            |> Complex.negate
            |> Complex.scale (1 / a)
