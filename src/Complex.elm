module Complex exposing (Complex, complex, conjugate, im, inverse, negate, norm, re)


type Complex
    = Cartesian { real : Float, imaginary : Float }


complex : Float -> Float -> Complex
complex real imaginary =
    Cartesian { real = real, imaginary = imaginary }


re : Complex -> Float
re (Cartesian { real }) =
    real


im : Complex -> Float
im (Cartesian { imaginary }) =
    imaginary


norm : Complex -> Float
norm (Cartesian { real, imaginary }) =
    real * real + imaginary * imaginary


inverse : Complex -> Complex
inverse z =
    let
        n =
            norm z
    in
    z
        |> scale (1 / n)
        |> conjugate


conjugate : Complex -> Complex
conjugate (Cartesian { real, imaginary }) =
    Cartesian { real = real, imaginary = -imaginary }


negate : Complex -> Complex
negate (Cartesian { real, imaginary }) =
    Cartesian { real = -real, imaginary = -imaginary }


scale : Float -> Complex -> Complex
scale s (Cartesian { real, imaginary }) =
    Cartesian { real = s * real, imaginary = s * imaginary }
