module Complex exposing (Complex, complex, im, inverse, re)


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


inverse : Complex -> Complex
inverse (Cartesian { real, imaginary }) =
    let
        n =
            real * real + imaginary * imaginary

        r =
            real / n

        i =
            -imaginary / n
    in
    Cartesian { real = r, imaginary = i }
