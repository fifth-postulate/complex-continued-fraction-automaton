module Complex exposing (Complex, complex, im, re)


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
