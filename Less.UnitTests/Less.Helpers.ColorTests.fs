module Less.Helpers.ColorTests

open Xunit
open Less.Helpers
open FsUnit.Xunit

let green, greenHSL = Color.Create(0.000, 0.500, 0.000), (120.0, 1.000, 0.250)
let navy, navyHSL = Color.Create(0.211, 0.149, 0.597), (248.3, 0.601, 0.373)
let magenta, magentaHSL = Color.Create(0.704, 0.187, 0.897), (283.7, 0.775, 0.542)

let hsvColors = hsv (248.3, 0.750, 0.597), hsv (248.3035714, 0.7504187605, 0.597)

let verifyHsl (color:Color) (h, s, l) =
    let h', s', l' = color.Hsl
    h' |> should (equalWithin 0.01) h
    s' |> should (equalWithin 0.01) s
    l' |> should (equalWithin 0.01) l

[<Fact>]
let ``Magenta RGB to HSL`` () =
    verifyHsl magenta magentaHSL

[<Fact>]
let ``Green RGB to HSL`` () =
    verifyHsl green greenHSL

[<Fact>]
let ``Navy RGB to HSL`` () =
    verifyHsl navy navyHSL