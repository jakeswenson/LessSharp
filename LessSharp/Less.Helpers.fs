module Less.Helpers

open System

module Utilities = 
    let fst (a, _, _) = a
    let snd (_, b, _) = b
    let thrd (_, _, c) = c
    let quartet d (a,b,c) = (a, b, c, d) 

    let inline validate f = 
        if f < 0.0 || f > 1.0 then failwith "Invalid color value provided!"
        else f

    let inline clamp f =  
        if f < 0.0  then 0.0
        elif f > 1.0 then 1.0
        else f

    let clampAngle a = 
        let h = a % 360.0
        if h < 0.0 then 360.0 + h else h

    let inline samplePerc f = float f / 255.0

open Utilities

module ColorComponents =
    let computeHMmChroma R G B = 
        let max a b c = max a (max b c)
        let min a b c = min a (min b c)
        let M = max R G B
        let m = min R G B
        let chroma = M - m
        let h' = 
            if chroma = 0.0 then nan
            elif M = R then ((G-B) / chroma) % 6.0
            elif M = G then ((B-R) / chroma) + 2.0
            elif M = B then ((R-G) / chroma) + 4.0
            else nan
        let H = 
            let h = if Double.IsNaN(h') then 0.0 else h' * 60.0
            clampAngle h
    
        H, M, m, chroma

    let toHsl (R, G, B) = 
        let H, M, m, chroma = computeHMmChroma R G B
        let L = (M+m) / 2.0
        let Shsl = if chroma = 0.0 then 0.0 else chroma / (1.0 - abs(2.0 * L - 1.0))
        H, Shsl, L

    let toHsv (R, G, B) = 
        let H, M, m, chroma = computeHMmChroma R G B
        let V = M
        let Shsv = if chroma = 0.0 then 0.0 else chroma / V
        H, Shsv, V

    let toHsla rgb alpha = toHsl rgb |> quartet alpha
    let toHsva rgb alpha = toHsv rgb |> quartet alpha

    let rgbBase h h' chroma x = 
        if Double.IsNaN(h) then (0.0, 0.0, 0.0)
        elif h' >= 0.0 && h' < 1.0 then (chroma, x, 0.0)
        elif h' >= 1.0 && h' < 2.0 then (x, chroma, 0.0)
        elif h' >= 2.0 && h' < 3.0 then (0.0, chroma, x)
        elif h' >= 3.0 && h' < 4.0 then (0.0, x, chroma)
        elif h' >= 4.0 && h' < 5.0 then (x, 0.0, chroma)
        elif h' >= 5.0 && h' < 6.0 then (chroma, 0.0, x)
        else (0.0, 0.0, 0.0)

type Color = 
    {
        R: float;
        G: float;
        B: float;
        Alpha: float;
    }
    static member private byte v = byte (v * float Byte.MaxValue)

    override this.ToString() = 
        match this.Alpha with
        | alpha when alpha < 1.0 -> sprintf "rgba(%d, %d, %d, %O)" (Color.byte this.R) (Color.byte this.G) (Color.byte this.B) alpha
        | _ -> sprintf "rgb(%d, %d, %d)" (Color.byte this.R) (Color.byte this.G) (Color.byte this.B)
    
    member this.RGB = sprintf "#%2x%2x%2x" (Color.byte this.R) (Color.byte this.G) (Color.byte this.B)
    member this.ARGB = sprintf "#%2X%2X%2X%2X" (Color.byte this.R) (Color.byte this.G) (Color.byte this.B) (Color.byte this.Alpha)
    
    member this.Hsv = ColorComponents.toHsv (this.R, this.G, this.B)
    member this.Hsl = ColorComponents.toHsl (this.R, this.G, this.B)
    member this.Hsva = ColorComponents.toHsva (this.R, this.G, this.B) this.Alpha
    member this.Hsla = ColorComponents.toHsla (this.R, this.G, this.B) this.Alpha
    
    member this.SetAlpha alpha = { this with Alpha = clamp alpha }
    
    static member Zero = { R = 0.0; G = 0.0; B = 0.0; Alpha = 1.0 }
    static member Force (r:int,g:int,b:int) = { R = samplePerc r; G = samplePerc g; B = samplePerc b; Alpha = 1.0 }
    static member Force (r:int,g:int,b:int, a) = { Color.Force(r, g, b) with Alpha = a }
    static member Create(r:byte, g:byte, b:byte) = Color.Force(int r, int g, int b)
    static member Create(r:float, g:float, b:float) = { R = validate r; G = validate g; B = validate b; Alpha = 1.0 }
    static member Create(r:float, g:float, b:float, a) = { Color.Create(r, g, b) with Alpha = a }

let hsv (h, s, v) = 
    let h, s, v = clampAngle h, clamp s, clamp v
    let chroma = v * s
    let h' = h / 60.0
    let x = chroma * (1.0 - abs(h' % 2.0 - 1.0))
    let r1, g1, b1 = ColorComponents.rgbBase h h' chroma x
    let m = v - chroma
    Color.Create(r1 + m, g1 + m, b1 + m)

let hsl (h, s, l) = 
    let h, s, l = clampAngle h, clamp s, clamp l
    let chroma = (1.0 - abs(2.0 * l - 1.0)) * s
    let h' = h / 60.0
    let x = chroma * (1.0 - abs(h' % 2.0 - 1.0))
    let r1, g1, b1 = ColorComponents.rgbBase h h' chroma x
    let m = l - 0.5 * chroma
    Color.Create(r1 + m, g1 + m, b1 + m)

let hsva (h, s, v, a) = hsv(h, s, v).SetAlpha (clamp a)
let hsla (h, s, l, a) = hsl(h, s, l).SetAlpha (clamp a)

let hue (color : Color) = color.Hsl |> fst
let saturation (color : Color) = color.Hsl |> snd
let lightness (color : Color) = color.Hsl |> thrd
let brightness (color : Color) = color.Hsv |> thrd

let saturate (color : Color) amount = 
    let h, s, l, alpha = color.Hsla
    hsla(h, clamp(s + amount), l, alpha)
    
let desaturate (color : Color) amount = 
    let h, s, l, alpha = color.Hsla
    hsla(h, clamp(s - amount), l, alpha)

let lighten (color : Color) amount = 
    let h, s, l, alpha = color.Hsla
    hsla(h, s, clamp(l + amount), alpha)
    
let darken (color : Color) amount = 
    let h, s, l, alpha = color.Hsla
    hsla(h, s, clamp(l - amount), alpha)

let alpha (color:Color) = color.Alpha
let greyscale color = desaturate color 1.0
let fade color alpha = { color with Alpha = clamp alpha }
let fadein color alpha = { color with Alpha = clamp (color.Alpha + alpha) }
let fadeout color alpha = { color with Alpha = clamp (color.Alpha - alpha) }
let spin (color:Color) angle =
    let h, s, l, alpha = color.Hsla
    hsla(clampAngle(h + angle), s,l, alpha)

let mix (c1:Color) (c2:Color) weight =
    let weight = clamp weight
    let w = weight * 2.0 - 1.0
    let alphaDiff = c1.Alpha - c2.Alpha
    let w1 = 
        let w1 = 
            if w * alphaDiff = -1.0 then w
            else (w + alphaDiff) / (1.0 + w * alphaDiff)
        (w1 + 1.0) / 2.0
    let w2 = 1.0 - w1
    { 
        R = c1.R * w1 + c2.R * w2;
        G = c1.G * w1 + c2.G * w2;
        B = c1.B * w1 + c2.B * w2;
        Alpha = c1.Alpha * weight + c2.Alpha * (1.0 - weight)
    }