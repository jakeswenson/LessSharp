module Less.Helpers

open System

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
        if h < 0.0 then 360.0 + h else h
    
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

let validate f = 
    if f < 0.0 || f > 1.0 then failwith "Invalid color value provided!"
    else f

type Color = 
    {
        R: float;
        G: float;
        B: float;
        A: float option;
    }
    override this.ToString() = 
        match this.A with
        | Some A -> sprintf "RGBA(%d, %d, %d, %f)" (Color.byte this.R) (Color.byte this.G) (Color.byte this.B) A
        | _ -> sprintf "RGB(%d, %d, %d)" (Color.byte this.R) (Color.byte this.G) (Color.byte this.B)
    member this.Hsv = toHsv (this.R, this.G, this.B)
    member this.Hsl = toHsl (this.R, this.G, this.B)
    static member Zero = { R = 0.0; G = 0.0; B = 0.0; A = None }
    static member private byte v = byte (v * float Byte.MaxValue)
    static member Force (r:int,g:int,b:int) = { R = float r / 255.0; G = float g / 255.0; B = float b / 255.0; A = None }
    static member Force (r:int,g:int,b:int, a) = { R = float r / 255.0; G = float g / 255.0; B = float b / 255.0; A = Some a }
    static member Create(r:byte, g:byte, b:byte) = Color.Force(int r, int g, int b)
    static member Create(r, g, b) = { R = validate r; G = validate g; B = validate b; A = None }
    static member Create(r, g, b, a) = { R = validate r; G = validate g; B = validate b; A = Some(validate a) }

let rgbBase h h' chroma x = 
    if Double.IsNaN(h) then (0.0, 0.0, 0.0)
    elif h' >= 0.0 && h' < 1.0 then (chroma, x, 0.0)
    elif h' >= 1.0 && h' < 2.0 then (x, chroma, 0.0)
    elif h' >= 2.0 && h' < 3.0 then (0.0, chroma, x)
    elif h' >= 3.0 && h' < 4.0 then (0.0, x, chroma)
    elif h' >= 4.0 && h' < 5.0 then (x, 0.0, chroma)
    elif h' >= 5.0 && h' < 6.0 then (chroma, 0.0, x)
    else (0.0, 0.0, 0.0)

let hsv (h, s, v) = 
    assert (h >= 0.0 && h <= 360.0)
    assert (s >= 0.0 && s <= 1.0)
    assert (v >= 0.0 && v <= 1.0)
    let chroma = v * s
    let h' = h / 60.0
    let x = chroma * (1.0 - abs(h' % 2.0 - 1.0))
    let r1, g1, b1 = rgbBase h h' chroma x
    let m = v - chroma
    Color.Create(r1 + m, g1 + m, b1 + m)

let hsl (h, s, l) = 
    assert (h >= 0.0 && h <= 360.0)
    assert (s >= 0.0 && s <= 1.0)
    assert (l >= 0.0 && l <= 1.0)
    let chroma = (1.0 - abs(2.0 * l - 1.0)) * s
    let h' = h / 60.0
    let x = chroma * (1.0 - abs(h' % 2.0 - 1.0))
    let r1, g1, b1 = rgbBase h h' chroma x
    let m = l - 0.5 * chroma
    Color.Create(r1 + m, g1 + m, b1 + m)

