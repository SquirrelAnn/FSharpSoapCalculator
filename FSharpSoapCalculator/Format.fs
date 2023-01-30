namespace Formatting 

open System
open Microsoft.FSharp.Core.LanguagePrimitives

module Format = 
    let DecimalToFloat (decimalval:decimal) =
        float decimalval

    let RoundTwo<[<Measure>]'u>(x: float<'u>): float<'u> = Math.Round(float x, 2) |> FloatWithMeasure
    let RoundZero<[<Measure>]'u>(x: float<'u>): float<'u> = (Math.Round(decimal x, MidpointRounding.AwayFromZero) |> DecimalToFloat) |> FloatWithMeasure


    
