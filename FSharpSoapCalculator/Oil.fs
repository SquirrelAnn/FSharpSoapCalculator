namespace Oil

open Microsoft.FSharp.Reflection

module Oil = 

    let toString (x:'a) = 
        let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
        case.Name
    
    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None
    
    type OilCategory =
        | Vegetarian
        | Animal with 
        override this.ToString() = toString this
        static member fromString s = fromString<OilCategory> s
    
    type FatType =
        | Default
        | Wax    
        | Fat    
        | Oil    
        | Acid    
        | Lacid    
        | Butter with 
        override this.ToString() = toString this
        static member fromString s = fromString<FatType> s
    
    [<Measure>] type Grams 
    
    type OilProperties =
        {
            Jod: double
            Reinigung: double
            HaerteStabilitaet: double
            HaerteFestigkeit: double
            Pflege: double
            Schaummenge: double
            Schaumstabilitaet: double
            Haltbarkeit: double
            Ins: double
            Boost: double
        }
    
    type Oil =
        {
            Id: int
            Name: string
            SaponificationNaOH: double
            Category: OilCategory
            FatType: FatType        
            OilProperties: OilProperties
            Rancid: bool
            Inci: string
            Grams: float<Grams>
        }

