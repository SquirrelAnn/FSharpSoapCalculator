namespace RecipeProps

open Oil.Oil
open RecipeSettings

module Recipe =
    type SoapProperties = {
        Jod: float
        Reinigung: float
        HaerteStabilitaet: float
        HaerteFestigkeit: float
        Pflege: float
        Schaummenge: float
        Schaumstabilitaet: float
        Haltbarkeit: float
        Ins: float
        Boost: float
    }

    type LyeWithAndWithoutSF = {
        NaOHWithSuperfat: float<Grams> // only oils
        NaOHWithoutSuperfat: float<Grams> // only acids
        KOHWithSuperfat: float<Grams> // only oils
        KOHWithoutSuperfat: float<Grams> // only acids
    }

    type LyeResult = {
        LyeWithAndWithoutSF: LyeWithAndWithoutSF
        KOHGrams: float<Grams>
        NaOHGrams: float<Grams>
    }

    type Recipe = {
        Id: int
        SumFats: float<Grams>
        SoapWeight: float<Grams>
        SoapProperties: SoapProperties
        RecipeSettings: RecipeSettings
        SuperfatLyeResults: (Superfat * LyeResult) list
    }    