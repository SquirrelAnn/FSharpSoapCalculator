namespace RecipeSettings

open Oil.Oil

type Superfat = int  

type RecipeSettings = {
    Location: string
    RecipeName: string
    Notes: string
    RecipeCategory: string
    LiquidName: string
    LiquidGrams: float<Grams>
    LiquidPercent: float
    ScentName: string
    ScentGrams: float<Grams>
    ScentPercent: float
    ReviewSoaps: int
    ReviewNotes: string 
    NaohPercent: float
    KohPercent: float
    KohPurity: float
    SuperfatFrom: Superfat
    SuperfatTo: Superfat
} 