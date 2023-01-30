namespace TestDataProvider

open Calculator.RecipeCalculator
open Oil.Oil
open RecipeSettings
open RecipeProps.Recipe
open FSharp.Data

module TestDataProvider =
    type JsonData =
        | TestData4 of JsonProvider<"Testdata/TestCaseData4.json">
    
    let convertToOil (oil:JsonProvider<"Testdata/TestCaseData4.json">.Oil) = 
        {
          Id = oil.Id
          Name = oil.Name
          SaponificationNaOH = (double) oil.SaponificationNaOh
          Category = (OilCategory.fromString oil.Category).Value
          FatType = (FatType.fromString oil.FatType).Value  
          OilProperties = {
                  OilProperties.Jod = oil.Jod
                  Reinigung = oil.Reinigung
                  HaerteStabilitaet = oil.HaerteStabilitaet
                  HaerteFestigkeit = oil.HaerteFestigkeit
                  Pflege = oil.Pflege
                  Schaummenge = oil.Schaummenge
                  Schaumstabilitaet = oil.Schaumstabilitaet
                  Haltbarkeit = oil.Haltbarkeit
                  Ins = oil.Ins
                  Boost = oil.Boost
                }
          Rancid = oil.Rancid
          Inci = oil.Inci
          Grams = (float) oil.Grams * 1.0<Grams>
        }
    
    let getSoapProps (soapProps:JsonProvider<"Testdata/TestCaseData4.json">.ExpectedSoapProps) =
        {
          SoapProperties.Jod = soapProps.Jod
          Reinigung = soapProps.Reinigung
          HaerteStabilitaet = soapProps.HaerteStabilitaet
          HaerteFestigkeit = soapProps.HaerteFestigkeit
          Pflege = soapProps.Pflege
          Schaummenge = soapProps.Schaummenge
          Schaumstabilitaet = soapProps.Schaumstabilitaet
          Haltbarkeit = soapProps.Haltbarkeit
          Ins = soapProps.Ins
          Boost = soapProps.Boost
        }
    
    let getSettings (settings:JsonProvider<"Testdata/TestCaseData4.json">.Settings) =
        {
            RecipeSettings.Location= settings.Location
            RecipeName= settings.RecipeName
            Notes= settings.Notes
            RecipeCategory= settings.RecipeCategory
            LiquidName= settings.LiquidName
            LiquidGrams= (float) settings.LiquidGrams * 1.0<Grams>
            LiquidPercent= settings.LiquidPercent
            ScentName= settings.ScentName
            ScentGrams= (float) settings.ScentGrams * 1.0<Grams>
            ScentPercent= settings.ScentPercent
            ReviewSoaps= settings.ReviewSoaps
            ReviewNotes= settings.ReviewNotes
            NaohPercent= settings.NaohPercent
            KohPercent= settings.KohPercent
            KohPurity= settings.KohPurity
            SuperfatFrom= settings.SuperfatFrom
            SuperfatTo= settings.SuperfatTo
        } 

    let addToRecipe (recipe:RecipeOils) (oil:Oil) =
        recipe.Add oil

    let addoils oils recipe=
        oils 
        |> List.ofArray 
        |> List.map(fun x -> convertToOil x)
        |> List.fold addToRecipe recipe
