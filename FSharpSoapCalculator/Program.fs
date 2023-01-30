namespace test

open Calculator.RecipeCalculator
open Oil.Oil
open RecipeSettings

module test = 

  let recipeOils = RecipeOils.NewRecipe
  printf "recipeOils="; recipeOils.Display
    
  let oilProps = {
        Jod = 1.0
        Reinigung = 1.0
        HaerteStabilitaet = 1.0
        HaerteFestigkeit = 1.0
        Pflege = 1.0
        Schaummenge = 1.0
        Schaumstabilitaet = 1.0
        Haltbarkeit = 1.0
        Ins = 1.0
        Boost = 20.0
  }

  let oilProps2 = {
        Jod = 50.0
        Reinigung = 50.0
        HaerteStabilitaet = 50.0
        HaerteFestigkeit = 50.0
        Pflege = 50.0
        Schaummenge = 50.0
        Schaumstabilitaet = 50.0
        Haltbarkeit = 50.0
        Ins = 50.0
        Boost = 0.0
  }

  let coconutOil = {
        Id = 1
        Name = "Coconut Oil"
        SaponificationNaOH = 1
        Category = OilCategory.Vegetarian
        FatType = FatType.Oil   
        OilProperties = oilProps
        Rancid = false
        Inci = "Cocos Nuciferia Oil"
        Grams = 100.0<Grams>
    }
  let coconutOil2 = {
        Id = 1
        Name = "Coconut Oil2"
        SaponificationNaOH = 1
        Category = OilCategory.Vegetarian
        FatType = FatType.Oil   
        OilProperties = oilProps2
        Rancid = false
        Inci = "Cocos Nuciferia Oil"
        Grams = 20.0<Grams>
    }

  let settings = {
      Location= ""
      RecipeName= "TestRecipe"
      Notes= "Notes"
      RecipeCategory= "Category"
      LiquidName= "Water"
      LiquidGrams= 250.0<Grams>
      LiquidPercent= 25.00
      ScentName= "No Scent"
      ScentGrams= 30.0<Grams>
      ScentPercent= 3.00
      ReviewSoaps= 1
      ReviewNotes= "Review" 
      NaohPercent= 100.00
      KohPercent= 0.00
      KohPurity= 90.00
      SuperfatFrom= 5
      SuperfatTo= 12
  } 
      
  let b = recipeOils.Add coconutOil
  let cartA = b.Add coconutOil2
  printf "cartA="; cartA.Display
  let grams = cartA.Calculate settings
  printf "soap weight %A" grams
