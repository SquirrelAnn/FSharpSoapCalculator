namespace SoapCalculatorTests

module SoapCalculatorTests =

    open NUnit.Framework
    open Calculator.RecipeCalculator
    open Oil.Oil
    open RecipeSettings
    open RecipeProps.Recipe
    open FSharp.Data
    open TestDataProvider.TestDataProvider
    
    [<SetUp>]
    let Setup () =
        ()
    
    type TestCase4 = JsonProvider<"Testdata/TestCaseData4.json">
    
    
    let unpackRecipe (recipe:Recipe option) =
        match recipe with
        | Some i -> i
        | None -> nullArg "Recipe is none, but should be some."   
    
    [<Test>]
    let ``100%Coconut oil ref.`` () =
        let emptyRecipe = RecipeOils.NewRecipe
    
          
        let oilProps = {
              OilProperties.Jod = 9.0
              Reinigung = 67.0
              HaerteStabilitaet = 82.0
              HaerteFestigkeit = 93.0
              Pflege = 10.0
              Schaummenge = 67.0
              Schaumstabilitaet = 12.0
              Haltbarkeit = 98.0
              Ins = 239.0
              Boost = 0.0
        }
    
        let coconutOil = {
              Id = 1
              Name = "Coconut oil ref."
              SaponificationNaOH = 0.1768
              Category = OilCategory.Vegetarian
              FatType = FatType.Oil   
              OilProperties = oilProps
              Rancid = false
              Inci = "Cocos Nuciferia Oil"
              Grams = 900.0<Grams>
          }
    
        let expectedProps = {
              SoapProperties.Jod = 9.0
              Reinigung = 67.0
              HaerteStabilitaet = 82.0
              HaerteFestigkeit = 93.0
              Pflege = 10.0
              Schaummenge = 67.0
              Schaumstabilitaet = 12.0
              Haltbarkeit = 98.0
              Ins = 239.0
              Boost = 0.0
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
    
        let LyeWithAndWithoutSF = {
            NaOHWithSuperfat= 0.0<Grams> // only oils
            NaOHWithoutSuperfat= 0.0<Grams> // only acids
            KOHWithSuperfat= 0.0<Grams> // only oils
            KOHWithoutSuperfat= 0.0<Grams> // only acids
        }
    
        let expectedNaOHSuperfat5 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 0.0<Grams>
            NaOHGrams = 151.16<Grams>
        }
        let expectedNaOHSuperfat6 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 0.0<Grams>
            NaOHGrams = 149.57<Grams>
        }
        let expectedNaOHSuperfat7 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 0.0<Grams>
            NaOHGrams = 147.98<Grams>
        }
    
        let superfats = [settings.SuperfatFrom .. settings.SuperfatTo]
            
        let b = emptyRecipe.Add coconutOil
        printf "cartA="; b.Display
        let recipe = b.Calculate settings
        Assert.IsTrue(recipe.IsSome)
    
        Assert.AreEqual(expectedNaOHSuperfat5.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[0]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[1]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[2]).NaOHGrams)
    
        Assert.AreEqual(recipe.Value.SoapProperties, expectedProps)
    
    [<Test>]
    let ``100%Coconut oil ref. with citric acid 50/50 koh naoh`` () =
        let emptyRecipe = RecipeOils.NewRecipe
          
        let oilProps = {
              OilProperties.Jod = 9.0
              Reinigung = 67.0
              HaerteStabilitaet = 82.0
              HaerteFestigkeit = 93.0
              Pflege = 10.0
              Schaummenge = 67.0
              Schaumstabilitaet = 12.0
              Haltbarkeit = 98.0
              Ins = 239.0
              Boost = 0.0
        }
    
        let coconutOil = {
              Id = 1
              Name = "Coconut oil ref."
              SaponificationNaOH = 0.1768
              Category = OilCategory.Vegetarian
              FatType = FatType.Oil   
              OilProperties = oilProps
              Rancid = false
              Inci = "Cocos Nuciferia Oil"
              Grams = 900.0<Grams>
        }
    
        let citricAcidProps = {
              OilProperties.Jod = 0.0
              Reinigung = 0.0
              HaerteStabilitaet = 0.0
              HaerteFestigkeit = 0.0
              Pflege = 0.0
              Schaummenge = 0.0
              Schaumstabilitaet = 0.0
              Haltbarkeit = 0.0
              Ins = 0.0
              Boost = 0.0
        }
    
        let citricAcid = {
              Id = 1
              Name = "Citric acid"
              SaponificationNaOH = 0.571
              Category = OilCategory.Vegetarian
              FatType = FatType.Lacid   
              OilProperties = citricAcidProps
              Rancid = false
              Inci = "Citric acid"
              Grams = 30.0<Grams>
        }
    
        let expectedProps = {
              SoapProperties.Jod = 9.0
              Reinigung = 67.0
              HaerteStabilitaet = 82.0
              HaerteFestigkeit = 93.0
              Pflege = 10.0
              Schaummenge = 67.0
              Schaumstabilitaet = 12.0
              Haltbarkeit = 98.0
              Ins = 239.0
              Boost = 0.0
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
            NaohPercent= 50.00
            KohPercent= 50.00
            KohPurity= 90.00
            SuperfatFrom= 5
            SuperfatTo= 12
        } 
    
        let LyeWithAndWithoutSF = {
            NaOHWithSuperfat= 0.0<Grams> // only oils
            NaOHWithoutSuperfat= 0.0<Grams> // only acids
            KOHWithSuperfat= 0.0<Grams> // only oils
            KOHWithoutSuperfat= 0.0<Grams> // only acids
        }
    
        let expectedNaOHSuperfat5 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 131.15<Grams>
            NaOHGrams = 84.15<Grams>
        }
        let expectedNaOHSuperfat6 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 129.91<Grams>
            NaOHGrams = 83.35<Grams>
        }
        let expectedNaOHSuperfat7 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 128.67<Grams>
            NaOHGrams = 82.56<Grams>
        }
    
        let superfats = [settings.SuperfatFrom .. settings.SuperfatTo]
            
        let b = emptyRecipe.Add coconutOil
        let c = b.Add citricAcid
        let recipe = c.Calculate settings
        Assert.IsTrue(recipe.IsSome)
    
        Assert.AreEqual(expectedNaOHSuperfat5.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[0]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[1]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[2]).NaOHGrams)
    
        Assert.AreEqual(expectedNaOHSuperfat5.KOHGrams, snd(recipe.Value.SuperfatLyeResults[0]).KOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.KOHGrams, snd(recipe.Value.SuperfatLyeResults[1]).KOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.KOHGrams, snd(recipe.Value.SuperfatLyeResults[2]).KOHGrams)
    
        Assert.AreEqual(recipe.Value.SoapProperties, expectedProps)
    
    [<Test>]
    let ``50% coconut 50% shea with citric acid 50/50 koh naoh`` () =
        let emptyRecipe = RecipeOils.NewRecipe
          
        let coconutOilProps = {
              OilProperties.Jod = 9.0
              Reinigung = 67.0
              HaerteStabilitaet = 82.0
              HaerteFestigkeit = 93.0
              Pflege = 10.0
              Schaummenge = 67.0
              Schaumstabilitaet = 12.0
              Haltbarkeit = 98.0
              Ins = 239.0
              Boost = 0.0
        }
    
        let coconutOil = {
              Id = 1
              Name = "Coconut oil ref."
              SaponificationNaOH = 0.1768
              Category = OilCategory.Vegetarian
              FatType = FatType.Oil   
              OilProperties = coconutOilProps
              Rancid = false
              Inci = "Cocos Nuciferia Oil"
              Grams = 900.0<Grams>
        }
    
        let citricAcidProps = {
              OilProperties.Jod = 0.0
              Reinigung = 0.0
              HaerteStabilitaet = 0.0
              HaerteFestigkeit = 0.0
              Pflege = 0.0
              Schaummenge = 0.0
              Schaumstabilitaet = 0.0
              Haltbarkeit = 0.0
              Ins = 0.0
              Boost = 0.0
        }
    
        let citricAcid = {
              Id = 1
              Name = "Citric acid"
              SaponificationNaOH = 0.571
              Category = OilCategory.Vegetarian
              FatType = FatType.Lacid   
              OilProperties = citricAcidProps
              Rancid = false
              Inci = "Citric acid"
              Grams = 30.0<Grams>
        }
    
        let sheaProps = {
              OilProperties.Jod = 60.0
              Reinigung = 0.0
              HaerteStabilitaet = 46.0
              HaerteFestigkeit = 85.0
              Pflege = 45.0
              Schaummenge = 0.0
              Schaumstabilitaet = 45.0
              Haltbarkeit = 94.0
              Ins = 119.0
              Boost = 0.0
        }
    
        let sheaButter = {
              Id = 1
              Name = "Shea Butter"
              SaponificationNaOH = 0.12761
              Category = OilCategory.Vegetarian
              FatType = FatType.Butter   
              OilProperties = sheaProps
              Rancid = false
              Inci = "Shea Butter"
              Grams = 900.0<Grams>
        }
    
        let expectedProps = {
              SoapProperties.Jod = 35.0
              Reinigung = 34.0
              HaerteStabilitaet = 64.0
              HaerteFestigkeit = 89.0
              Pflege = 28.0
              Schaummenge = 34.0
              Schaumstabilitaet = 29.0
              Haltbarkeit = 96.0
              Ins = 179.0
              Boost = 0.0
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
            NaohPercent= 50.00
            KohPercent= 50.00
            KohPurity= 90.00
            SuperfatFrom= 5
            SuperfatTo= 12
        } 
    
        let LyeWithAndWithoutSF = {
            NaOHWithSuperfat= 0.0<Grams> // only oils
            NaOHWithoutSuperfat= 0.0<Grams> // only acids
            KOHWithSuperfat= 0.0<Grams> // only oils
            KOHWithoutSuperfat= 0.0<Grams> // only acids
        }
    
        let expectedNaOHSuperfat5 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 216.18<Grams>
            NaOHGrams = 138.7<Grams>
        }
        let expectedNaOHSuperfat6 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 214.04<Grams>
            NaOHGrams = 137.33<Grams>
        }
        let expectedNaOHSuperfat7 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 211.91<Grams>
            NaOHGrams = 135.96<Grams>
        }
    
        let superfats = [settings.SuperfatFrom .. settings.SuperfatTo]
            
        let b = emptyRecipe.Add coconutOil
        let c = b.Add citricAcid
        let d = c.Add sheaButter
        let recipe = d.Calculate settings
        Assert.IsTrue(recipe.IsSome)
    
        Assert.AreEqual(expectedNaOHSuperfat5.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[0]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[1]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.NaOHGrams, snd(recipe.Value.SuperfatLyeResults[2]).NaOHGrams)
    
        Assert.AreEqual(expectedNaOHSuperfat5.KOHGrams, snd(recipe.Value.SuperfatLyeResults[0]).KOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.KOHGrams, snd(recipe.Value.SuperfatLyeResults[1]).KOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.KOHGrams, snd(recipe.Value.SuperfatLyeResults[2]).KOHGrams)
    
        Assert.AreEqual(recipe.Value.SoapProperties, expectedProps)
    
    [<Test>]
    let ``80% coconut 18% castor with citric acid 100% naoh`` () =
        let emptyRecipe = RecipeOils.NewRecipe
    
        let test = TestCase4.Load("Testdata/TestCaseData4.json")
        
        
        let LyeWithAndWithoutSF = {
            NaOHWithSuperfat= 0.0<Grams> // only oils
            NaOHWithoutSuperfat= 0.0<Grams> // only acids
            KOHWithSuperfat= 0.0<Grams> // only oils
            KOHWithoutSuperfat= 0.0<Grams> // only acids
        }
    
        let expectedNaOHSuperfat5 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 0.0<Grams>
            NaOHGrams = 192.73<Grams>
        }
        let expectedNaOHSuperfat6 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 0.0<Grams>
            NaOHGrams = 190.88<Grams>
        }
        let expectedNaOHSuperfat7 = {
            LyeWithAndWithoutSF = LyeWithAndWithoutSF
            KOHGrams = 0.0<Grams>
            NaOHGrams = 189.03<Grams>
        }
    
        let settings = getSettings test.Settings
    
        let d = addoils test.Oils emptyRecipe
        let recipeCalc = d.Calculate settings
        Assert.IsTrue(recipeCalc.IsSome)
    
        Assert.AreEqual(expectedNaOHSuperfat5.NaOHGrams, snd(recipeCalc.Value.SuperfatLyeResults[0]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.NaOHGrams, snd(recipeCalc.Value.SuperfatLyeResults[1]).NaOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.NaOHGrams, snd(recipeCalc.Value.SuperfatLyeResults[2]).NaOHGrams)
    
        Assert.AreEqual(expectedNaOHSuperfat5.KOHGrams, snd(recipeCalc.Value.SuperfatLyeResults[0]).KOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat6.KOHGrams, snd(recipeCalc.Value.SuperfatLyeResults[1]).KOHGrams)
        Assert.AreEqual(expectedNaOHSuperfat7.KOHGrams, snd(recipeCalc.Value.SuperfatLyeResults[2]).KOHGrams)
    
        let test2 = unpackRecipe recipeCalc
        Assert.AreEqual(test2.SoapProperties, getSoapProps test.ExpectedSoapProps)