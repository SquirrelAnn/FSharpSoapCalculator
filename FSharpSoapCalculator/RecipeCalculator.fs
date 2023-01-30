namespace Calculator 

open Oil.Oil
open RecipeSettings
open RecipeProps.Recipe

module RecipeCalculator =
    type EmptyState = NoItems

    type ActiveState = { Oils : Oil list; }
 
    type RecipeOils =
        | Empty of EmptyState
        | Active of ActiveState

    let addToEmptyState oil =
        RecipeOils.Active {Oils=[oil]}

    let addToActiveState state oilToAdd =
        let newList = oilToAdd :: state.Oils
        RecipeOils.Active {state with Oils=newList }

    let removeFromActiveState state itemToRemove =
       let newList = state.Oils
                     |> List.filter (fun i -> i<>itemToRemove)

       match newList with
       | [] -> RecipeOils.Empty NoItems
       | _ -> RecipeOils.Active {state with Oils=newList}
    
    type EmptyState with
        member this.Add = addToEmptyState

    type ActiveState with
       member this.Add = addToActiveState this
       member this.Remove = removeFromActiveState this

    let addOilToRecipe recipeOils oil =
       match recipeOils with
       | Empty state -> state.Add oil
       | Active state -> state.Add oil

    let removeOilFromRecipe recipe oil =
       match recipe with
       | Empty state ->
          printfn "ERROR: No oils chosen"
          recipe
       | Active state ->
          state.Remove oil
    
    let displayOils recipe  =
       match recipe with
       | Empty state ->
          printfn "No oils chosen"
       | Active state ->
          printfn "The recipe contains %A oils"
                                                    state.Oils

    let sumOfFatsNoLacid oils =
         oils
         |> List.filter (fun x -> x.FatType <> Lacid)
         |> List.map (fun x -> x.Grams)
         |> List.sum

    let sumOfFats oils =
         oils
         |> List.map (fun x -> x.Grams)
         |> List.sum
    
    let calcSoapProps (totalGrams:float<Grams>, previous) (property,oilGrams) =
        totalGrams, previous + (property * (oilGrams / totalGrams))

    let getSoapPropertyValue totalGrams grams =
        grams
        |> List.fold calcSoapProps (totalGrams, 0.0) 
        |> snd
        |> Formatting.Format.RoundZero

    let calculateBubbleBooster totalGrams oils =
        let Boost = oils |> List.map (fun x -> x.OilProperties.Boost, x.Grams) |> getSoapPropertyValue totalGrams
        let Schaummenge = oils |> List.map (fun x -> x.OilProperties.Schaummenge, x.Grams) |> getSoapPropertyValue totalGrams
        match Boost with
            | 0.0 -> Schaummenge
            | _ -> Schaummenge * (1.0 + (Boost / 30.0)) |> Formatting.Format.RoundZero

    let calcSoapProperties oils =
        let totalGrams = sumOfFatsNoLacid oils
        let initVal = totalGrams, 0.0
        {   
            Jod = oils |> List.map (fun x -> x.OilProperties.Jod, x.Grams) |> getSoapPropertyValue totalGrams
            Reinigung = oils |> List.map (fun x -> x.OilProperties.Reinigung, x.Grams) |> getSoapPropertyValue totalGrams
            HaerteStabilitaet = oils |> List.map (fun x -> x.OilProperties.HaerteStabilitaet, x.Grams) |> getSoapPropertyValue totalGrams
            HaerteFestigkeit = oils |> List.map (fun x -> x.OilProperties.HaerteFestigkeit, x.Grams) |> getSoapPropertyValue totalGrams
            Pflege = oils |> List.map (fun x -> x.OilProperties.Pflege, x.Grams) |> getSoapPropertyValue totalGrams
            Schaummenge = calculateBubbleBooster totalGrams oils
            Schaumstabilitaet = oils |> List.map (fun x -> x.OilProperties.Schaumstabilitaet, x.Grams) |> getSoapPropertyValue totalGrams
            Haltbarkeit = oils |> List.map (fun x -> x.OilProperties.Haltbarkeit, x.Grams) |> getSoapPropertyValue totalGrams
            Ins = oils |> List.map (fun x -> x.OilProperties.Ins, x.Grams) |> getSoapPropertyValue totalGrams
            Boost = oils |> List.map (fun x -> x.OilProperties.Boost, x.Grams) |> getSoapPropertyValue totalGrams
        }

    let calculateSoapWeight sumFats liquidGrams scentGrams =
        sumFats + liquidGrams + scentGrams

    let naohFullSaponification oil =
        oil.Grams * oil.SaponificationNaOH

    let calculateNaOHWithoutSF oil settings =
        naohFullSaponification oil * settings.NaohPercent / 100.0
    
    let calculateNaOHWithSF (naOHWithoutSF:float<Grams>) superfat =
        naOHWithoutSF * (1.0 - superfat / 100.0)

    let kohWithSF settings oil superfat =
        let naOHForKOHWithSF = naohFullSaponification oil * (1.0 - superfat / 100.0)
        1.40272 * (1.0 - (settings.NaohPercent / 100.0)) * naOHForKOHWithSF / (settings.KohPurity / 100.0)
    
    let kohWithoutSF settings oil =
        let naOHForKOHWithouSF = naohFullSaponification oil
        1.40272 * (1.0 - (settings.NaohPercent / 100.0)) * naOHForKOHWithouSF / (settings.KohPurity / 100.0)

    let calcLyeForOil superfat settings oil =
        let naohFullSaponification = naohFullSaponification oil
        let naohFSWithPerc = calculateNaOHWithoutSF oil settings
        {
            NaOHWithSuperfat = // only oils
                match oil.FatType with 
                | FatType.Lacid -> 0.0<Grams> 
                | _ -> calculateNaOHWithSF naohFSWithPerc superfat
            NaOHWithoutSuperfat = // only acids
                match oil.FatType with 
                | FatType.Lacid -> calculateNaOHWithoutSF oil settings
                | _ -> 0.0<Grams>  
            KOHWithSuperfat = // only oils
                match oil.FatType with 
                | FatType.Lacid -> 0.0<Grams> 
                | _ -> kohWithSF settings oil superfat
            KOHWithoutSuperfat = // only acids
                match oil.FatType with 
                | FatType.Lacid ->  kohWithoutSF settings oil
                | _ -> 0.0<Grams>
        }
    
    let calcLyeForSuperfat previous next =
        {
            NaOHWithSuperfat = previous.NaOHWithSuperfat + next.NaOHWithSuperfat // only oils
            NaOHWithoutSuperfat = previous.NaOHWithoutSuperfat + next.NaOHWithoutSuperfat // only acids
            KOHWithSuperfat = previous.KOHWithSuperfat + next.KOHWithSuperfat // only oils
            KOHWithoutSuperfat = previous.KOHWithoutSuperfat + next.KOHWithoutSuperfat // only acids
        }
    
    let calcLyeResult lyeWithAndWithoutSF =
        {
            LyeWithAndWithoutSF = lyeWithAndWithoutSF
            KOHGrams = lyeWithAndWithoutSF.KOHWithoutSuperfat + lyeWithAndWithoutSF.KOHWithSuperfat |> Formatting.Format.RoundTwo
            NaOHGrams = lyeWithAndWithoutSF.NaOHWithoutSuperfat + lyeWithAndWithoutSF.NaOHWithSuperfat |> Formatting.Format.RoundTwo
        }

    let calculateLye (oils:Oil list) settings =
        let superfats = [settings.SuperfatFrom .. settings.SuperfatTo]

        superfats
        |> List.map(fun x -> x, calcLyeResult (oils |> List.fold(fun prev next -> calcLyeForSuperfat prev (calcLyeForOil x settings next)) {
            NaOHWithSuperfat = 0.0<Grams> // only oils
            NaOHWithoutSuperfat = 0.0<Grams> // only acids
            KOHWithSuperfat = 0.0<Grams> // only oils
            KOHWithoutSuperfat = 0.0<Grams> // only acids
        }))

    let calculate oils settings =
        {
            Id = 1
            SumFats = sumOfFatsNoLacid oils |> Formatting.Format.RoundTwo
            SoapWeight = calculateSoapWeight (sumOfFats oils) settings.LiquidGrams settings.ScentGrams |> Formatting.Format.RoundTwo
            SoapProperties = calcSoapProperties oils
            RecipeSettings = settings
            SuperfatLyeResults = calculateLye oils settings
        }

    let calculateRecipe recipe settings =
     match recipe with
     | Empty state -> None
     | Active state ->
        let recipeCalculated = calculate state.Oils settings
        Some recipeCalculated
    
    type RecipeOils with
       static member NewRecipe = RecipeOils.Empty NoItems
       member this.Add = addOilToRecipe this
       member this.Remove = removeOilFromRecipe this
       member this.Display = displayOils this
       member this.Calculate = calculateRecipe this

