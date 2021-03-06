﻿namespace AssetOpt

module DataGenerator = 

    type System.Random with 
        member this.NextDouble(minVal : float, maxVal : float) : float = minVal + (maxVal - minVal) * this.NextDouble()


    // TODO: The distribution of exchange rates is clearly not uniform.
    // TODO: Account for exchange rate spread if necessary
    // Exchange rates between assets. Modify as appropriate.
    let getExchangeRates (rnd : System.Random) (conf : ConfigData) : float[][] =
        let rateToBase = 
            if conf.rescaleRates 
            then [| for i in 0..conf.noOfAssets - 1 -> 1.0 |]
            else [| for i in 0..conf.noOfAssets - 1 -> if i = 0 then 1.0 else rnd.NextDouble (conf.minRate, conf.maxRate) |]

            
        rateToBase |> Array.map (fun i -> rateToBase |> Array.map (fun j -> i / j))


    // Number of resources for a contract
    let getNoOfRes (rnd : System.Random) (conf : ConfigData) : int = 
        rnd.Next (conf.minNoOfAssets, conf.maxNoOfAssets + 1)


    // Resources used by a contract
    let getResources (rnd : System.Random) (conf : ConfigData) : int[] =
        let noOfRes = getNoOfRes rnd conf
        [| for i in 1..(noOfRes + 2) -> rnd.Next (0, conf.maxNoOfAssets) |] 
        |> Array.distinct
        |> Array.sort


    // Increment for a contract
    let getIncrement (rnd : System.Random) (conf : ConfigData) : float = 
      conf.incrValues.[rnd.Next(conf.incrValues.Length)]


    (* Min value for a contract *)
    let getMinValue (rnd : System.Random) (conf : ConfigData) (incr : float) : float = 
        incr * conf.minValMultipliers.[rnd.Next(conf.minValMultipliers.Length)]


    // TODO: Rounding rules were not specified
    let getAssetDescriptor (rnd : System.Random) (conf : ConfigData) (asset : int) : AssetDescriptor = 
        let increment = getIncrement rnd conf
        let minVal = getMinValue rnd conf increment

        {
            asset = asset
            incr = increment
            minVal = minVal
        }


    let getContract (rnd : System.Random) (conf : ConfigData) (contractID : int) : ContractDescriptor = 
        let resoures = getResources rnd conf
        let baseAsset = resoures.[rnd.Next(0, resoures.Length)]
        let amount = round (conf.maxContractAmount * rnd.NextDouble())
        let nonPayingRate = round (rnd.NextDouble (conf.maxIncomeAnnualRate, conf.maxNonPayingAnnualRate)) // / 365.0

        let overPayingRate = 0.0; // Update if needed

        let descriptors = 
            resoures
            |> Array.map (fun r -> getAssetDescriptor rnd conf r)
   
        {
            contractID = contractID
            baseAsset = baseAsset
            amount = amount
            descriptors = descriptors
            nonPayingRate = nonPayingRate
            //overPayingRate = overPayingRate
        }


    let getAllContracts (rnd : System.Random) (conf : ConfigData) : ContractDescriptor[] = 
       [| for i in 0..conf.noOfContracts - 1 -> getContract rnd conf i |]


    let getPosition (rnd : System.Random) (conf : ConfigData) (asset : int) : PositionData = 
        {
            asset = asset
            balance = round (rnd.NextDouble (conf.minAmountOnHand, conf.maxAmountOnHand))
            incomeRate = round (conf.maxIncomeAnnualRate * rnd.NextDouble())
        }


    let getAllPositions (rnd : System.Random) (conf : ConfigData) : PositionData[] = 
        [| for i in 0..conf.noOfAssets - 1 -> getPosition rnd conf i |]


    let getAllData (rnd : System.Random) (conf : ConfigData) : AllData = 
        {
            conf = conf
            exchangeRates = getExchangeRates rnd conf
            contracts = getAllContracts rnd conf
            positions = getAllPositions rnd conf
        }


    
