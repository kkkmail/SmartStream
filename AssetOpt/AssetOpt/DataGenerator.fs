namespace AssetOpt

module DataGenerator = 

    type System.Random with 
        member this.NextDouble(minVal : float, maxVal : float) : float = minVal + (maxVal - minVal) * this.NextDouble()


    // TODO: The distribution of exchange rates is clearly not uniform.
    // TODO: Account for exchange rate spread if necessary
    // Exchange rates between assets. Modify as appropriate.
    let getExchangeRates (rnd : System.Random) (conf : ConfigData) =
        let rateToBase = [| for i in 0..conf.noOfAssets - 1 -> if i = 0 then 1.0 else rnd.NextDouble (conf.minRate, conf.maxRate) |]
        rateToBase |> Array.map (fun i -> rateToBase |> Array.map (fun j -> i / j))


    // Number of resources for a contract
    let getNoOfRes (rnd : System.Random) (conf : ConfigData) : int = 
        rnd.Next (conf.minNoOfRes, conf.maxNoOfRes + 1)


    // Resources used by a contract
    let getResources (rnd : System.Random) (conf : ConfigData) : int[] =
        let noOfRes = getNoOfRes rnd conf
        [| for i in 1..(noOfRes + 2) -> rnd.Next (0, conf.maxNoOfRes) |] 
        |> Array.distinct
        |> Array.sort


    // TODO: Account for the exchange rate - weak currency should have larger increment


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


    let getContract (rnd : System.Random) (conf : ConfigData) : ContractDescriptor = 
        let resoures = getResources rnd conf
        let baseAsset = resoures.[rnd.Next(0, resoures.Length)]
        let amount = conf.maxContractAmount * rnd.NextDouble()
        let nonPayingRate = (rnd.NextDouble (conf.maxBorrowingAnnualRate, conf.maxNonPayingAnnualRate)) / 365.0

        let overPayingRate = 0.0; // Update if needed

        let descriptors = 
            resoures
            |> Array.map (fun r -> getAssetDescriptor rnd conf r)
   
        {
            baseAsset = baseAsset
            amount = amount
            descriptors = descriptors
            nonPayingRate = nonPayingRate
            overPayingRate = overPayingRate
        }


    let getAllContracts (rnd : System.Random) (conf : ConfigData) : ContractDescriptor[] = 
       [| for i in 1..conf.noOfContracts -> getContract rnd conf |]
       

    // Interest rates on positive / negative balances
    let getAssetInterestRate (rnd : System.Random) (conf : ConfigData) : InterestRate = 
        let positiveRate = conf.maxIncomeAnnualRate * rnd.NextDouble() / 365.0
        let negativeRate = rnd.NextDouble (conf.maxIncomeAnnualRate, conf.maxBorrowingAnnualRate) / 365.0

        {
            incomeRate = positiveRate
            borrowingRate = negativeRate
        }


    let getInterestRates (rnd : System.Random) (conf : ConfigData) : InterestRate[] = 
       [| for i in 1..conf.noOfAssets -> getAssetInterestRate rnd conf |]


    let getAssetBalances (rnd : System.Random) (conf : ConfigData) : float[] = 
        [| for i in 1..conf.noOfAssets -> rnd.NextDouble (conf.minAmountOnHand, conf.maxAmountOnHand) |]


    let getAllData (rnd : System.Random) (conf : ConfigData) : AllData = 
        {
            exchangeRates = getExchangeRates rnd conf
            contracts = getAllContracts rnd conf
            interestRates = getInterestRates rnd conf
            balances = getAssetBalances rnd conf
        }


    let getExchangeRate (allData : AllData) (fromAsset : int) (toAsset : int) : float =  
        allData.exchangeRates.[fromAsset].[toAsset]

    // Amount in the base asset of the contract
    let getBaseAmount (allData : AllData) (amt : float) (deliveryAsset : int) (baseAsset : int) =
        amt * (getExchangeRate allData deliveryAsset baseAsset)


    // TODO failwith "" should be allData
    // Cost of contract in accounting currency
    // v is vector of ...
    let getContractCost (contr : ContractDescriptor)  (v : int[]) = 
        let baseAsset = contr.baseAsset
        let amtToPay = contr.amount
        let descr = contr.descriptors
   
        let len = descr.Length // v must have the same length
   
        // Paid amount in base asset of the contract
        let paid = 
            v 
            |> Array.zip descr
            |> Array.fold (fun acc (d, e) -> acc + (getBaseAmount (failwith "") (d.getDeliveryAmount e) d.asset baseAsset)) 0.0
   
        let netAmt = amtToPay - paid
        let baseInc = -netAmt * (if netAmt > 0.0 then contr.nonPayingRate else contr.overPayingRate)
   
        // Amount in accounting currency
        let retVal = baseInc * getExchangeRate (failwith "") baseAsset ConfigData.baseAccountingAsset

        retVal




    
