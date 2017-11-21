namespace AssetOpt

type AssetOptimizer (allData : AllData) = 
    let getExchangeRate (fromAsset : int) (toAsset : int) : float =  
        allData.exchangeRates.[fromAsset].[toAsset]


    // Amount in the base asset of the contract
    let getBaseAmount (amt : float) (deliveryAsset : int) (baseAsset : int) =
        amt * (getExchangeRate deliveryAsset baseAsset)


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
            |> Array.fold (fun acc (d, e) -> acc + (getBaseAmount (d.getDeliveryAmount e) d.asset baseAsset)) 0.0
   
        let netAmt = amtToPay - paid
        let baseInc = -netAmt * (if netAmt > 0.0 then contr.nonPayingRate else contr.overPayingRate)
   
        // Amount in accounting currency
        let retVal = baseInc * getExchangeRate baseAsset ConfigData.baseAccountingAsset

        retVal


    // Calculate changes to position due to settlement of a given contract
    let getPositionChange (contr : ContractDescriptor)  (v : int[]) : float[] = 
        let m = [| for i in 0..allData.conf.noOfAssets - 1 -> (1, 0.0) |] |> Map.ofArray

        v
        |> Array.zip contr.descriptors
        |> Array.fold (fun (acc : Map<int, float>) (d, i) -> acc.Add(d.asset, acc.[d.asset] - (d.getDeliveryAmount i))) m
        |> Map.toArray
        |> Array.sortBy (fun e -> fst e)
        |> Array.map (fun e -> snd e)
        
        


