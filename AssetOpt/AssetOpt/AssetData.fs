namespace AssetOpt

type AssetDescriptor = 
    {
        asset : int
        minVal : float
        incr : float
    }
//with
//    // valueID - ID of how much to pay
//    // Returns amount of the asset used for settlement
//    member rd.getDeliveryAmount (valueID : int) =
//        if valueID > 0 then rd.minVal + rd.incr * (float (valueID - 1)) else 0.0


type ContractDescriptor = 
    {
        contractID : int
        baseAsset : int
        amount : float
        descriptors : AssetDescriptor[]
        nonPayingRate : float
        //overPayingRate : float
    }


type PositionData = 
    {
        asset : int
        incomeRate : float
        balance : float
    }


type AllData = 
    {
        conf : ConfigData
        exchangeRates : float[][]
        contracts : ContractDescriptor[]
        positions : PositionData[]
    }


