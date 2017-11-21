﻿namespace AssetOpt

type AssetDescriptor = 
    {
        asset : int
        minVal : float
        incr : float
    }
with
    // valueID - ID of how much to pay
    // Returns amount of the asset used for settlement
    member rd.getDeliveryAmount (valueID : int) =
        if valueID > 0 then rd.minVal + rd.incr * (float (valueID - 1)) else 0.0


type ContractDescriptor = 
    {
        baseAsset : int
        amount : float
        descriptors : AssetDescriptor[]
        nonPayingRate : float
        overPayingRate : float
    }


type InterestRate = 
    {
        incomeRate : float
        borrowingRate : float
    }


type AllData = 
    {
        exchangeRates : float[][]
        contracts : ContractDescriptor[]
        interestRates : InterestRate[]
        balances : float[]
    }


