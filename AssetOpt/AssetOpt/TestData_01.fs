namespace AssetOpt

module TestData_01 = 

    let conf = 
        {   
            ConfigData.defaultValue with
                noOfContracts = 6
                noOfAssets = 4

                //minNoOfAssets = 10
                //maxNoOfAssets = 10

                //maxAmountOnHand = 2500000.0

                rescaleRates = true
        }


    let exchangeRates : float[][] = [| for i in 1..conf.noOfContracts -> [| for j in 1..conf.noOfContracts -> 1.0 |] |]

// { asset = 000; minVal = 000.0; incr = 000.0 }
    let contracts : ContractDescriptor[] = 
        [|
            { contractID = 0; baseAsset = 2; amount = 565.0; descriptors = [| { asset = 1; minVal = 80.0; incr = 10.0 }; { asset = 2; minVal = 20.0; incr = 5.0 } |]; nonPayingRate = 113.0  }
            { contractID = 1; baseAsset = 0; amount = 752.0; descriptors = [| { asset = 0; minVal = 200.0; incr = 50.0 }; { asset = 1; minVal = 80.0; incr = 10.0 } |]; nonPayingRate = 145.0  }
            { contractID = 2; baseAsset = 2; amount = 2789.0; descriptors = [| { asset = 0; minVal = 60.0; incr = 10.0 }; { asset = 1; minVal = 100.0; incr = 25.0 }; { asset = 2; minVal = 40.0; incr = 5.0 } |]; nonPayingRate = 111.0  }
            { contractID = 3; baseAsset = 1; amount = 999.0; descriptors = [| { asset = 0; minVal = 80.0; incr = 10.0 }; { asset = 1; minVal = 400.0; incr = 100.0 }; { asset = 2; minVal = 40.0; incr = 5.0 }; { asset = 3; minVal = 30.0; incr = 5.0 } |]; nonPayingRate = 120.0  }
            { contractID = 4; baseAsset = 1; amount = 1585.0; descriptors = [| { asset = 1; minVal = 60.0; incr = 10.0 }; { asset = 2; minVal = 1000.0; incr = 100.0 } |]; nonPayingRate = 135.0  }
            { contractID = 5; baseAsset = 0; amount = 527.0; descriptors = [| { asset = 0; minVal = 600.0; incr = 100.0 }; { asset = 2; minVal = 100.0; incr = 25.0 } |]; nonPayingRate = 135.0  }
        |]


    let positions : PositionData[] = 
        [|
            { asset = 0; balance = 250.0; incomeRate = 97.0 }
            { asset = 1; balance = 431.0; incomeRate = 66.0 }
            { asset = 2; balance = 8441.0; incomeRate = 52.0 }
            { asset = 3; balance = 8319.0; incomeRate = 85.0 }
        |]


    let allData = 
        {
            conf = conf
            exchangeRates = exchangeRates
            contracts = contracts
            positions = positions
        }

