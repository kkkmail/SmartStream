namespace AssetOpt

module TestData_03 = 
    // All generated, DO NOT MODIFY !!!

    let conf = 
        {   
            ConfigData.defaultValue with
                noOfContracts = 20
                noOfAssets = 6
                rescaleRates = true
        }


    let exchangeRates : float[][] = [| for i in 1..conf.noOfContracts -> [| for j in 1..conf.noOfContracts -> 1.0 |] |]

// { asset = 000; minVal = 000.0; incr = 000.0 }
    let contracts : ContractDescriptor[] = 
        [|
            { contractID = 0; baseAsset = 5; amount = 1699.; descriptors = [| { asset = 3; minVal = 30.; incr = 5. }; { asset = 5; minVal = 20.; incr = 5. };  |]; nonPayingRate = 138. }
            { contractID = 1; baseAsset = 2; amount = 2249.; descriptors = [| { asset = 0; minVal = 300.; incr = 50. }; { asset = 2; minVal = 400.; incr = 50. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 4; minVal = 200.; incr = 25. }; { asset = 5; minVal = 60.; incr = 10. };  |]; nonPayingRate = 101. }
            { contractID = 2; baseAsset = 2; amount = 360.; descriptors = [| { asset = 1; minVal = 100.; incr = 10. }; { asset = 2; minVal = 500.; incr = 50. };  |]; nonPayingRate = 162. }
            { contractID = 3; baseAsset = 0; amount = 263.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 80.; incr = 10. }; { asset = 2; minVal = 60.; incr = 10. };  |]; nonPayingRate = 130. }
            { contractID = 4; baseAsset = 0; amount = 2616.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 1; minVal = 200.; incr = 25. }; { asset = 2; minVal = 400.; incr = 50. }; { asset = 4; minVal = 50.; incr = 5. };  |]; nonPayingRate = 131. }
            { contractID = 5; baseAsset = 4; amount = 62.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 2; minVal = 250.; incr = 25. }; { asset = 3; minVal = 300.; incr = 50. }; { asset = 4; minVal = 500.; incr = 50. }; { asset = 5; minVal = 400.; incr = 100. };  |]; nonPayingRate = 190. }
            { contractID = 6; baseAsset = 4; amount = 2506.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 1; minVal = 200.; incr = 25. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 4; minVal = 200.; incr = 50. }; { asset = 5; minVal = 200.; incr = 25. };  |]; nonPayingRate = 139. }
            { contractID = 7; baseAsset = 5; amount = 742.; descriptors = [| { asset = 0; minVal = 250.; incr = 25. }; { asset = 1; minVal = 250.; incr = 25. }; { asset = 3; minVal = 400.; incr = 100. }; { asset = 5; minVal = 400.; incr = 50. };  |]; nonPayingRate = 184. }
            { contractID = 8; baseAsset = 2; amount = 1716.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 300.; incr = 50. }; { asset = 2; minVal = 300.; incr = 50. }; { asset = 3; minVal = 200.; incr = 25. }; { asset = 5; minVal = 200.; incr = 50. };  |]; nonPayingRate = 150. }
            { contractID = 9; baseAsset = 4; amount = 233.; descriptors = [| { asset = 0; minVal = 300.; incr = 50. }; { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 80.; incr = 10. }; { asset = 3; minVal = 200.; incr = 25. }; { asset = 4; minVal = 300.; incr = 50. };  |]; nonPayingRate = 153. }
            { contractID = 10; baseAsset = 4; amount = 2940.; descriptors = [| { asset = 1; minVal = 40.; incr = 5. }; { asset = 2; minVal = 20.; incr = 5. }; { asset = 3; minVal = 100.; incr = 25. }; { asset = 4; minVal = 500.; incr = 50. };  |]; nonPayingRate = 155. }
            { contractID = 11; baseAsset = 3; amount = 2011.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 1; minVal = 150.; incr = 25. }; { asset = 2; minVal = 100.; incr = 25. }; { asset = 3; minVal = 400.; incr = 100. };  |]; nonPayingRate = 146. }
            { contractID = 12; baseAsset = 3; amount = 2367.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 4; minVal = 600.; incr = 100. }; { asset = 5; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 159. }
            { contractID = 13; baseAsset = 2; amount = 893.; descriptors = [| { asset = 0; minVal = 200.; incr = 50. }; { asset = 1; minVal = 100.; incr = 25. }; { asset = 2; minVal = 30.; incr = 5. }; { asset = 4; minVal = 200.; incr = 25. };  |]; nonPayingRate = 199. }
            { contractID = 14; baseAsset = 1; amount = 651.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 1; minVal = 80.; incr = 10. }; { asset = 3; minVal = 60.; incr = 10. };  |]; nonPayingRate = 149. }
            { contractID = 15; baseAsset = 3; amount = 2360.; descriptors = [| { asset = 0; minVal = 40.; incr = 10. }; { asset = 1; minVal = 300.; incr = 50. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 4; minVal = 100.; incr = 25. }; { asset = 5; minVal = 20.; incr = 5. };  |]; nonPayingRate = 113. }
            { contractID = 16; baseAsset = 4; amount = 1958.; descriptors = [| { asset = 2; minVal = 20.; incr = 5. }; { asset = 4; minVal = 40.; incr = 10. };  |]; nonPayingRate = 181. }
            { contractID = 17; baseAsset = 2; amount = 1419.; descriptors = [| { asset = 2; minVal = 200.; incr = 50. };  |]; nonPayingRate = 158. }
            { contractID = 18; baseAsset = 4; amount = 762.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 1; minVal = 250.; incr = 25. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 4; minVal = 400.; incr = 50. }; { asset = 5; minVal = 200.; incr = 50. };  |]; nonPayingRate = 200. }
            { contractID = 19; baseAsset = 0; amount = 821.; descriptors = [| { asset = 0; minVal = 100.; incr = 25. }; { asset = 2; minVal = 250.; incr = 25. }; { asset = 3; minVal = 40.; incr = 10. }; { asset = 5; minVal = 600.; incr = 100. };  |]; nonPayingRate = 147. }
        |]


    let positions : PositionData[] = 
        [|
            { asset = 0; balance = 12500.; incomeRate = 90. }
            { asset = 1; balance = 1088.; incomeRate = 97. }
            { asset = 2; balance = 6940.; incomeRate = 56. }
            { asset = 3; balance = 11222.; incomeRate = 7. }
            { asset = 4; balance = 8048.; incomeRate = 35. }
            { asset = 5; balance = 8645.; incomeRate = 44. }
        |]


    let allData = 
        {
            conf = conf
            exchangeRates = exchangeRates
            contracts = contracts
            positions = positions
        }

