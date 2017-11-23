﻿namespace AssetOpt

module TestData_02 = 
    // All generated, DO NOT MODIFY !!!

    let conf = 
        {   
            ConfigData.defaultValue with
                noOfContracts = 100
                noOfAssets = 10
                rescaleRates = true
        }


    let exchangeRates : float[][] = [| for i in 1..conf.noOfContracts -> [| for j in 1..conf.noOfContracts -> 1.0 |] |]

// { asset = 000; minVal = 000.0; incr = 000.0 }
    let contracts : ContractDescriptor[] = 
        [|
            { contractID = 0; baseAsset = 5; amount = 193.; descriptors = [| { asset = 4; minVal = 200.; incr = 50. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 6; minVal = 80.; incr = 10. };  |]; nonPayingRate = 148. }
            { contractID = 1; baseAsset = 8; amount = 1161.; descriptors = [| { asset = 1; minVal = 1000.; incr = 100. }; { asset = 4; minVal = 60.; incr = 10. }; { asset = 5; minVal = 40.; incr = 10. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 8; minVal = 30.; incr = 5. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 105. }
            { contractID = 2; baseAsset = 0; amount = 654.; descriptors = [| { asset = 0; minVal = 20.; incr = 5. }; { asset = 2; minVal = 400.; incr = 100. };  |]; nonPayingRate = 150. }
            { contractID = 3; baseAsset = 3; amount = 1685.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 1; minVal = 400.; incr = 50. }; { asset = 3; minVal = 40.; incr = 5. }; { asset = 7; minVal = 20.; incr = 5. }; { asset = 8; minVal = 30.; incr = 5. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 157. }
            { contractID = 4; baseAsset = 6; amount = 2639.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 1; minVal = 800.; incr = 100. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 5; minVal = 150.; incr = 25. }; { asset = 6; minVal = 150.; incr = 25. }; { asset = 7; minVal = 600.; incr = 100. }; { asset = 8; minVal = 80.; incr = 10. };  |]; nonPayingRate = 157. }
            { contractID = 5; baseAsset = 7; amount = 1994.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 7; minVal = 50.; incr = 5. }; { asset = 9; minVal = 300.; incr = 50. };  |]; nonPayingRate = 121. }
            { contractID = 6; baseAsset = 1; amount = 1523.; descriptors = [| { asset = 1; minVal = 100.; incr = 10. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 5; minVal = 100.; incr = 10. }; { asset = 9; minVal = 40.; incr = 10. };  |]; nonPayingRate = 122. }
            { contractID = 7; baseAsset = 2; amount = 2702.; descriptors = [| { asset = 2; minVal = 50.; incr = 5. }; { asset = 6; minVal = 150.; incr = 25. }; { asset = 7; minVal = 20.; incr = 5. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 153. }
            { contractID = 8; baseAsset = 7; amount = 2685.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 1; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 5; minVal = 100.; incr = 25. }; { asset = 7; minVal = 600.; incr = 100. }; { asset = 8; minVal = 200.; incr = 50. }; { asset = 9; minVal = 600.; incr = 100. };  |]; nonPayingRate = 194. }
            { contractID = 9; baseAsset = 7; amount = 1641.; descriptors = [| { asset = 4; minVal = 50.; incr = 5. }; { asset = 6; minVal = 20.; incr = 5. }; { asset = 7; minVal = 100.; incr = 25. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 132. }
            { contractID = 10; baseAsset = 3; amount = 1207.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 300.; incr = 50. }; { asset = 3; minVal = 300.; incr = 50. }; { asset = 8; minVal = 100.; incr = 25. }; { asset = 9; minVal = 250.; incr = 25. };  |]; nonPayingRate = 120. }
            { contractID = 11; baseAsset = 7; amount = 2373.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 2; minVal = 40.; incr = 10. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 7; minVal = 80.; incr = 10. }; { asset = 8; minVal = 800.; incr = 100. }; { asset = 9; minVal = 20.; incr = 5. };  |]; nonPayingRate = 161. }
            { contractID = 12; baseAsset = 9; amount = 991.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 2; minVal = 200.; incr = 50. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 6; minVal = 60.; incr = 10. }; { asset = 9; minVal = 600.; incr = 100. };  |]; nonPayingRate = 175. }
            { contractID = 13; baseAsset = 1; amount = 1042.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 1; minVal = 100.; incr = 10. }; { asset = 2; minVal = 30.; incr = 5. }; { asset = 3; minVal = 50.; incr = 5. }; { asset = 4; minVal = 400.; incr = 50. }; { asset = 5; minVal = 250.; incr = 25. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 127. }
            { contractID = 14; baseAsset = 0; amount = 2805.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 150.; incr = 25. }; { asset = 4; minVal = 250.; incr = 25. };  |]; nonPayingRate = 179. }
            { contractID = 15; baseAsset = 2; amount = 2438.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 2; minVal = 200.; incr = 25. };  |]; nonPayingRate = 154. }
            { contractID = 16; baseAsset = 3; amount = 2209.; descriptors = [| { asset = 1; minVal = 400.; incr = 100. }; { asset = 2; minVal = 30.; incr = 5. }; { asset = 3; minVal = 200.; incr = 50. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 5; minVal = 300.; incr = 50. }; { asset = 7; minVal = 30.; incr = 5. }; { asset = 8; minVal = 150.; incr = 25. }; { asset = 9; minVal = 400.; incr = 100. };  |]; nonPayingRate = 149. }
            { contractID = 17; baseAsset = 2; amount = 2438.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 60.; incr = 10. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 4; minVal = 400.; incr = 100. }; { asset = 5; minVal = 200.; incr = 25. }; { asset = 6; minVal = 200.; incr = 50. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 119. }
            { contractID = 18; baseAsset = 4; amount = 2015.; descriptors = [| { asset = 2; minVal = 600.; incr = 100. }; { asset = 4; minVal = 800.; incr = 100. }; { asset = 7; minVal = 50.; incr = 5. }; { asset = 8; minVal = 1000.; incr = 100. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 143. }
            { contractID = 19; baseAsset = 2; amount = 1391.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 1; minVal = 30.; incr = 5. }; { asset = 2; minVal = 100.; incr = 25. }; { asset = 5; minVal = 250.; incr = 25. }; { asset = 7; minVal = 400.; incr = 100. }; { asset = 9; minVal = 20.; incr = 5. };  |]; nonPayingRate = 137. }
            { contractID = 20; baseAsset = 5; amount = 357.; descriptors = [| { asset = 2; minVal = 600.; incr = 100. }; { asset = 4; minVal = 100.; incr = 25. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 6; minVal = 300.; incr = 50. };  |]; nonPayingRate = 196. }
            { contractID = 21; baseAsset = 0; amount = 573.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 167. }
            { contractID = 22; baseAsset = 4; amount = 1992.; descriptors = [| { asset = 3; minVal = 250.; incr = 25. }; { asset = 4; minVal = 300.; incr = 50. };  |]; nonPayingRate = 179. }
            { contractID = 23; baseAsset = 3; amount = 428.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 3; minVal = 500.; incr = 50. }; { asset = 4; minVal = 200.; incr = 50. }; { asset = 6; minVal = 300.; incr = 50. }; { asset = 8; minVal = 200.; incr = 50. }; { asset = 9; minVal = 500.; incr = 50. };  |]; nonPayingRate = 167. }
            { contractID = 24; baseAsset = 6; amount = 86.; descriptors = [| { asset = 1; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 6; minVal = 50.; incr = 5. }; { asset = 8; minVal = 600.; incr = 100. };  |]; nonPayingRate = 141. }
            { contractID = 25; baseAsset = 0; amount = 1145.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 5; minVal = 30.; incr = 5. }; { asset = 6; minVal = 400.; incr = 50. }; { asset = 8; minVal = 20.; incr = 5. };  |]; nonPayingRate = 180. }
            { contractID = 26; baseAsset = 4; amount = 508.; descriptors = [| { asset = 1; minVal = 200.; incr = 25. }; { asset = 3; minVal = 50.; incr = 5. }; { asset = 4; minVal = 250.; incr = 25. };  |]; nonPayingRate = 173. }
            { contractID = 27; baseAsset = 6; amount = 2013.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 5; minVal = 40.; incr = 10. }; { asset = 6; minVal = 50.; incr = 5. }; { asset = 7; minVal = 400.; incr = 50. }; { asset = 8; minVal = 50.; incr = 5. };  |]; nonPayingRate = 156. }
            { contractID = 28; baseAsset = 4; amount = 1858.; descriptors = [| { asset = 0; minVal = 200.; incr = 50. }; { asset = 1; minVal = 150.; incr = 25. }; { asset = 2; minVal = 500.; incr = 50. }; { asset = 3; minVal = 200.; incr = 25. }; { asset = 4; minVal = 600.; incr = 100. }; { asset = 7; minVal = 100.; incr = 25. }; { asset = 8; minVal = 600.; incr = 100. };  |]; nonPayingRate = 123. }
            { contractID = 29; baseAsset = 3; amount = 1095.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 100.; incr = 25. };  |]; nonPayingRate = 193. }
            { contractID = 30; baseAsset = 0; amount = 718.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 2; minVal = 40.; incr = 5. }; { asset = 4; minVal = 500.; incr = 50. }; { asset = 5; minVal = 500.; incr = 50. }; { asset = 6; minVal = 150.; incr = 25. }; { asset = 9; minVal = 600.; incr = 100. };  |]; nonPayingRate = 133. }
            { contractID = 31; baseAsset = 9; amount = 2522.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 2; minVal = 1000.; incr = 100. }; { asset = 7; minVal = 600.; incr = 100. }; { asset = 8; minVal = 400.; incr = 50. }; { asset = 9; minVal = 20.; incr = 5. };  |]; nonPayingRate = 177. }
            { contractID = 32; baseAsset = 6; amount = 1651.; descriptors = [| { asset = 6; minVal = 40.; incr = 5. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 187. }
            { contractID = 33; baseAsset = 5; amount = 1424.; descriptors = [| { asset = 3; minVal = 40.; incr = 5. }; { asset = 5; minVal = 60.; incr = 10. }; { asset = 6; minVal = 80.; incr = 10. }; { asset = 7; minVal = 30.; incr = 5. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 156. }
            { contractID = 34; baseAsset = 7; amount = 2618.; descriptors = [| { asset = 2; minVal = 200.; incr = 50. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 7; minVal = 200.; incr = 25. };  |]; nonPayingRate = 169. }
            { contractID = 35; baseAsset = 8; amount = 897.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 1; minVal = 400.; incr = 50. }; { asset = 2; minVal = 200.; incr = 50. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 8; minVal = 40.; incr = 5. };  |]; nonPayingRate = 125. }
            { contractID = 36; baseAsset = 2; amount = 2855.; descriptors = [| { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 30.; incr = 5. }; { asset = 8; minVal = 60.; incr = 10. };  |]; nonPayingRate = 150. }
            { contractID = 37; baseAsset = 7; amount = 883.; descriptors = [| { asset = 7; minVal = 40.; incr = 5. }; { asset = 8; minVal = 200.; incr = 25. };  |]; nonPayingRate = 123. }
            { contractID = 38; baseAsset = 8; amount = 1292.; descriptors = [| { asset = 1; minVal = 100.; incr = 25. }; { asset = 3; minVal = 200.; incr = 25. }; { asset = 7; minVal = 200.; incr = 25. }; { asset = 8; minVal = 80.; incr = 10. };  |]; nonPayingRate = 101. }
            { contractID = 39; baseAsset = 0; amount = 1910.; descriptors = [| { asset = 0; minVal = 30.; incr = 5. }; { asset = 8; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 114. }
            { contractID = 40; baseAsset = 7; amount = 2920.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 2; minVal = 300.; incr = 50. }; { asset = 4; minVal = 100.; incr = 10. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 6; minVal = 600.; incr = 100. }; { asset = 7; minVal = 60.; incr = 10. }; { asset = 8; minVal = 40.; incr = 10. }; { asset = 9; minVal = 20.; incr = 5. };  |]; nonPayingRate = 169. }
            { contractID = 41; baseAsset = 6; amount = 2758.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 2; minVal = 500.; incr = 50. }; { asset = 3; minVal = 1000.; incr = 100. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 6; minVal = 80.; incr = 10. }; { asset = 8; minVal = 200.; incr = 25. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 181. }
            { contractID = 42; baseAsset = 7; amount = 906.; descriptors = [| { asset = 0; minVal = 100.; incr = 25. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 7; minVal = 200.; incr = 50. }; { asset = 9; minVal = 300.; incr = 50. };  |]; nonPayingRate = 145. }
            { contractID = 43; baseAsset = 5; amount = 2681.; descriptors = [| { asset = 1; minVal = 400.; incr = 50. }; { asset = 3; minVal = 40.; incr = 10. }; { asset = 5; minVal = 80.; incr = 10. }; { asset = 6; minVal = 300.; incr = 50. }; { asset = 9; minVal = 500.; incr = 50. };  |]; nonPayingRate = 128. }
            { contractID = 44; baseAsset = 2; amount = 1270.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 2; minVal = 100.; incr = 10. }; { asset = 7; minVal = 40.; incr = 10. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 167. }
            { contractID = 45; baseAsset = 3; amount = 1445.; descriptors = [| { asset = 0; minVal = 250.; incr = 25. }; { asset = 1; minVal = 500.; incr = 50. }; { asset = 2; minVal = 60.; incr = 10. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 4; minVal = 200.; incr = 50. }; { asset = 5; minVal = 600.; incr = 100. };  |]; nonPayingRate = 146. }
            { contractID = 46; baseAsset = 5; amount = 2150.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 2; minVal = 50.; incr = 5. }; { asset = 4; minVal = 500.; incr = 50. }; { asset = 5; minVal = 30.; incr = 5. }; { asset = 7; minVal = 200.; incr = 50. }; { asset = 9; minVal = 200.; incr = 50. };  |]; nonPayingRate = 190. }
            { contractID = 47; baseAsset = 1; amount = 88.; descriptors = [| { asset = 1; minVal = 300.; incr = 50. }; { asset = 2; minVal = 200.; incr = 50. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 5; minVal = 100.; incr = 10. }; { asset = 6; minVal = 200.; incr = 25. }; { asset = 7; minVal = 500.; incr = 50. };  |]; nonPayingRate = 177. }
            { contractID = 48; baseAsset = 3; amount = 1356.; descriptors = [| { asset = 1; minVal = 40.; incr = 10. }; { asset = 2; minVal = 800.; incr = 100. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 4; minVal = 300.; incr = 50. }; { asset = 8; minVal = 100.; incr = 25. };  |]; nonPayingRate = 163. }
            { contractID = 49; baseAsset = 5; amount = 666.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 2; minVal = 20.; incr = 5. }; { asset = 5; minVal = 500.; incr = 50. }; { asset = 7; minVal = 500.; incr = 50. }; { asset = 8; minVal = 80.; incr = 10. };  |]; nonPayingRate = 100. }
            { contractID = 50; baseAsset = 3; amount = 794.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 5; minVal = 200.; incr = 50. }; { asset = 7; minVal = 300.; incr = 50. }; { asset = 8; minVal = 400.; incr = 100. };  |]; nonPayingRate = 194. }
            { contractID = 51; baseAsset = 2; amount = 2629.; descriptors = [| { asset = 2; minVal = 50.; incr = 5. }; { asset = 7; minVal = 300.; incr = 50. };  |]; nonPayingRate = 118. }
            { contractID = 52; baseAsset = 0; amount = 2710.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 1; minVal = 600.; incr = 100. }; { asset = 6; minVal = 400.; incr = 100. }; { asset = 7; minVal = 60.; incr = 10. }; { asset = 8; minVal = 150.; incr = 25. }; { asset = 9; minVal = 40.; incr = 10. };  |]; nonPayingRate = 131. }
            { contractID = 53; baseAsset = 3; amount = 2957.; descriptors = [| { asset = 3; minVal = 500.; incr = 50. }; { asset = 9; minVal = 80.; incr = 10. };  |]; nonPayingRate = 147. }
            { contractID = 54; baseAsset = 8; amount = 353.; descriptors = [| { asset = 0; minVal = 40.; incr = 10. }; { asset = 1; minVal = 40.; incr = 10. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 5; minVal = 1000.; incr = 100. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 7; minVal = 100.; incr = 10. }; { asset = 8; minVal = 80.; incr = 10. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 117. }
            { contractID = 55; baseAsset = 1; amount = 2623.; descriptors = [| { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 250.; incr = 25. }; { asset = 7; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 130. }
            { contractID = 56; baseAsset = 5; amount = 470.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 3; minVal = 1000.; incr = 100. }; { asset = 4; minVal = 150.; incr = 25. }; { asset = 5; minVal = 60.; incr = 10. }; { asset = 7; minVal = 30.; incr = 5. }; { asset = 8; minVal = 800.; incr = 100. }; { asset = 9; minVal = 800.; incr = 100. };  |]; nonPayingRate = 165. }
            { contractID = 57; baseAsset = 2; amount = 1980.; descriptors = [| { asset = 0; minVal = 250.; incr = 25. }; { asset = 1; minVal = 800.; incr = 100. }; { asset = 2; minVal = 500.; incr = 50. }; { asset = 3; minVal = 100.; incr = 25. }; { asset = 4; minVal = 400.; incr = 50. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 6; minVal = 100.; incr = 10. }; { asset = 7; minVal = 60.; incr = 10. }; { asset = 8; minVal = 100.; incr = 10. }; { asset = 9; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 155. }
            { contractID = 58; baseAsset = 4; amount = 2941.; descriptors = [| { asset = 4; minVal = 20.; incr = 5. }; { asset = 6; minVal = 100.; incr = 25. };  |]; nonPayingRate = 174. }
            { contractID = 59; baseAsset = 1; amount = 2928.; descriptors = [| { asset = 1; minVal = 250.; incr = 25. }; { asset = 3; minVal = 1000.; incr = 100. }; { asset = 6; minVal = 20.; incr = 5. }; { asset = 8; minVal = 40.; incr = 10. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 150. }
            { contractID = 60; baseAsset = 4; amount = 2274.; descriptors = [| { asset = 1; minVal = 250.; incr = 25. }; { asset = 4; minVal = 800.; incr = 100. }; { asset = 6; minVal = 100.; incr = 10. }; { asset = 8; minVal = 250.; incr = 25. };  |]; nonPayingRate = 102. }
            { contractID = 61; baseAsset = 6; amount = 2666.; descriptors = [| { asset = 1; minVal = 300.; incr = 50. }; { asset = 6; minVal = 200.; incr = 25. };  |]; nonPayingRate = 176. }
            { contractID = 62; baseAsset = 6; amount = 1708.; descriptors = [| { asset = 2; minVal = 100.; incr = 25. }; { asset = 3; minVal = 200.; incr = 50. }; { asset = 4; minVal = 150.; incr = 25. }; { asset = 5; minVal = 400.; incr = 50. }; { asset = 6; minVal = 40.; incr = 10. }; { asset = 7; minVal = 20.; incr = 5. }; { asset = 8; minVal = 30.; incr = 5. };  |]; nonPayingRate = 147. }
            { contractID = 63; baseAsset = 7; amount = 1826.; descriptors = [| { asset = 1; minVal = 200.; incr = 25. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 7; minVal = 50.; incr = 5. };  |]; nonPayingRate = 143. }
            { contractID = 64; baseAsset = 9; amount = 665.; descriptors = [| { asset = 0; minVal = 400.; incr = 50. }; { asset = 2; minVal = 300.; incr = 50. }; { asset = 3; minVal = 100.; incr = 25. }; { asset = 8; minVal = 500.; incr = 50. }; { asset = 9; minVal = 800.; incr = 100. };  |]; nonPayingRate = 112. }
            { contractID = 65; baseAsset = 3; amount = 938.; descriptors = [| { asset = 1; minVal = 600.; incr = 100. }; { asset = 2; minVal = 250.; incr = 25. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 4; minVal = 150.; incr = 25. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 7; minVal = 100.; incr = 10. }; { asset = 8; minVal = 500.; incr = 50. };  |]; nonPayingRate = 159. }
            { contractID = 66; baseAsset = 7; amount = 2447.; descriptors = [| { asset = 3; minVal = 60.; incr = 10. }; { asset = 7; minVal = 400.; incr = 100. };  |]; nonPayingRate = 169. }
            { contractID = 67; baseAsset = 8; amount = 79.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 3; minVal = 400.; incr = 100. }; { asset = 4; minVal = 30.; incr = 5. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 7; minVal = 50.; incr = 5. }; { asset = 8; minVal = 150.; incr = 25. };  |]; nonPayingRate = 181. }
            { contractID = 68; baseAsset = 7; amount = 1253.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 60.; incr = 10. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 6; minVal = 300.; incr = 50. }; { asset = 7; minVal = 150.; incr = 25. }; { asset = 8; minVal = 40.; incr = 5. };  |]; nonPayingRate = 110. }
            { contractID = 69; baseAsset = 6; amount = 2762.; descriptors = [| { asset = 1; minVal = 40.; incr = 5. }; { asset = 2; minVal = 60.; incr = 10. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 5; minVal = 150.; incr = 25. }; { asset = 6; minVal = 50.; incr = 5. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 153. }
            { contractID = 70; baseAsset = 6; amount = 2504.; descriptors = [| { asset = 0; minVal = 30.; incr = 5. }; { asset = 2; minVal = 250.; incr = 25. }; { asset = 6; minVal = 1000.; incr = 100. }; { asset = 7; minVal = 40.; incr = 10. }; { asset = 8; minVal = 40.; incr = 5. };  |]; nonPayingRate = 119. }
            { contractID = 71; baseAsset = 0; amount = 357.; descriptors = [| { asset = 0; minVal = 800.; incr = 100. }; { asset = 6; minVal = 400.; incr = 100. };  |]; nonPayingRate = 178. }
            { contractID = 72; baseAsset = 8; amount = 1374.; descriptors = [| { asset = 0; minVal = 400.; incr = 50. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 7; minVal = 80.; incr = 10. }; { asset = 8; minVal = 300.; incr = 50. };  |]; nonPayingRate = 122. }
            { contractID = 73; baseAsset = 2; amount = 1906.; descriptors = [| { asset = 1; minVal = 40.; incr = 5. }; { asset = 2; minVal = 500.; incr = 50. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 5; minVal = 20.; incr = 5. };  |]; nonPayingRate = 124. }
            { contractID = 74; baseAsset = 5; amount = 484.; descriptors = [| { asset = 0; minVal = 500.; incr = 50. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 4; minVal = 400.; incr = 50. }; { asset = 5; minVal = 100.; incr = 10. }; { asset = 6; minVal = 400.; incr = 50. }; { asset = 8; minVal = 20.; incr = 5. }; { asset = 9; minVal = 20.; incr = 5. };  |]; nonPayingRate = 196. }
            { contractID = 75; baseAsset = 6; amount = 2003.; descriptors = [| { asset = 1; minVal = 600.; incr = 100. }; { asset = 2; minVal = 100.; incr = 25. }; { asset = 3; minVal = 500.; incr = 50. }; { asset = 6; minVal = 40.; incr = 5. }; { asset = 7; minVal = 80.; incr = 10. }; { asset = 8; minVal = 1000.; incr = 100. }; { asset = 9; minVal = 250.; incr = 25. };  |]; nonPayingRate = 131. }
            { contractID = 76; baseAsset = 9; amount = 103.; descriptors = [| { asset = 2; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 400.; incr = 100. }; { asset = 5; minVal = 400.; incr = 100. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 132. }
            { contractID = 77; baseAsset = 1; amount = 2059.; descriptors = [| { asset = 0; minVal = 20.; incr = 5. }; { asset = 1; minVal = 80.; incr = 10. }; { asset = 2; minVal = 80.; incr = 10. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 5; minVal = 40.; incr = 10. };  |]; nonPayingRate = 103. }
            { contractID = 78; baseAsset = 5; amount = 2189.; descriptors = [| { asset = 1; minVal = 100.; incr = 10. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 6; minVal = 150.; incr = 25. };  |]; nonPayingRate = 137. }
            { contractID = 79; baseAsset = 4; amount = 2860.; descriptors = [| { asset = 0; minVal = 60.; incr = 10. }; { asset = 4; minVal = 800.; incr = 100. }; { asset = 8; minVal = 100.; incr = 10. };  |]; nonPayingRate = 175. }
            { contractID = 80; baseAsset = 0; amount = 1010.; descriptors = [| { asset = 0; minVal = 20.; incr = 5. }; { asset = 2; minVal = 100.; incr = 25. }; { asset = 7; minVal = 150.; incr = 25. }; { asset = 8; minVal = 150.; incr = 25. };  |]; nonPayingRate = 101. }
            { contractID = 81; baseAsset = 0; amount = 1599.; descriptors = [| { asset = 0; minVal = 200.; incr = 50. }; { asset = 1; minVal = 400.; incr = 100. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 5; minVal = 400.; incr = 50. }; { asset = 6; minVal = 40.; incr = 10. }; { asset = 8; minVal = 250.; incr = 25. }; { asset = 9; minVal = 80.; incr = 10. };  |]; nonPayingRate = 194. }
            { contractID = 82; baseAsset = 8; amount = 1520.; descriptors = [| { asset = 0; minVal = 200.; incr = 50. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 8; minVal = 300.; incr = 50. };  |]; nonPayingRate = 136. }
            { contractID = 83; baseAsset = 9; amount = 2337.; descriptors = [| { asset = 1; minVal = 60.; incr = 10. }; { asset = 3; minVal = 300.; incr = 50. }; { asset = 4; minVal = 200.; incr = 50. }; { asset = 5; minVal = 60.; incr = 10. }; { asset = 6; minVal = 60.; incr = 10. }; { asset = 7; minVal = 60.; incr = 10. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 130. }
            { contractID = 84; baseAsset = 5; amount = 1720.; descriptors = [| { asset = 0; minVal = 200.; incr = 50. }; { asset = 2; minVal = 200.; incr = 50. }; { asset = 3; minVal = 40.; incr = 10. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 7; minVal = 100.; incr = 25. }; { asset = 8; minVal = 400.; incr = 100. };  |]; nonPayingRate = 110. }
            { contractID = 85; baseAsset = 7; amount = 1283.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 300.; incr = 50. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 5; minVal = 400.; incr = 100. }; { asset = 6; minVal = 100.; incr = 10. }; { asset = 7; minVal = 40.; incr = 10. }; { asset = 8; minVal = 30.; incr = 5. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 138. }
            { contractID = 86; baseAsset = 8; amount = 784.; descriptors = [| { asset = 0; minVal = 300.; incr = 50. }; { asset = 5; minVal = 30.; incr = 5. }; { asset = 6; minVal = 200.; incr = 25. }; { asset = 7; minVal = 100.; incr = 10. }; { asset = 8; minVal = 200.; incr = 25. };  |]; nonPayingRate = 109. }
            { contractID = 87; baseAsset = 4; amount = 724.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 1; minVal = 250.; incr = 25. }; { asset = 3; minVal = 100.; incr = 25. }; { asset = 4; minVal = 20.; incr = 5. }; { asset = 5; minVal = 200.; incr = 25. }; { asset = 6; minVal = 200.; incr = 25. }; { asset = 7; minVal = 400.; incr = 100. }; { asset = 8; minVal = 100.; incr = 25. }; { asset = 9; minVal = 600.; incr = 100. };  |]; nonPayingRate = 140. }
            { contractID = 88; baseAsset = 7; amount = 1164.; descriptors = [| { asset = 1; minVal = 800.; incr = 100. }; { asset = 5; minVal = 400.; incr = 100. }; { asset = 7; minVal = 400.; incr = 50. };  |]; nonPayingRate = 101. }
            { contractID = 89; baseAsset = 7; amount = 2307.; descriptors = [| { asset = 0; minVal = 60.; incr = 10. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 4; minVal = 150.; incr = 25. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 7; minVal = 250.; incr = 25. }; { asset = 8; minVal = 400.; incr = 100. };  |]; nonPayingRate = 170. }
            { contractID = 90; baseAsset = 7; amount = 2023.; descriptors = [| { asset = 5; minVal = 800.; incr = 100. }; { asset = 7; minVal = 100.; incr = 10. };  |]; nonPayingRate = 182. }
            { contractID = 91; baseAsset = 6; amount = 419.; descriptors = [| { asset = 1; minVal = 800.; incr = 100. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 9; minVal = 200.; incr = 25. };  |]; nonPayingRate = 186. }
            { contractID = 92; baseAsset = 1; amount = 198.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 1; minVal = 300.; incr = 50. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 4; minVal = 1000.; incr = 100. }; { asset = 6; minVal = 40.; incr = 10. }; { asset = 7; minVal = 20.; incr = 5. };  |]; nonPayingRate = 131. }
            { contractID = 93; baseAsset = 1; amount = 1140.; descriptors = [| { asset = 1; minVal = 60.; incr = 10. }; { asset = 2; minVal = 250.; incr = 25. }; { asset = 4; minVal = 200.; incr = 25. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 7; minVal = 600.; incr = 100. }; { asset = 8; minVal = 400.; incr = 50. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 127. }
            { contractID = 94; baseAsset = 2; amount = 871.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 2; minVal = 300.; incr = 50. }; { asset = 4; minVal = 400.; incr = 100. }; { asset = 7; minVal = 300.; incr = 50. }; { asset = 8; minVal = 200.; incr = 25. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 198. }
            { contractID = 95; baseAsset = 5; amount = 1789.; descriptors = [| { asset = 2; minVal = 300.; incr = 50. }; { asset = 3; minVal = 200.; incr = 50. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 8; minVal = 20.; incr = 5. }; { asset = 9; minVal = 200.; incr = 25. };  |]; nonPayingRate = 143. }
            { contractID = 96; baseAsset = 5; amount = 2116.; descriptors = [| { asset = 0; minVal = 30.; incr = 5. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 2; minVal = 200.; incr = 25. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 8; minVal = 100.; incr = 25. };  |]; nonPayingRate = 183. }
            { contractID = 97; baseAsset = 1; amount = 242.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 1; minVal = 1000.; incr = 100. }; { asset = 2; minVal = 60.; incr = 10. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 5; minVal = 30.; incr = 5. }; { asset = 6; minVal = 150.; incr = 25. }; { asset = 8; minVal = 50.; incr = 5. }; { asset = 9; minVal = 250.; incr = 25. };  |]; nonPayingRate = 161. }
            { contractID = 98; baseAsset = 8; amount = 116.; descriptors = [| { asset = 2; minVal = 40.; incr = 10. }; { asset = 3; minVal = 40.; incr = 5. }; { asset = 8; minVal = 600.; incr = 100. };  |]; nonPayingRate = 176. }
            { contractID = 99; baseAsset = 2; amount = 613.; descriptors = [| { asset = 0; minVal = 30.; incr = 5. }; { asset = 2; minVal = 600.; incr = 100. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 5; minVal = 40.; incr = 5. }; { asset = 7; minVal = 1000.; incr = 100. }; { asset = 8; minVal = 20.; incr = 5. }; { asset = 9; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 164. }
        |]


    let positions : PositionData[] = 
        [|
            { asset = 0; balance = 7237.; incomeRate = 54. }
            { asset = 1; balance = 6700.; incomeRate = 40. }
            { asset = 2; balance = 50497.; incomeRate = 93. }
            { asset = 3; balance = 40180.; incomeRate = 50. }
            { asset = 4; balance = 37400.; incomeRate = 6. }
            { asset = 5; balance = 43915.; incomeRate = 8. }
            { asset = 6; balance = 58895.; incomeRate = 81. }
            { asset = 7; balance = 2335.; incomeRate = 38. }
            { asset = 8; balance = 68369.; incomeRate = 80. }
            { asset = 9; balance = 14162.; incomeRate = 34. }
        |]


    let allData = 
        {
            conf = conf
            exchangeRates = exchangeRates
            contracts = contracts
            positions = positions
        }

