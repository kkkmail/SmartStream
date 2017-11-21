printfn "Starting..."

#r @".\bin\Debug\AssetOpt.dll"
open AssetOpt
open AssetOpt.DataGenerator

let sep = "\n=========================================="

let rnd = new System.Random (1)

let conf = ConfigData.defaultValue
//printfn "conf = %A" conf

let allData = getAllData rnd conf


let getExchangeRate (fromAsset : int) (toAsset : int) : float =  
    allData.exchangeRates.[fromAsset].[toAsset]


let payContract (contr : ContractDescriptor) (asset : int) (availableFunds : float) : (ContractDescriptor * float) = 
    match contr.descriptors |> Array.tryFind (fun d -> d.asset = asset) with
    | Some d -> 
        let rate = getExchangeRate contr.baseAsset asset

        (* paid in asset, not in baseAsset *)
        let paid =   
            let canPayUpTo = (min availableFunds (contr.amount / rate))

            if canPayUpTo < d.minVal 
            then 0.0 
            else (floor ((canPayUpTo - d.minVal) / d.incr)) * d.incr + d.minVal

        ({ contr with amount = contr.amount - paid * rate }, availableFunds - paid)
    | None -> (contr, availableFunds)


let getContractCost (contr : ContractDescriptor) : float = 
    contr.amount * contr.nonPayingRate * (getExchangeRate contr.baseAsset ConfigData.baseAccountingAsset)


printfn "%s\nallData.interestRates = %A" sep allData.interestRates

let sortedRates =
    allData.interestRates 
    |> Array.zip allData.assets
    |> Array.sortBy (fun (a, i) -> i.incomeRate)


let sortedContracts = 
    allData.contracts
    |> Array.sortBy (fun c -> -c.nonPayingRate)


printfn "%s\nsortedRates" sep
sortedRates |> Array.map (fun e -> printfn "    %A" e)

printfn "%s\nsortedContracts" sep
sortedContracts |> Array.map (fun e -> printfn "    %A" e)

//printfn "allData.contracts = %A" allData.contracts

printfn "Pay most expensive contract with most useless asset."
let c = sortedContracts.[0]
printfn "%s\nMost expensive contract:\n %A" sep c

let a = 