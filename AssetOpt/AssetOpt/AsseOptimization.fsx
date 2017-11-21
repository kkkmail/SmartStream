﻿printfn "Starting..."

#r @".\bin\Debug\AssetOpt.dll"
open AssetOpt
open AssetOpt.DataGenerator

let printResults = true

let sep = "\n=========================================="

let rnd = new System.Random (1)

let conf = 
    {   
        ConfigData.defaultValue with
            //noOfContracts = 10000
            //noOfAssets = 100

            //minNoOfAssets = 10
            //maxNoOfAssets = 10

            //maxAmountOnHand = 2500000.0

            rescaleRates = true
    }

printfn "conf = %A" conf

let allData = getAllData rnd conf


let getExchangeRate (fromAsset : int) (toAsset : int) : float =  
    allData.exchangeRates.[fromAsset].[toAsset]


let payContract (contr : ContractDescriptor) (position : PositionData) : (ContractDescriptor * PositionData) = 
    match contr.descriptors |> Array.tryFind (fun d -> d.asset = position.asset) with
    | Some d -> 
        let rate = getExchangeRate contr.baseAsset position.asset

        (* paid in asset, not in baseAsset *)
        let paid =   
            let canPayUpTo = (min position.balance (contr.amount / rate))

            if canPayUpTo < d.minVal 
            then 0.0 
            else (floor ((canPayUpTo - d.minVal) / d.incr)) * d.incr + d.minVal

        ( { contr with amount = contr.amount - paid * rate }, { position with balance = position.balance - paid } )
    | None -> (contr, position)


let getContractCost (contr : ContractDescriptor) : float = 
    contr.amount * contr.nonPayingRate * (getExchangeRate contr.baseAsset ConfigData.baseAccountingAsset)


let getPositionIncome (position : PositionData) : float = 
    position.balance * position.incomeRate * (getExchangeRate position.asset ConfigData.baseAccountingAsset)


let getAllContractsCost (contracts : ContractDescriptor[]) = 
    contracts |> Array.fold (fun acc c -> acc + getContractCost c) 0.0


let getAllPositionsIncome (positions : PositionData[]) = 
    positions |> Array.fold (fun acc p -> acc + getPositionIncome p) 0.0


let payAllContracts (contracts : ContractDescriptor[], position : PositionData) : (ContractDescriptor[] * PositionData) = 
    let allPaid, newPos = 
        contracts
        |> Array.fold (fun 
                        (paidContracts : List<ContractDescriptor>, currentPosition : PositionData) c -> 
                            let paid, updatedPosition = payContract c currentPosition
                            (paid :: paidContracts), updatedPosition
                        ) ([], position)

    (allPaid |> List.rev |> List.toArray, newPos)


let payAll (contracts : ContractDescriptor[], positions : PositionData[]) : (ContractDescriptor[] * PositionData[]) =
    let newContracts, newPositions = 
        positions
        |> Array.fold (fun 
                        (paidContracts :  ContractDescriptor[], updatedPositions : List<PositionData>) p -> 
                            let newPaidContracts, newPos = payAllContracts (paidContracts, p)
                            newPaidContracts, (newPos :: updatedPositions)
                        ) (contracts, [])

    (newContracts, newPositions |> List.rev |> List.toArray)


let getNetBalance (constracts : ContractDescriptor[]) (positions : PositionData[]) = 
    (getAllPositionsIncome positions) - 
    (getAllContractsCost constracts)


let sortedContracts = 
    allData.contracts
    |> Array.sortBy (fun c -> -c.nonPayingRate)


let sortedPositions =
    allData.positions 
    |> Array.sortBy (fun p -> p.incomeRate)


if printResults then 
    printfn "%s\nallData.positions = %A" sep allData.positions

    printfn "%s\nsortedContracts" sep
    sortedContracts |> Array.map (fun e -> printfn "    %A" e) |> ignore

    printfn "%s\nsortedPositions" sep
    sortedPositions |> Array.map (fun e -> printfn "    %A" e) |> ignore

    printfn "Pay most expensive contract with most useless asset."
    let c = sortedContracts.[0]
    printfn "%s\nMost expensive contract:\n %A" sep c

    let p = sortedPositions.[0]
    printfn "%s\nMost useless asset:\n %A" sep p

    let c1, p1 = payContract c p

    printfn "%s\nMost expensive contract (paid):\n %A" sep c1
    printfn "%s\nMost useless asset (paid contract):\n %A" sep p1


printfn "%s\nPaying contracts..." sep

#time
let paid, newPos = payAll (sortedContracts, sortedPositions)
#time


if printResults then 
    printfn "%s\npaid" sep
    paid |> Array.map (fun e -> printfn "    %A" e) |> ignore

    printfn "%s\nnewPos = %A" sep newPos


let startingBalance = getNetBalance sortedContracts sortedPositions
let endingBalance = getNetBalance paid newPos

if (abs startingBalance) > 1000000.0  then 
    printfn "%s\nstartingBalance = %AM, endingBalance = %AM" sep (startingBalance / 1000000.0) (endingBalance / 1000000.0)
else    
    printfn "%s\nstartingBalance = %A, endingBalance = %A" sep startingBalance endingBalance

printfn "sortedContracts.Lengtn = %A, sortedPositions.Length = %A" sortedContracts.Length sortedPositions.Length

