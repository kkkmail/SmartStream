printfn "Starting..."

#r @".\bin\Debug\AssetOpt.dll"
open AssetOpt
open AssetOpt.TestData_04

#time
let optimizer = AssetOptimizer (allData)
#time

//optimizer.printInformation ()
optimizer.printBalances ()
optimizer.printResults ()

