printfn "Starting..."

#r @".\bin\Debug\AssetOpt.dll"
open AssetOpt.Optimization

runOptimizer AssetOpt.TestData__a4_c6.allData
runOptimizer AssetOpt.TestData__a6_c20.allData
runOptimizer AssetOpt.TestData__a10_c100.allData
runOptimizer AssetOpt.TestData__a20_c200.allData

