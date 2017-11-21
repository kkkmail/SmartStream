namespace AssetOpt

type ConfigData = 
    {
        noOfContracts : int
        noOfAssets : int

        incrValues : float[]
        minValMultipliers : float[]

        minRate : float
        maxRate : float

        maxIncomeAnnualRate : float
        //maxBorrowingAnnualRate : float
        maxNonPayingAnnualRate : float

        maxContractAmount : float

        amountMultiplier : float
        minAmountOnHand : float
        maxAmountOnHand : float

        rescaleRates : bool // Set to true to generate exchange rates = 1.0
        //useWeights : bool // Set to true to use weights instead of costs
    }
with
    static member defaultNoOfAssets : int = 3
    static member defaultNoOfContracts : int = 4
    static member defaultMaxContractAmount : float = 10000.0
    static member defaultAmountMultiplier : float = (float ConfigData.defaultNoOfContracts) * ConfigData.defaultMaxContractAmount / 2.0
    static member baseAccountingAsset = 0; // Asset in which perform all calculations.

    static member defaultValue : ConfigData = 
        {
            noOfContracts = ConfigData.defaultNoOfContracts
            noOfAssets = ConfigData.defaultNoOfAssets

            incrValues = [| 5.0; 10.0; 25.0; 50.0; 100.0 |]
            minValMultipliers = [| 4.0; 6.0; 8.0; 100.0 |]

            minRate = 0.01; (* Around JPY *)
            maxRate = 3.0; (* Around KWD / BHD / OMR *)

            maxIncomeAnnualRate = 100.0 ;
            //maxBorrowingAnnualRate = 3.0 ;
            maxNonPayingAnnualRate = 200.0 ;

            maxContractAmount = ConfigData.defaultMaxContractAmount;

            (* Adjust as necessary *)
            (* min/max amount of assets before settlement *)
            amountMultiplier = ConfigData.defaultAmountMultiplier

            minAmountOnHand = -ConfigData.defaultAmountMultiplier / 5.0
            maxAmountOnHand = ConfigData.defaultAmountMultiplier

            rescaleRates = true
            //useWeights = true
        }

    member this.minNoOfAssets : int = min 2 this.noOfAssets
    member this.maxNoOfAssets : int = min 10 this.noOfAssets


