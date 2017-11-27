In[152]:= (* ==============================================*)
ClearAll["Global`*"];
(* ==============================================*)
(* Set the same seedRandomValue for deterministic results *)
rndVal=RandomInteger[{0,10^12}];
seedRandomValue=rndVal;
seedRandomValue = 597543585369;
SeedRandom[seedRandomValue];
Print["seedRandomValue = ", seedRandomValue];
(* ==============================================*)
(* Parameters *)
noOfContracts =20;
noOfAssets = 10;

incrValues = {5,10,25,50,100};
minValMultipliers = {4,6,8,10};

minRate = 0.01; (* Around JPY *)
maxRate = 3; (* Around KWD / BHD / OMR *)

maxIncomeAnnualRate = 1.0 ;
maxBorrowingAnnualRate = 3.0 ;
maxNonPayingAnnualRate = 10.0 ;

maxContractAmount = 1000;

(* Adjust as necessary *)
(* min/max amount of assets before settlement *)
amountMultiplier = noOfContracts * maxContractAmount / 2;
minAmountOnHand = -(amountMultiplier/5);
maxAmountOnHand = amountMultiplier;
(* ==============================================*)
minNoOfRes = Min[2, noOfAssets];
maxNoOfRes =  Min[10, noOfAssets];
baseAccountingAsset = 1; (* Asset in which perform all calculations. *)
(* ==============================================*)
(* Some legacy stuff... *)
(* Sorts the second list using the normal ordering of the first one. *)
sortByFirst[lstOrder : {__}, lstToBeSorted : {__}] := Module[{retVal, nn, lst, lstSorted},
nn = Length[lstOrder];

If[Length[lstToBeSorted] != nn,
(
Print["sortByFirst::Lists have different length!"];
Return[Indeterminate];
)
];

lst = Table[{lstOrder[[ii]], lstToBeSorted[[ii]]}, {ii, 1, nn}];
lstSorted = SortBy[lst, First];
retVal = Table[lstSorted[[ii, 2]], {ii, 1, nn}];
Return[retVal];
];

strSeparator = "=============================================" <> FromCharacterCode[10];
strSeparatorSmall = "---------------------------------------------";
strCRLF = FromCharacterCode[10];
tStart = AbsoluteTime[];
tMid = tStart;

printTimeUsed[showTime_?BooleanQ] := Module[{tEnd, now, diff, diffTot},
  tEnd = AbsoluteTime[];
  now = DateString[];

  If[showTime,
    (
      diff = tEnd - tMid;

      If[diff < 100,
        (
          diff = N[Round[diff, 10^-3]];
        ),
        (
          diff = Round[diff];
        )
      ];

      diffTot = Round[tEnd - tStart];
      Print[now, ", time used: ", diff, ", total time used: ", diffTot, ".", FromCharacterCode[10] <> strSeparatorSmall]
    ),
    (
      Print["Time used reset."]
    )
  ];

  tMid = tEnd;
];

printTimeUsed[] := printTimeUsed[True];
(* ==============================================*)
(* TODO: The distribution of exchange rates is clearly not uniform. This is irrelevant for the test. *)
(* TODO: Account for exchange rate spread if necessary *)
(* Exchange rates between assets. Modify as appropriate. *)
getExchangeRates[]:=Module[{retVal, rateToBase},
rateToBase=Table[If[ii==1, 1, RandomReal[{minRate, maxRate}]],{ii,1,noOfAssets}];
retVal = Table[If[ii==jj, 1,rateToBase[[ii]]/rateToBase[[jj]]],{ii,1,noOfAssets},{jj,1,noOfAssets}];
Return[retVal];
];

(* Number of resources for a contract *)
getNoOfRes[]:=RandomInteger[{minNoOfRes, maxNoOfRes}];

(* Resources used by a contract*)
getResources[]:=Module[{noOfRes, retVal},
noOfRes = getNoOfRes[];

(* Chances of NOT getting enough are small. It is irrelevant for this test. *)
retVal =Take[ DeleteDuplicates[Table[RandomInteger[{1, maxNoOfRes}],{ii,1,noOfRes + 2}]], UpTo[noOfRes]];
Return[retVal];
];

(* TODO: Account for the exchange rate - weak currency should have larger increment *)
(* Increment for a contract *)
getIncrement[]:=incrValues[[RandomInteger[{1, Length[incrValues]}]]];

(* Min value for a contract *)
getMinValue[incr_]:=incr * minValMultipliers[[RandomInteger[{1, Length[minValMultipliers]}]]];

(* *)
(* TODO: Rounding rules were not specified *)
getResourceDescriptor[resource_]:=Module[{retVal, increment, minVal},
increment = getIncrement[];
minVal =getMinValue[increment];

retVal = {resource, minVal, increment};
Return[retVal];
];

(* Update as needed... *)
resourceDescriptorQ[rd_]:=If[VectorQ[rd] && Length[rd]== 3, True, False, False];

(* Do not want to use associations, datasets, ... *)
rdGetAsset[rd_?resourceDescriptorQ]:=rd[[1]];
rdGetMinVal[rd_?resourceDescriptorQ]:=rd[[2]];
rdGetIncr[rd_?resourceDescriptorQ]:=rd[[3]];


rdGeValue[rd_?resourceDescriptorQ, idx_?IntegerQ]:=If[idx<=0, rdGetMinVal[rd],rdGetMinVal[rd] + idx* rdGetIncr[rd]];

(* *)
getContract[]:=Module[{retVal, resoures, increment, minVal, amount,baseCurrency, nonPayingRate, overPayingRate},
resoures = getResources[];
baseCurrency=resoures[[RandomInteger[{1,Length[resoures]}]]];
amount=RandomReal[maxContractAmount];
nonPayingRate = RandomReal[{maxBorrowingAnnualRate, maxNonPayingAnnualRate}] / 365;
overPayingRate=0.0; (* Update if needed *)

retVal = {baseCurrency,amount,SortBy[Table[getResourceDescriptor[resoures[[ii]]],{ii,1,Length[resoures]}], First], nonPayingRate, overPayingRate};
Return[retVal];
];

(* Update as needed... *)
contractQ[c_]:=If[ListQ[c]&& Length[c]==5,True,False, False];

contrGetBaseCurrency[c_?contractQ]:=c[[1]];
contrGetAmount[c_?contractQ]:=c[[2]];
contrGetDescr[c_?contractQ]:=c[[3]];
contrGetNonPayingRate[c_?contractQ]:=c[[4]];
contrGetOverPayingRate[c_?contractQ]:=c[[5]];

(* *)
getAllContracts[]:=Module[{retVal},
retVal = Table[getContract[],{ii,1,noOfContracts}];
Return[retVal];
];

(* Interest rates on positive / negative balances *)
getInstrumentInterestRate[]:=Module[{retVal, positiveRate, negativeRate},
positiveRate =RandomReal[{0, maxIncomeAnnualRate}];
negativeRate = RandomReal[{positiveRate, maxBorrowingAnnualRate}];
retVal = {positiveRate, negativeRate}/ 365;
Return[retVal];
];

getInterestRates[]:=Module[{retVal},
retVal=Table[getInstrumentInterestRate[],{ii, 1, noOfAssets}];
Return[retVal];
];

(* Update as needed... *)
interestRateQ[c_]:=If[ListQ[c]&& Length[c]==2,True,False, False];

intGetIncomeRate[d_?interestRateQ]:=d[[1]];
intGetBorrowingRate[d_?interestRateQ]:=d[[2]];

getAssets[]:=Module[{retVal},
retVal = Table[RandomInteger[{minAmountOnHand, maxAmountOnHand}],{ii,1,noOfAssets}];
Return[retVal];
];
(* ==============================================*)
(* Must be at the top if getIncrement and getMinValue have to be modified to account for the rates. *)
allExchangeRates = getExchangeRates[];

allContracts = getAllContracts[];
(* allWeights = getWeights[]; *)
allInterestRates = getInterestRates[];

allAssets = getAssets[];
(* ==============================================*)
getExchangeRate[from_?IntegerQ, to_?IntegerQ]:=allExchangeRates[[from, to]];
(* ==============================================*)
(* Amount in the base asset of the contract *)
getBaseAmount[amt_, deliveryAsset_?IntegerQ, baseAsset_?IntegerQ]:=Module[{retVal},
retVal = amt * getExchangeRate[deliveryAsset, baseAsset];
(* Print["getBaseAmount::retVal = ", retVal]; *)
Return[retVal];
];

(* valueID - ID of how much much to pay *)
(* Returns amount of the asset used for settlement *)
(* getDeliveryAmount[rd_?resourceDescriptorQ, valueID_?IntegerQ]:= *)
getDeliveryAmount[rd_?resourceDescriptorQ, valueID_]:=
Module[{retVal, res, minVal, incr},
res=rdGetAsset[rd];
 minVal=rdGetMinVal[rd];
incr =rdGetIncr[rd];
retVal=If[valueID>0, Evaluate[minVal + incr * (valueID - 1)], 0];
(* Print["getDeliveryAmount::retVal = ", retVal]; *)
Return[retVal];
];

(* Cost of contract in accounting currency *)
(* v is vector of ... *)
getContractCost[contr_?contractQ, v_]:=Module[{retVal,len, baseAsset, amtToPay, descr, paid, netAmt, baseInc},
baseAsset = contrGetBaseCurrency[contr];
amtToPay = contrGetAmount[contr];
descr = contrGetDescr[contr];
(* Print["contrCost::descr = ", descr]; *)

len = Length [descr]; (* v must have the same length *)

(* Paid amount in base asset of the contract *)
paid=Sum[getBaseAmount[getDeliveryAmount[descr[[ii]], v[[ii]]], rdGetAsset[descr[[ii]]], baseAsset],{ii, 1, len}];

netAmt = amtToPay-paid;
baseInc = -netAmt*If[netAmt>0,  Evaluate[contrGetNonPayingRate[contr]],  Evaluate[contrGetOverPayingRate[contr]]];

(* Amount in accounting currency *)
retVal=baseInc* getExchangeRate[baseAsset, baseAccountingAsset];
Return[retVal];
];

(* Calculate changes to position due to settlement of a given contract *)
getPositionChange[contr_?contractQ, v_]:=Module[{retVal, descr, len},
descr = contrGetDescr[contr];
len = Length [descr];
retVal=Table[0,{ii,1,noOfAssets}];

Do[
(
retVal[[ rdGetAsset[descr[[ii]]]]]-=getDeliveryAmount[descr[[ii]], v[[ii]]];
),
{ii,1,len}
];

Return[retVal];
];
(* ==============================================*)
(*
Print["allContracts"];
allContracts // MatrixForm

Print["allExchangeRates"];
allExchangeRates // MatrixForm

Print["allInterestRates"];
allInterestRates // MatrixForm

Print["allAssets"];
allAssets // MatrixForm

Print["Contract Cost"];
contractID= 1;
contr = allContracts[[contractID]];
descr = contrGetDescr[contr];
payVal=Table[RandomInteger[{0,5}],{ii, 1, Length[descr]}];
cost=getContractCost[contr, payVal];
change = getPositionChange[contr, payVal];

Print["contr = ", contr];
Print["descr = ", descr];
Print["payVal = ", payVal];
Print["cost = ", cost];
Print["change = ", change];
*)
(* ==============================================*)
Print["Variables"];
allVars=Table[ToExpression["x" <> ToString[ii] <> "V" <>  ToString[rdGetAsset[contrGetDescr[allContracts[[ii]]][[jj]]]]],{ii,1,noOfContracts}, {jj,1,Length[contrGetDescr[allContracts[[ii]]]]}];

allVarsLinear  = Flatten[allVars];

nonNegativeCond = "";
Do[nonNegativeCond = nonNegativeCond <> If[ii > 1, " && ", ""] <>ToString[allVarsLinear[[ii]]] <> " >= 0" ,{ii,1,Length[allVarsLinear]}];
nonNegativeCond=ToExpression[nonNegativeCond];

(*
allVars // MatrixForm
allVarsLinear

Print["nonNegativeCond"];
nonNegativeCond
*)
(* ==============================================*)
(* Maximization function *)
generateContractCost[contractID_?IntegerQ]:=Module[{retVal},
retVal=getContractCost[allContracts[[contractID]], allVars[[contractID]]];
Return[retVal];
];

generatePositionChange[contractID_?IntegerQ]:=Module[{retVal},
retVal=getPositionChange[allContracts[[contractID]], allVars[[contractID]]];
Return[retVal];
];

generateMaxFunc[]:=Module[{retVal, cost, positionChange, balance, assetIncome, totalAssetIncome},

cost=Sum[generateContractCost[ii],{ii,1,noOfContracts}];
positionChange=Sum[generatePositionChange[ii],{ii,1,noOfContracts}];
balance =allAssets + positionChange;
assetIncome = Table[balance[[ii]]*If[balance[[ii]]>=0, Evaluate[intGetIncomeRate[allInterestRates[[ii]]]],Evaluate[intGetBorrowingRate[allInterestRates[[ii]]]]],{ii,1,noOfAssets}];
totalAssetIncome = Sum[assetIncome[[ii]] * getExchangeRate[ii, baseAccountingAsset],{ii,1, noOfAssets}];
retVal=cost + totalAssetIncome;
Return[retVal];
];
(* ==============================================*)
(*
Print["generateContractCost"];
generateContractCost[1]

Print["generatePositionChange"];
generatePositionChange[1]


Print["generateMaxFunc"];
*)
maxFunc=generateMaxFunc[];

printTimeUsed[];
Print["Calling NMaximize..."];
NMaximize[{maxFunc, nonNegativeCond && Element[allVarsLinear, Integers]}, allVarsLinear, MaxIterations->10000]
printTimeUsed[];

(* ==============================================*)



During evaluation of In[152]:= seedRandomValue = 597543585369
During evaluation of In[152]:= Variables
During evaluation of In[152]:= Sun 19 Nov 2017 18:43:10, time used: 0.04, total time used: 0.
---------------------------------------------
During evaluation of In[152]:= Calling NMaximize...
Out[226]= {17.3211,{x1V3->0,x1V5->4,x1V6->0,x1V8->0,x1V9->1,x1V10->0,x2V3->0,x2V4->0,x2V5->3,x2V6->0,x2V7->0,x2V8->0,x2V9->2,x3V3->0,x3V4->6,x3V7->1,x3V10->0,x4V2->2,x4V8->0,x5V2->0,x5V4->0,x5V5->0,x5V9->1,x5V10->0,x6V1->0,x6V7->1,x6V8->0,x7V2->0,x7V5->15,x7V7->0,x7V8->0,x8V1->0,x8V4->0,x8V5->76,x8V7->42,x9V4->0,x9V9->1,x9V10->0,x10V1->0,x10V4->0,x10V5->242,x10V6->0,x10V8->0,x10V9->0,x10V10->0,x11V8->0,x11V10->1,x12V1->0,x12V2->0,x12V3->0,x12V5->1,x12V7->0,x12V9->0,x12V10->0,x13V2->0,x13V3->0,x13V4->0,x13V5->0,x13V6->0,x13V7->0,x13V8->0,x13V9->1,x14V8->0,x14V9->1,x14V10->0,x15V1->0,x15V6->0,x15V7->1,x15V8->0,x15V9->2,x16V2->0,x16V3->0,x16V6->0,x16V7->0,x16V8->0,x16V9->1,x17V1->0,x17V3->0,x17V6->0,x17V9->0,x17V10->1,x18V2->0,x18V3->0,x18V4->0,x18V6->0,x18V7->1,x18V8->0,x19V2->0,x19V3->0,x19V4->0,x19V5->38,x19V7->0,x19V8->0,x20V1->1,x20V2->0}}
During evaluation of In[152]:= Sun 19 Nov 2017 19:10:09, time used: 1619, total time used: 1619.
---------------------------------------------