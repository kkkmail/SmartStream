(* ==============================================*)
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

(* Set to True to run NMinimize *)
runNMinimize = True;

(* Set to True to print detailed information, like variable, conditions, etc... *)
printDetailedInfo = True;

(*
noOfContracts =1;
noOfAssets = 2;
*)

(*
(* Test #3 *)
noOfContracts = 20;
noOfAssets = 6;
*)

(*
(* Test #4 *)
noOfContracts = 100;
noOfAssets = 10;
*)

(* Test #5 *)
noOfContracts = 200;
noOfAssets = 20;


incrValues = {5,10,25,50,100};
minValMultipliers = {4,6,8,10};

(*
minRate = 0.01; (* Around JPY *)
maxRate = 3; (* Around KWD / BHD / OMR *)
*)

minRate = 1;
maxRate = 1;

(*
maxIncomeAnnualRate = 1.0 ;
maxBorrowingAnnualRate = 3.0 ;
maxNonPayingAnnualRate = 10.0 ;
*)

maxIncomeAnnualRate = 100 ;
(* maxBorrowingAnnualRate = 3.0 ; *)
maxNonPayingAnnualRate = 200 ;

maxContractAmount = 3000;

(* Adjust as necessary *)
(* min/max amount of assets before settlement *)
amountMultiplier = noOfContracts * maxContractAmount / 2;
minAmountOnHand = -(amountMultiplier/5)*0;
maxAmountOnHand = amountMultiplier/2;
(* ==============================================*)
minNoOfRes = Min[2, noOfAssets];
maxNoOfRes =  Min[10, noOfAssets];
baseAccountingAsset = 1; (* Asset in which perform all calculations. *)
(* ==============================================*)
(* Function to calculate payment amount for the contract *)
(* x - ID of payment: 0 - do not pay, 1 - pay minAmount (m), over 1 - pay minAmount + step * (x - 1) *)
fPay[x_, m_, s_]:=s*x-((m-s)*(-1+Sqrt[(-1+x)^2]-x))/2;
(* Print[Plot[fPay[x, 10,1],{x, 0,5}, PlotRange All, FrameTrue, GridLinesAutomatic]]; *)
(* ==============================================*)
(* Some legacy stuff... *)
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
(* rateToBase=Table[If[ii1, 1, RandomReal[{minRate, maxRate}]],{ii,1,noOfAssets}]; *)
rateToBase=Table[1,{ii,1,noOfAssets}];
retVal = Table[If[ii==jj, 1,rateToBase[[ii]]/rateToBase[[jj]]],{ii,1,noOfAssets},{jj,1,noOfAssets}];
Return[retVal];
];

(* Number of resources for a contract *)
getNoOfRes[]:=RandomInteger[{minNoOfRes, maxNoOfRes}];

(* Resources used by a contract*)
getResources[]:=Module[{noOfRes, retVal,len, noDupl},
noOfRes = getNoOfRes[];

(* Chances of NOT getting enough are small. It is irrelevant for this test. *)
noDupl = DeleteDuplicates[Table[RandomInteger[{1, maxNoOfRes}],{ii,1,noOfRes + 2}]];
retVal =Take[noDupl, Min[noOfRes, Length[noDupl]]];
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

(* *)
getContract[]:=Module[{retVal, resoures, increment, minVal, amount,baseCurrency, nonPayingRate, overPayingRate},
resoures = getResources[];
baseCurrency=resoures[[RandomInteger[{1,Length[resoures]}]]];
amount=RandomInteger[maxContractAmount];
nonPayingRate = RandomInteger[{maxIncomeAnnualRate, maxNonPayingAnnualRate}];

(*  RandomReal[{maxBorrowingAnnualRate, maxNonPayingAnnualRate}] / 365 *)
overPayingRate=0; (* Update if needed *)

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
(*
positiveRate =RandomReal[{0, maxIncomeAnnualRate}];
negativeRate = RandomReal[{positiveRate, maxBorrowingAnnualRate}];
*)
positiveRate =RandomInteger[{0, maxIncomeAnnualRate}];
negativeRate =10^6; (* Do not want to borrow... *)

retVal = {positiveRate, negativeRate} (* / 365 *);
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
(* retVal=fPay[valueID, minVal, incr]; *)

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
(* baseInc=-contrGetNonPayingRate[contr]*(Sqrt[netAmt^2]+netAmt)/2; *)

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

getContractAmt[contrID_]:=Sum[getBaseAmount[getDeliveryAmount[contrGetDescr[allContracts[[contrID]]][[ii]], allVars[[contrID]][[ii]]], rdGetAsset[contrGetDescr[allContracts[[contrID]]][[ii]]], contrGetBaseCurrency[allContracts[[contrID]]]],{ii, 1, Length[ allVars[[contrID]]]}];
(* ==============================================*)
If[printDetailedInfo,
(
Print["allContracts"];
Print[allContracts // MatrixForm];

Print["allExchangeRates"];
Print[allExchangeRates // MatrixForm // MatrixForm];

Print["allInterestRates"];
Print[allInterestRates // MatrixForm // MatrixForm];

Print["allAssets"];
Print[allAssets // MatrixForm // MatrixForm];

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
)
];
(* ==============================================*)
Print["Variables"];
allVars=Table[ToExpression["x" <> ToString[ii] <> "V" <>  ToString[rdGetAsset[contrGetDescr[allContracts[[ii]]][[jj]]]]],{ii,1,noOfContracts}, {jj,1,Length[contrGetDescr[allContracts[[ii]]]]}];


getMaxPmtID[d_?resourceDescriptorQ, amt_, baseCur_]:=Max[Floor[2+(amt*getExchangeRate[baseCur,rdGetAsset[d]]-rdGetMinVal[d])/rdGetIncr[d]],1];

allVarsWithRange=Table[{allVars[[ii,jj]], 0, getMaxPmtID[contrGetDescr[allContracts[[ii]]][[jj]],contrGetAmount[allContracts[[ii]]],contrGetBaseCurrency[allContracts[[ii]]]]},{ii,1,noOfContracts}, {jj,1,Length[contrGetDescr[allContracts[[ii]]]]}];

allVarsWithRangeLinear = Flatten[allVarsWithRange,1];


allVarsLinear  = Flatten[allVars];
zeroRule=Table[allVarsLinear[[ii]] -> 0, {ii, 1, Length[allVarsLinear]}];


nonNegativeCond = "";
Do[nonNegativeCond = nonNegativeCond <> If[ii > 1, " && ", ""] <>ToString[allVarsLinear[[ii]]] <> " >= 0" ,{ii,1,Length[allVarsLinear]}];
nonNegativeCond=ToExpression[nonNegativeCond];

upperBoundCond = "";
Do[upperBoundCond = upperBoundCond <> If[ii > 1, " && ", ""] <>ToString[allVarsLinear[[ii]]] <> " <= " <> ToString[allVarsWithRangeLinear[[ii, 3]]] ,{ii,1,Length[allVarsWithRangeLinear]}];
upperBoundCond=ToExpression[upperBoundCond];

(* ==============================================*)
(* Maximization function *)
generateContractCost[contractID_?IntegerQ]:=Module[{retVal},
retVal=getContractCost[allContracts[[contractID]], allVars[[contractID]]];
(* Print["generateContractCost::retVal = ", retVal, "..."]; *)
Return[retVal];
];

generatePositionChange[contractID_?IntegerQ]:=Module[{retVal},
retVal=getPositionChange[allContracts[[contractID]], allVars[[contractID]]];

(* Print["generatePositionChange::retVal = ", retVal]; *)
Return[retVal];
];

generateBalance[]:=Module[{positionChange, balance},
positionChange=Sum[generatePositionChange[ii],{ii,1,noOfContracts}];
balance =allAssets + positionChange;
Return[balance];
];

generateMaxFunc[]:=Module[{retVal, cost, balance, assetIncome, totalAssetIncome},
cost=Sum[generateContractCost[ii],{ii,1,noOfContracts}];
balance=generateBalance[];

assetIncome = Table[balance[[ii]]*intGetIncomeRate[allInterestRates[[ii]]],{ii,1,noOfAssets}];

(*
assetIncome = Table[balance[[ii]]*If[balance[[ii]]≥0, Evaluate[intGetIncomeRate[allInterestRates[[ii]]]],Evaluate[intGetBorrowingRate[allInterestRates[[ii]]]]],{ii,1,noOfAssets}];
*)


totalAssetIncome = Sum[assetIncome[[ii]] * getExchangeRate[ii, baseAccountingAsset],{ii,1, noOfAssets}];
retVal=cost + totalAssetIncome;
(* Print["generateMaxFunc::retVal = ", retVal]; *)
Return[retVal];
];

allBalance=generateBalance[];
nonNegBalance=Table[allBalance[[ii]]>= 0, {ii, 1, Length[allBalance]}];

nonNegBalanceCond = "";
Do[nonNegBalanceCond = nonNegBalanceCond <> If[ii > 1, " && ", ""] <>ToString[InputForm[allBalance[[ii]]]] <> " >= 0" ,{ii,1,Length[allBalance]}];
nonNegBalanceCond=ToExpression[nonNegBalanceCond];

maxFunc=generateMaxFunc[];
(* ==============================================*)
If[printDetailedInfo,
(
Print["allVars"];
Print[allVars // MatrixForm];
Print["allVarsLinear"];
Print[allVarsLinear];
Print["allVarsWithRange"];
Print[allVarsWithRange // MatrixForm];
Print["allVarsWithRangeLinear"];
Print[allVarsWithRangeLinear];

Print["nonNegativeCond"];
Print[nonNegativeCond];
Print["upperBoundCond"];
Print[upperBoundCond];
Print["nonNegBalanceCond"];
Print[nonNegBalanceCond];

Print["generateContractCost"];
Print["generateContractCost[1] = ", generateContractCost[1]]

Print["generatePositionChange"];
Print[generatePositionChange[1]];

Print["generateMaxFunc"];
Print[maxFunc];
)
];

printTimeUsed[];
(* ==============================================*)
(* crlf=FromCharacterCode[13]<>FromCharacterCode[10]; *)
crlf=FromCharacterCode[13];

generateConfStr[]:=Module[{s},
s=""<>crlf;
s=s<>"    let conf = "<>crlf;
s=s<>"        {   "<>crlf;
s=s<>"            ConfigData.defaultValue with"<>crlf;
s=s<>"                noOfContracts = "<>ToString[noOfContracts]<>crlf;
s=s<>"                noOfAssets = "<>ToString[noOfAssets]<>crlf;
s=s<>"                rescaleRates = true"<>crlf;
s=s<>"        }"<>crlf<>crlf;

Return[s];
];

generateContractsStr[]:=Module[{s, len,descr},
s=""<>crlf;
s=s<>"    let contracts : ContractDescriptor[] = "<>crlf;
s=s<>"        [|"<>crlf;

Do[
(
s=s<>"            { contractID = " <> ToString[ii-1] <> "; baseAsset = " <> ToString[contrGetBaseCurrency[allContracts[[ii]]]-1] <> "; amount = " <>ToString[N[contrGetAmount[allContracts[[ii]]]]] <> "; descriptors = [| ";

descr=contrGetDescr[allContracts[[ii]]];
len = Length[descr];

Do[
(
s=s <> "{ asset = " <> ToString[rdGetAsset[descr[[jj]]] - 1]  <> "; minVal = " <> ToString[N[rdGetMinVal[descr[[jj]]]]] <> "; incr = " <> ToString[N[rdGetIncr[descr[[jj]]]]] <> " }; ";
),{jj,1,len}];

s=s<>" |]; nonPayingRate = " <> ToString[N[contrGetNonPayingRate[allContracts[[ii]]]]] <> " }"<>crlf;
),{ii,1,noOfContracts}];

s=s<>"        |]"<>crlf;

Return[s];
];

generatePositionsStr[]:=Module[{s},
s=""<>crlf;
s=s<>"    let positions : PositionData[] = "<>crlf;
s=s<>"        [|"<>crlf;

Do[
(
s=s<>"            { asset = " <> ToString[ii-1] <> "; balance = " <> ToString[N[allAssets[[ii]]]] <> "; incomeRate = " <> ToString[N[intGetIncomeRate[allInterestRates[[ii]]]]] <> " }"<>crlf;
),{ii,1,noOfAssets}];

s=s<>"        |]"<>crlf;

Return[s];
];

Print["conf = ", crlf, generateConfStr[]];
Print["contracts = ", crlf, generateContractsStr[]];
Print["positions = ", crlf, generatePositionsStr[]];
(* ==============================================*)
If[runNMinimize,
(
Print["Calling NMaximize..."];
sol = NMaximize[{maxFunc, nonNegativeCond && upperBoundCond && nonNegBalanceCond && Element[allVarsLinear, Integers]}, allVarsLinear, MaxIterations->100000];
(* sol = FindMaximum[Evaluate[{maxFunc, nonNegativeCond && nonNegBalanceCond && Element[allVarsLinear, Integers]}], Evaluate[allVarsWithRangeLinear], MaxIterations100000]; *)

printTimeUsed[];
Print["sol = ", sol];

Print["Initial balances"];
zeroBal=generateBalance[] /. zeroRule;
 Print[zeroBal // MatrixForm];
Print[strSeparator];

Print["Updated balances:"];
newBal=generateBalance[] /. sol[[2]];
Print[newBal // MatrixForm];
Print[strSeparator];

Print["Initial contract amounts"];
initContrAmt=Table[contrGetAmount[allContracts[[ii]]]-getContractAmt[ii] /.zeroRule, {ii,1 noOfContracts}];
 Print[initContrAmt // MatrixForm];
Print[strSeparator];

Print ["Updated contract amounts."];
newContrAmt = Table[contrGetAmount[allContracts[[ii]]]-getContractAmt[ii] /. sol[[2]], {ii,1 noOfContracts}];
Print[newContrAmt // MatrixForm];
Print[strSeparator];
),
(
Print["Not running NMinimize..."];
),
(
Print["Invalid value of runNMinimize..."];
)
];
(* ==============================================*)



seedRandomValue = 597543585369
allContracts
(6	193	{{5,200,50},{6,20,5},{7,80,10}}	148	0
9	1161	{{2,1000,100},{5,60,10},{6,40,10},{7,500,50},{9,30,5},{10,400,50}}	105	0
1	654	{{1,20,5},{3,400,100}}	150	0
4	1685	{{1,500,50},{2,400,50},{4,40,5},{8,20,5},{9,30,5},{10,50,5}}	157	0
7	2639	{{1,600,100},{2,800,100},{4,60,10},{6,150,25},{7,150,25},{8,600,100},{9,80,10}}	157	0
8	1994	{{1,50,5},{4,250,25},{8,50,5},{10,300,50}}	121	0
2	1523	{{2,100,10},{4,150,25},{6,100,10},{10,40,10}}	122	0
3	2702	{{3,50,5},{7,150,25},{8,20,5},{10,50,5}}	153	0
8	2685	{{1,400,100},{2,1000,100},{4,150,25},{6,100,25},{8,600,100},{9,200,50},{10,600,100}}	194	0
8	1641	{{5,50,5},{7,20,5},{8,100,25},{10,30,5}}	132	0
4	1207	{{1,100,10},{2,20,5},{3,300,50},{4,300,50},{9,100,25},{10,250,25}}	120	0
8	2373	{{1,600,100},{3,40,10},{4,100,10},{5,40,10},{8,80,10},{9,800,100},{10,20,5}}	161	0
10	991	{{1,100,10},{3,200,50},{4,600,100},{7,60,10},{10,600,100}}	175	0
2	1042	{{1,40,5},{2,100,10},{3,30,5},{4,50,5},{5,400,50},{6,250,25},{7,250,25},{10,50,5}}	127	0
1	2805	{{1,50,5},{2,150,25},{5,250,25}}	179	0
3	2438	{{1,150,25},{3,200,25}}	154	0
4	2209	{{2,400,100},{3,30,5},{4,200,50},{5,250,25},{6,300,50},{8,30,5},{9,150,25},{10,400,100}}	149	0
3	2438	{{1,400,100},{2,20,5},{3,60,10},{4,150,25},{5,400,100},{6,200,25},{7,200,50},{10,40,5}}	119	0
5	2015	{{3,600,100},{5,800,100},{8,50,5},{9,1000,100},{10,100,25}}	143	0
3	1391	{{1,600,100},{2,30,5},{3,100,25},{6,250,25},{8,400,100},{10,20,5}}	137	0
6	357	{{3,600,100},{5,100,25},{6,50,5},{7,300,50}}	196	0
1	573	{{1,50,5},{3,150,25},{4,800,100},{6,20,5},{10,50,5}}	167	0
5	1992	{{4,250,25},{5,300,50}}	179	0
4	428	{{1,500,50},{3,150,25},{4,500,50},{5,200,50},{7,300,50},{9,200,50},{10,500,50}}	167	0
7	86	{{2,1000,100},{4,80,10},{7,50,5},{9,600,100}}	141	0
1	1145	{{1,50,5},{6,30,5},{7,400,50},{9,20,5}}	180	0
5	508	{{2,200,25},{4,50,5},{5,250,25}}	173	0
7	2013	{{1,500,50},{6,40,10},{7,50,5},{8,400,50},{9,50,5}}	156	0
5	1858	{{1,200,50},{2,150,25},{3,500,50},{4,200,25},{5,600,100},{8,100,25},{9,600,100}}	123	0
4	1095	{{1,1000,100},{4,100,25}}	193	0
1	718	{{1,1000,100},{3,40,5},{5,500,50},{6,500,50},{7,150,25},{10,600,100}}	133	0
10	2522	{{1,400,100},{3,1000,100},{8,600,100},{9,400,50},{10,20,5}}	177	0
7	1651	{{7,40,5},{10,40,5}}	187	0
6	1424	{{4,40,5},{6,60,10},{7,80,10},{8,30,5},{10,40,5}}	156	0
8	2618	{{3,200,50},{4,100,10},{8,200,25}}	169	0
9	897	{{1,500,50},{2,400,50},{3,200,50},{4,250,25},{6,50,5},{9,40,5}}	125	0
3	2855	{{2,20,5},{3,30,5},{9,60,10}}	150	0
8	883	{{8,40,5},{9,200,25}}	123	0
9	1292	{{2,100,25},{4,200,25},{8,200,25},{9,80,10}}	101	0
1	1910	{{1,30,5},{9,1000,100}}	114	0
8	2920	{{1,200,25},{3,300,50},{5,100,10},{6,800,100},{7,600,100},{8,60,10},{9,40,10},{10,20,5}}	169	0
7	2758	{{1,1000,100},{3,500,50},{4,1000,100},{6,800,100},{7,80,10},{9,200,25},{10,100,25}}	181	0
8	906	{{1,100,25},{3,150,25},{4,800,100},{5,40,10},{8,200,50},{10,300,50}}	145	0
6	2681	{{2,400,50},{4,40,10},{6,80,10},{7,300,50},{10,500,50}}	128	0
3	1270	{{1,600,100},{3,100,10},{8,40,10},{10,400,50}}	167	0
4	1445	{{1,250,25},{2,500,50},{3,60,10},{4,250,25},{5,200,50},{6,600,100}}	146	0
6	2150	{{1,1000,100},{3,50,5},{5,500,50},{6,30,5},{8,200,50},{10,200,50}}	190	0
2	88	{{2,300,50},{3,200,50},{4,30,5},{6,100,10},{7,200,25},{8,500,50}}	177	0
4	1356	{{2,40,10},{3,800,100},{4,20,5},{5,300,50},{9,100,25}}	163	0
6	666	{{1,150,25},{3,20,5},{6,500,50},{8,500,50},{9,80,10}}	100	0
4	794	{{1,100,10},{4,800,100},{6,200,50},{8,300,50},{9,400,100}}	194	0
3	2629	{{3,50,5},{8,300,50}}	118	0
1	2710	{{1,500,50},{2,600,100},{7,400,100},{8,60,10},{9,150,25},{10,40,10}}	131	0
4	2957	{{4,500,50},{10,80,10}}	147	0
9	353	{{1,40,10},{2,40,10},{4,20,5},{6,1000,100},{7,500,50},{8,100,10},{9,80,10},{10,400,50}}	117	0
2	2623	{{2,20,5},{3,250,25},{8,1000,100}}	130	0
6	470	{{1,500,50},{4,1000,100},{5,150,25},{6,60,10},{8,30,5},{9,800,100},{10,800,100}}	165	0
3	1980	{{1,250,25},{2,800,100},{3,500,50},{4,100,25},{5,400,50},{6,20,5},{7,100,10},{8,60,10},{9,100,10},{10,1000,100}}	155	0
5	2941	{{5,20,5},{7,100,25}}	174	0
2	2928	{{2,250,25},{4,1000,100},{7,20,5},{9,40,10},{10,40,5}}	150	0
5	2274	{{2,250,25},{5,800,100},{7,100,10},{9,250,25}}	102	0
7	2666	{{2,300,50},{7,200,25}}	176	0
7	1708	{{3,100,25},{4,200,50},{5,150,25},{6,400,50},{7,40,10},{8,20,5},{9,30,5}}	147	0
8	1826	{{2,200,25},{4,80,10},{8,50,5}}	143	0
10	665	{{1,400,50},{3,300,50},{4,100,25},{9,500,50},{10,800,100}}	112	0
4	938	{{2,600,100},{3,250,25},{4,30,5},{5,150,25},{6,50,5},{8,100,10},{9,500,50}}	159	0
8	2447	{{4,60,10},{8,400,100}}	169	0
9	79	{{1,150,25},{4,400,100},{5,30,5},{7,500,50},{8,50,5},{9,150,25}}	181	0
8	1253	{{1,50,5},{2,60,10},{4,100,10},{5,250,25},{7,300,50},{8,150,25},{9,40,5}}	110	0
7	2762	{{2,40,5},{3,60,10},{4,60,10},{6,150,25},{7,50,5},{10,50,5}}	153	0
7	2504	{{1,30,5},{3,250,25},{7,1000,100},{8,40,10},{9,40,5}}	119	0
1	357	{{1,800,100},{7,400,100}}	178	0
9	1374	{{1,400,50},{2,50,5},{6,20,5},{8,80,10},{9,300,50}}	122	0
3	1906	{{2,40,5},{3,500,50},{4,80,10},{6,20,5}}	124	0
6	484	{{1,500,50},{4,30,5},{5,400,50},{6,100,10},{7,400,50},{9,20,5},{10,20,5}}	196	0
7	2003	{{2,600,100},{3,100,25},{4,500,50},{7,40,5},{8,80,10},{9,1000,100},{10,250,25}}	131	0
10	103	{{3,1000,100},{4,400,100},{6,400,100},{10,30,5}}	132	0
2	2059	{{1,20,5},{2,80,10},{3,80,10},{4,20,5},{5,40,10},{6,40,10}}	103	0
6	2189	{{2,100,10},{6,800,100},{7,150,25}}	137	0
5	2860	{{1,60,10},{5,800,100},{9,100,10}}	175	0
1	1010	{{1,20,5},{3,100,25},{8,150,25},{9,150,25}}	101	0
1	1599	{{1,200,50},{2,400,100},{4,150,25},{6,400,50},{7,40,10},{9,250,25},{10,80,10}}	194	0
9	1520	{{1,200,50},{2,50,5},{9,300,50}}	136	0
10	2337	{{2,60,10},{4,300,50},{5,200,50},{6,60,10},{7,60,10},{8,60,10},{10,400,50}}	130	0
6	1720	{{1,200,50},{3,200,50},{4,40,10},{6,800,100},{7,30,5},{8,100,25},{9,400,100}}	110	0
8	1283	{{1,50,5},{2,300,50},{4,100,10},{6,400,100},{7,100,10},{8,40,10},{9,30,5},{10,100,25}}	138	0
9	784	{{1,300,50},{6,30,5},{7,200,25},{8,100,10},{9,200,25}}	109	0
5	724	{{1,100,10},{2,250,25},{4,100,25},{5,20,5},{6,200,25},{7,200,25},{8,400,100},{9,100,25},{10,600,100}}	140	0
8	1164	{{2,800,100},{6,400,100},{8,400,50}}	101	0
8	2307	{{1,60,10},{3,150,25},{4,100,10},{5,150,25},{7,500,50},{8,250,25},{9,400,100}}	170	0
8	2023	{{6,800,100},{8,100,10}}	182	0
7	419	{{2,800,100},{4,20,5},{7,250,25},{10,200,25}}	186	0
2	198	{{1,200,25},{2,300,50},{4,30,5},{5,1000,100},{7,40,10},{8,20,5}}	131	0
2	1140	{{2,60,10},{3,250,25},{5,200,25},{7,500,50},{8,600,100},{9,400,50},{10,30,5}}	127	0
3	871	{{1,200,25},{3,300,50},{5,400,100},{8,300,50},{9,200,25},{10,30,5}}	198	0
6	1789	{{3,300,50},{4,200,50},{5,40,10},{6,50,5},{7,250,25},{9,20,5},{10,200,25}}	143	0
6	2116	{{1,30,5},{2,50,5},{3,200,25},{6,800,100},{9,100,25}}	183	0
2	242	{{1,400,100},{2,1000,100},{3,60,10},{5,40,10},{6,30,5},{7,150,25},{9,50,5},{10,250,25}}	161	0
9	116	{{3,40,10},{4,40,5},{9,600,100}}	176	0
3	613	{{1,30,5},{3,600,100},{4,80,10},{6,40,5},{8,1000,100},{9,20,5},{10,1000,100}}	164	0
9	674	{{2,40,10},{3,400,100},{5,600,100},{6,40,5},{7,60,10},{9,100,10}}	107	0
7	1578	{{1,40,5},{3,150,25},{4,100,10},{7,30,5},{9,100,10}}	105	0
6	2063	{{1,40,5},{2,60,10},{3,600,100},{4,600,100},{6,200,50},{9,400,50},{10,40,5}}	190	0
9	479	{{6,80,10},{9,40,10}}	133	0
7	1941	{{6,30,5},{7,600,100}}	147	0
8	2959	{{3,40,5},{4,100,10},{6,800,100},{8,100,10},{9,60,10},{10,100,25}}	163	0
9	645	{{1,400,100},{2,60,10},{5,30,5},{7,500,50},{9,50,5}}	137	0
2	1988	{{2,800,100},{5,60,10}}	117	0
8	1200	{{1,100,10},{4,100,10},{6,600,100},{7,200,25},{8,40,5}}	145	0
7	2668	{{7,250,25},{10,600,100}}	149	0
7	2836	{{1,100,10},{2,20,5},{3,80,10},{4,600,100},{7,500,50},{8,200,25},{9,200,25}}	179	0
9	1704	{{3,60,10},{4,50,5},{5,100,25},{6,800,100},{7,20,5},{9,250,25}}	126	0
6	1449	{{1,100,25},{4,20,5},{5,40,5},{6,600,100},{7,400,50},{9,800,100},{10,1000,100}}	126	0
9	2395	{{1,100,10},{2,800,100},{3,600,100},{4,60,10},{6,50,5},{7,250,25},{8,400,100},{9,80,10},{10,500,50}}	177	0
4	1987	{{1,300,50},{4,1000,100},{7,1000,100},{9,80,10}}	170	0
9	2326	{{1,40,5},{2,40,5},{4,60,10},{5,40,10},{6,80,10},{8,60,10},{9,40,10},{10,200,50}}	137	0
6	1880	{{1,1000,100},{3,200,25},{4,600,100},{6,400,100},{7,400,50},{9,600,100},{10,100,25}}	150	0
10	2497	{{1,100,25},{7,30,5},{8,500,50},{9,40,5},{10,60,10}}	165	0
1	206	{{1,600,100},{9,60,10}}	184	0
4	2763	{{4,600,100},{7,800,100},{9,40,10}}	158	0
6	1149	{{5,80,10},{6,60,10}}	132	0
7	1259	{{5,40,10},{7,40,5},{8,400,100},{9,100,10},{10,400,50}}	180	0
2	195	{{1,50,5},{2,20,5},{4,30,5},{6,500,50},{9,30,5},{10,800,100}}	160	0
1	1803	{{1,1000,100},{5,1000,100},{7,800,100}}	148	0
4	2456	{{2,30,5},{4,50,5},{5,600,100},{6,40,5},{7,40,10},{9,200,25},{10,60,10}}	147	0
6	320	{{1,400,50},{2,600,100},{4,300,50},{6,50,5},{8,800,100},{9,500,50}}	181	0
2	2958	{{1,1000,100},{2,40,5},{4,60,10},{5,100,10},{6,20,5},{7,250,25},{9,100,10},{10,200,25}}	159	0
1	581	{{1,150,25},{4,400,50}}	145	0
9	1958	{{2,40,5},{5,600,100},{7,300,50},{9,60,10},{10,200,25}}	179	0
1	1009	{{1,20,5},{10,800,100}}	113	0
3	1389	{{1,30,5},{2,40,5},{3,80,10},{4,40,10},{5,200,25},{7,500,50}}	112	0
4	1709	{{1,800,100},{3,20,5},{4,30,5},{5,1000,100},{6,100,10},{7,30,5},{9,250,25}}	122	0
10	2349	{{1,200,25},{2,30,5},{4,100,25},{5,250,25},{8,150,25},{10,30,5}}	141	0
8	444	{{1,50,5},{2,50,5},{3,200,25},{5,20,5},{7,50,5},{8,150,25},{9,800,100},{10,500,50}}	102	0
6	376	{{1,1000,100},{3,40,10},{6,1000,100},{7,100,25}}	128	0
6	981	{{2,150,25},{3,150,25},{5,30,5},{6,50,5},{7,30,5}}	195	0
8	2392	{{2,600,100},{3,100,25},{4,400,50},{8,100,10}}	174	0
3	1481	{{2,400,50},{3,1000,100},{4,200,50},{5,100,25},{6,400,50},{7,30,5},{8,1000,100}}	139	0
6	1603	{{1,50,5},{3,20,5},{4,1000,100},{5,200,50},{6,250,25},{8,800,100},{9,400,100}}	167	0
1	7	{{1,40,5},{3,40,5},{4,80,10},{5,800,100},{7,100,25},{8,600,100},{9,800,100}}	200	0
5	1598	{{2,80,10},{4,60,10},{5,100,25},{6,500,50},{7,100,25},{8,800,100},{10,100,10}}	168	0
1	2724	{{1,800,100},{2,400,50},{3,100,10},{4,100,10},{6,400,50},{8,150,25},{9,40,5}}	184	0
9	2148	{{2,20,5},{6,250,25},{8,20,5},{9,200,50}}	167	0
1	1005	{{1,250,25},{7,400,50}}	185	0
10	2525	{{2,50,5},{5,40,5},{6,200,50},{7,200,25},{8,400,50},{10,30,5}}	190	0
3	1558	{{3,400,100},{5,150,25}}	112	0
6	1179	{{1,40,5},{4,30,5},{5,20,5},{6,50,5},{8,100,10},{10,200,50}}	181	0
2	1283	{{2,150,25},{4,250,25},{5,60,10},{7,100,10},{8,30,5}}	176	0
6	1897	{{4,800,100},{6,400,100}}	115	0
7	1114	{{4,800,100},{5,400,100},{6,600,100},{7,50,5},{8,500,50}}	164	0
10	1314	{{1,800,100},{2,300,50},{5,80,10},{6,200,25},{7,30,5},{9,200,50},{10,100,25}}	192	0
10	2455	{{2,30,5},{5,800,100},{7,300,50},{10,150,25}}	129	0
6	242	{{6,400,50},{7,40,10},{9,30,5}}	171	0
6	2638	{{2,800,100},{4,800,100},{5,250,25},{6,800,100},{7,800,100},{9,100,10},{10,40,5}}	148	0
8	1172	{{3,400,50},{5,20,5},{7,250,25},{8,100,10},{10,300,50}}	137	0
9	998	{{1,100,10},{3,40,10},{4,250,25},{6,20,5},{7,150,25},{9,60,10},{10,30,5}}	142	0
9	511	{{1,600,100},{3,300,50},{4,200,50},{5,200,25},{6,1000,100},{9,100,10}}	179	0
1	1176	{{1,60,10},{5,250,25},{6,400,50},{10,30,5}}	123	0
7	2530	{{7,200,25},{10,400,50}}	129	0
4	1523	{{2,50,5},{3,200,25},{4,800,100},{8,1000,100},{9,400,50}}	113	0
5	1893	{{3,200,25},{5,1000,100},{9,50,5}}	188	0
8	2334	{{1,1000,100},{2,80,10},{3,500,50},{4,100,10},{6,80,10},{7,20,5},{8,500,50},{9,250,25},{10,600,100}}	115	0
7	377	{{1,300,50},{4,150,25},{6,100,25},{7,80,10},{8,300,50},{10,1000,100}}	102	0
1	1682	{{1,150,25},{3,30,5},{4,200,25},{5,250,25},{6,100,10},{9,60,10},{10,30,5}}	194	0
3	1543	{{2,100,10},{3,100,10},{6,20,5},{7,150,25},{8,400,50}}	118	0
3	2759	{{1,200,25},{3,600,100},{4,800,100},{6,50,5}}	165	0
9	377	{{6,20,5},{9,200,50}}	104	0
5	671	{{2,300,50},{3,800,100},{5,500,50},{6,40,5},{9,200,25},{10,400,50}}	123	0
7	868	{{2,800,100},{4,600,100},{7,800,100},{8,400,100}}	174	0
4	629	{{2,60,10},{4,100,10},{9,30,5}}	166	0
6	270	{{3,150,25},{5,40,10},{6,200,50},{7,40,10}}	107	0
4	544	{{1,40,10},{2,100,25},{4,80,10},{8,30,5},{9,1000,100}}	112	0
6	2308	{{2,200,50},{3,400,50},{4,40,5},{6,20,5},{10,200,50}}	113	0
3	153	{{1,250,25},{2,30,5},{3,40,10},{4,150,25},{6,100,25},{7,100,25},{8,60,10},{10,300,50}}	105	0
6	1394	{{2,20,5},{3,150,25},{4,30,5},{5,200,25},{6,400,100},{10,40,5}}	198	0
6	957	{{3,80,10},{6,200,25}}	121	0
7	192	{{2,800,100},{7,50,5}}	193	0
3	1284	{{1,30,5},{2,50,5},{3,80,10},{4,20,5},{5,300,50},{7,50,5},{10,1000,100}}	139	0
9	1353	{{6,20,5},{7,100,10},{9,40,5}}	190	0
4	910	{{4,100,10},{6,150,25},{7,250,25},{8,40,5},{9,20,5}}	147	0
8	897	{{1,1000,100},{5,250,25},{8,1000,100},{10,800,100}}	117	0
4	2919	{{1,200,50},{2,30,5},{4,400,50},{8,1000,100},{9,800,100},{10,30,5}}	144	0
4	1350	{{1,600,100},{2,400,100},{4,1000,100},{8,40,10},{9,200,50}}	195	0
9	2914	{{1,100,25},{2,600,100},{4,250,25},{5,200,50},{6,30,5},{9,300,50},{10,50,5}}	131	0
1	1953	{{1,400,100},{2,30,5}}	128	0
10	26	{{1,150,25},{2,100,25},{3,400,100},{4,500,50},{8,500,50},{9,80,10},{10,400,50}}	199	0
9	2814	{{3,1000,100},{6,40,10},{7,30,5},{9,400,100},{10,100,25}}	174	0
10	1494	{{4,800,100},{5,80,10},{6,100,25},{7,40,10},{8,500,50},{9,80,10},{10,250,25}}	145	0
4	2348	{{1,400,50},{2,60,10},{4,800,100},{6,50,5},{10,300,50}}	163	0
8	1861	{{1,40,10},{2,50,5},{3,40,5},{4,20,5},{6,500,50},{8,80,10}}	108	0
9	2611	{{1,400,50},{2,300,50},{4,250,25},{8,40,10},{9,50,5}}	169	0
5	927	{{1,100,10},{3,1000,100},{4,40,5},{5,50,5},{6,300,50},{7,200,25},{9,40,10}}	172	0
9	2943	{{1,200,25},{5,100,10},{6,600,100},{7,60,10},{9,50,5},{10,400,100}}	142	0
3	2818	{{1,250,25},{3,80,10},{7,500,50},{10,1000,100}}	118	0
10	1320	{{2,50,5},{4,200,50},{5,300,50},{8,400,100},{9,500,50},{10,60,10}}	105	0
10	712	{{2,500,50},{3,400,100},{5,150,25},{7,1000,100},{10,100,10}}	199	0
7	410	{{7,1000,100},{10,400,100}}	167	0
2	949	{{1,40,10},{2,50,5},{5,30,5},{6,400,100},{9,800,100},{10,50,5}}	200	0
7	1925	{{1,1000,100},{4,600,100},{5,400,50},{7,100,10},{8,500,50},{9,100,10},{10,80,10}}	133	0
10	829	{{2,200,50},{3,100,10},{4,150,25},{5,400,100},{6,20,5},{10,250,25}}	165	0

)
allExchangeRates
(1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1	1

)
allInterestRates
(39	1000000
38	1000000
30	1000000
3	1000000
37	1000000
97	1000000
21	1000000
52	1000000
68	1000000
18	1000000
11	1000000
92	1000000
74	1000000
42	1000000
70	1000000
47	1000000
28	1000000
48	1000000
31	1000000
85	1000000

)
allAssets
(31699
54687
85057
149097
55197
35370
135474
80689
11090
97419
79605
28959
147861
72971
71512
93277
73274
50701
30051
95550

)
Contract Cost
contr = {6,193,{{5,200,50},{6,20,5},{7,80,10}},148,0}
descr = {{5,200,50},{6,20,5},{7,80,10}}
payVal = {0,5,5}
cost = -4884
change = {0,0,0,0,0,-40,-120,0,0,0,0,0,0,0,0,0,0,0,0,0}
Variables
allVars
({x1V5,x1V6,x1V7}
{x2V2,x2V5,x2V6,x2V7,x2V9,x2V10}
{x3V1,x3V3}
{x4V1,x4V2,x4V4,x4V8,x4V9,x4V10}
{x5V1,x5V2,x5V4,x5V6,x5V7,x5V8,x5V9}
{x6V1,x6V4,x6V8,x6V10}
{x7V2,x7V4,x7V6,x7V10}
{x8V3,x8V7,x8V8,x8V10}
{x9V1,x9V2,x9V4,x9V6,x9V8,x9V9,x9V10}
{x10V5,x10V7,x10V8,x10V10}
{x11V1,x11V2,x11V3,x11V4,x11V9,x11V10}
{x12V1,x12V3,x12V4,x12V5,x12V8,x12V9,x12V10}
{x13V1,x13V3,x13V4,x13V7,x13V10}
{x14V1,x14V2,x14V3,x14V4,x14V5,x14V6,x14V7,x14V10}
{x15V1,x15V2,x15V5}
{x16V1,x16V3}
{x17V2,x17V3,x17V4,x17V5,x17V6,x17V8,x17V9,x17V10}
{x18V1,x18V2,x18V3,x18V4,x18V5,x18V6,x18V7,x18V10}
{x19V3,x19V5,x19V8,x19V9,x19V10}
{x20V1,x20V2,x20V3,x20V6,x20V8,x20V10}
{x21V3,x21V5,x21V6,x21V7}
{x22V1,x22V3,x22V4,x22V6,x22V10}
{x23V4,x23V5}
{x24V1,x24V3,x24V4,x24V5,x24V7,x24V9,x24V10}
{x25V2,x25V4,x25V7,x25V9}
{x26V1,x26V6,x26V7,x26V9}
{x27V2,x27V4,x27V5}
{x28V1,x28V6,x28V7,x28V8,x28V9}
{x29V1,x29V2,x29V3,x29V4,x29V5,x29V8,x29V9}
{x30V1,x30V4}
{x31V1,x31V3,x31V5,x31V6,x31V7,x31V10}
{x32V1,x32V3,x32V8,x32V9,x32V10}
{x33V7,x33V10}
{x34V4,x34V6,x34V7,x34V8,x34V10}
{x35V3,x35V4,x35V8}
{x36V1,x36V2,x36V3,x36V4,x36V6,x36V9}
{x37V2,x37V3,x37V9}
{x38V8,x38V9}
{x39V2,x39V4,x39V8,x39V9}
{x40V1,x40V9}
{x41V1,x41V3,x41V5,x41V6,x41V7,x41V8,x41V9,x41V10}
{x42V1,x42V3,x42V4,x42V6,x42V7,x42V9,x42V10}
{x43V1,x43V3,x43V4,x43V5,x43V8,x43V10}
{x44V2,x44V4,x44V6,x44V7,x44V10}
{x45V1,x45V3,x45V8,x45V10}
{x46V1,x46V2,x46V3,x46V4,x46V5,x46V6}
{x47V1,x47V3,x47V5,x47V6,x47V8,x47V10}
{x48V2,x48V3,x48V4,x48V6,x48V7,x48V8}
{x49V2,x49V3,x49V4,x49V5,x49V9}
{x50V1,x50V3,x50V6,x50V8,x50V9}
{x51V1,x51V4,x51V6,x51V8,x51V9}
{x52V3,x52V8}
{x53V1,x53V2,x53V7,x53V8,x53V9,x53V10}
{x54V4,x54V10}
{x55V1,x55V2,x55V4,x55V6,x55V7,x55V8,x55V9,x55V10}
{x56V2,x56V3,x56V8}
{x57V1,x57V4,x57V5,x57V6,x57V8,x57V9,x57V10}
{x58V1,x58V2,x58V3,x58V4,x58V5,x58V6,x58V7,x58V8,x58V9,x58V10}
{x59V5,x59V7}
{x60V2,x60V4,x60V7,x60V9,x60V10}
{x61V2,x61V5,x61V7,x61V9}
{x62V2,x62V7}
{x63V3,x63V4,x63V5,x63V6,x63V7,x63V8,x63V9}
{x64V2,x64V4,x64V8}
{x65V1,x65V3,x65V4,x65V9,x65V10}
{x66V2,x66V3,x66V4,x66V5,x66V6,x66V8,x66V9}
{x67V4,x67V8}
{x68V1,x68V4,x68V5,x68V7,x68V8,x68V9}
{x69V1,x69V2,x69V4,x69V5,x69V7,x69V8,x69V9}
{x70V2,x70V3,x70V4,x70V6,x70V7,x70V10}
{x71V1,x71V3,x71V7,x71V8,x71V9}
{x72V1,x72V7}
{x73V1,x73V2,x73V6,x73V8,x73V9}
{x74V2,x74V3,x74V4,x74V6}
{x75V1,x75V4,x75V5,x75V6,x75V7,x75V9,x75V10}
{x76V2,x76V3,x76V4,x76V7,x76V8,x76V9,x76V10}
{x77V3,x77V4,x77V6,x77V10}
{x78V1,x78V2,x78V3,x78V4,x78V5,x78V6}
{x79V2,x79V6,x79V7}
{x80V1,x80V5,x80V9}
{x81V1,x81V3,x81V8,x81V9}
{x82V1,x82V2,x82V4,x82V6,x82V7,x82V9,x82V10}
{x83V1,x83V2,x83V9}
{x84V2,x84V4,x84V5,x84V6,x84V7,x84V8,x84V10}
{x85V1,x85V3,x85V4,x85V6,x85V7,x85V8,x85V9}
{x86V1,x86V2,x86V4,x86V6,x86V7,x86V8,x86V9,x86V10}
{x87V1,x87V6,x87V7,x87V8,x87V9}
{x88V1,x88V2,x88V4,x88V5,x88V6,x88V7,x88V8,x88V9,x88V10}
{x89V2,x89V6,x89V8}
{x90V1,x90V3,x90V4,x90V5,x90V7,x90V8,x90V9}
{x91V6,x91V8}
{x92V2,x92V4,x92V7,x92V10}
{x93V1,x93V2,x93V4,x93V5,x93V7,x93V8}
{x94V2,x94V3,x94V5,x94V7,x94V8,x94V9,x94V10}
{x95V1,x95V3,x95V5,x95V8,x95V9,x95V10}
{x96V3,x96V4,x96V5,x96V6,x96V7,x96V9,x96V10}
{x97V1,x97V2,x97V3,x97V6,x97V9}
{x98V1,x98V2,x98V3,x98V5,x98V6,x98V7,x98V9,x98V10}
{x99V3,x99V4,x99V9}
{x100V1,x100V3,x100V4,x100V6,x100V8,x100V9,x100V10}
{x101V2,x101V3,x101V5,x101V6,x101V7,x101V9}
{x102V1,x102V3,x102V4,x102V7,x102V9}
{x103V1,x103V2,x103V3,x103V4,x103V6,x103V9,x103V10}
{x104V6,x104V9}
{x105V6,x105V7}
{x106V3,x106V4,x106V6,x106V8,x106V9,x106V10}
{x107V1,x107V2,x107V5,x107V7,x107V9}
{x108V2,x108V5}
{x109V1,x109V4,x109V6,x109V7,x109V8}
{x110V7,x110V10}
{x111V1,x111V2,x111V3,x111V4,x111V7,x111V8,x111V9}
{x112V3,x112V4,x112V5,x112V6,x112V7,x112V9}
{x113V1,x113V4,x113V5,x113V6,x113V7,x113V9,x113V10}
{x114V1,x114V2,x114V3,x114V4,x114V6,x114V7,x114V8,x114V9,x114V10}
{x115V1,x115V4,x115V7,x115V9}
{x116V1,x116V2,x116V4,x116V5,x116V6,x116V8,x116V9,x116V10}
{x117V1,x117V3,x117V4,x117V6,x117V7,x117V9,x117V10}
{x118V1,x118V7,x118V8,x118V9,x118V10}
{x119V1,x119V9}
{x120V4,x120V7,x120V9}
{x121V5,x121V6}
{x122V5,x122V7,x122V8,x122V9,x122V10}
{x123V1,x123V2,x123V4,x123V6,x123V9,x123V10}
{x124V1,x124V5,x124V7}
{x125V2,x125V4,x125V5,x125V6,x125V7,x125V9,x125V10}
{x126V1,x126V2,x126V4,x126V6,x126V8,x126V9}
{x127V1,x127V2,x127V4,x127V5,x127V6,x127V7,x127V9,x127V10}
{x128V1,x128V4}
{x129V2,x129V5,x129V7,x129V9,x129V10}
{x130V1,x130V10}
{x131V1,x131V2,x131V3,x131V4,x131V5,x131V7}
{x132V1,x132V3,x132V4,x132V5,x132V6,x132V7,x132V9}
{x133V1,x133V2,x133V4,x133V5,x133V8,x133V10}
{x134V1,x134V2,x134V3,x134V5,x134V7,x134V8,x134V9,x134V10}
{x135V1,x135V3,x135V6,x135V7}
{x136V2,x136V3,x136V5,x136V6,x136V7}
{x137V2,x137V3,x137V4,x137V8}
{x138V2,x138V3,x138V4,x138V5,x138V6,x138V7,x138V8}
{x139V1,x139V3,x139V4,x139V5,x139V6,x139V8,x139V9}
{x140V1,x140V3,x140V4,x140V5,x140V7,x140V8,x140V9}
{x141V2,x141V4,x141V5,x141V6,x141V7,x141V8,x141V10}
{x142V1,x142V2,x142V3,x142V4,x142V6,x142V8,x142V9}
{x143V2,x143V6,x143V8,x143V9}
{x144V1,x144V7}
{x145V2,x145V5,x145V6,x145V7,x145V8,x145V10}
{x146V3,x146V5}
{x147V1,x147V4,x147V5,x147V6,x147V8,x147V10}
{x148V2,x148V4,x148V5,x148V7,x148V8}
{x149V4,x149V6}
{x150V4,x150V5,x150V6,x150V7,x150V8}
{x151V1,x151V2,x151V5,x151V6,x151V7,x151V9,x151V10}
{x152V2,x152V5,x152V7,x152V10}
{x153V6,x153V7,x153V9}
{x154V2,x154V4,x154V5,x154V6,x154V7,x154V9,x154V10}
{x155V3,x155V5,x155V7,x155V8,x155V10}
{x156V1,x156V3,x156V4,x156V6,x156V7,x156V9,x156V10}
{x157V1,x157V3,x157V4,x157V5,x157V6,x157V9}
{x158V1,x158V5,x158V6,x158V10}
{x159V7,x159V10}
{x160V2,x160V3,x160V4,x160V8,x160V9}
{x161V3,x161V5,x161V9}
{x162V1,x162V2,x162V3,x162V4,x162V6,x162V7,x162V8,x162V9,x162V10}
{x163V1,x163V4,x163V6,x163V7,x163V8,x163V10}
{x164V1,x164V3,x164V4,x164V5,x164V6,x164V9,x164V10}
{x165V2,x165V3,x165V6,x165V7,x165V8}
{x166V1,x166V3,x166V4,x166V6}
{x167V6,x167V9}
{x168V2,x168V3,x168V5,x168V6,x168V9,x168V10}
{x169V2,x169V4,x169V7,x169V8}
{x170V2,x170V4,x170V9}
{x171V3,x171V5,x171V6,x171V7}
{x172V1,x172V2,x172V4,x172V8,x172V9}
{x173V2,x173V3,x173V4,x173V6,x173V10}
{x174V1,x174V2,x174V3,x174V4,x174V6,x174V7,x174V8,x174V10}
{x175V2,x175V3,x175V4,x175V5,x175V6,x175V10}
{x176V3,x176V6}
{x177V2,x177V7}
{x178V1,x178V2,x178V3,x178V4,x178V5,x178V7,x178V10}
{x179V6,x179V7,x179V9}
{x180V4,x180V6,x180V7,x180V8,x180V9}
{x181V1,x181V5,x181V8,x181V10}
{x182V1,x182V2,x182V4,x182V8,x182V9,x182V10}
{x183V1,x183V2,x183V4,x183V8,x183V9}
{x184V1,x184V2,x184V4,x184V5,x184V6,x184V9,x184V10}
{x185V1,x185V2}
{x186V1,x186V2,x186V3,x186V4,x186V8,x186V9,x186V10}
{x187V3,x187V6,x187V7,x187V9,x187V10}
{x188V4,x188V5,x188V6,x188V7,x188V8,x188V9,x188V10}
{x189V1,x189V2,x189V4,x189V6,x189V10}
{x190V1,x190V2,x190V3,x190V4,x190V6,x190V8}
{x191V1,x191V2,x191V4,x191V8,x191V9}
{x192V1,x192V3,x192V4,x192V5,x192V6,x192V7,x192V9}
{x193V1,x193V5,x193V6,x193V7,x193V9,x193V10}
{x194V1,x194V3,x194V7,x194V10}
{x195V2,x195V4,x195V5,x195V8,x195V9,x195V10}
{x196V2,x196V3,x196V5,x196V7,x196V10}
{x197V7,x197V10}
{x198V1,x198V2,x198V5,x198V6,x198V9,x198V10}
{x199V1,x199V4,x199V5,x199V7,x199V8,x199V9,x199V10}
{x200V2,x200V3,x200V4,x200V5,x200V6,x200V10}

)
allVarsLinear
{x1V5,x1V6,x1V7,x2V2,x2V5,x2V6,x2V7,x2V9,x2V10,x3V1,x3V3,x4V1,x4V2,x4V4,x4V8,x4V9,x4V10,x5V1,x5V2,x5V4,x5V6,x5V7,x5V8,x5V9,x6V1,x6V4,x6V8,x6V10,x7V2,x7V4,x7V6,x7V10,x8V3,x8V7,x8V8,x8V10,x9V1,x9V2,x9V4,x9V6,x9V8,x9V9,x9V10,x10V5,x10V7,x10V8,x10V10,x11V1,x11V2,x11V3,x11V4,x11V9,x11V10,x12V1,x12V3,x12V4,x12V5,x12V8,x12V9,x12V10,x13V1,x13V3,x13V4,x13V7,x13V10,x14V1,x14V2,x14V3,x14V4,x14V5,x14V6,x14V7,x14V10,x15V1,x15V2,x15V5,x16V1,x16V3,x17V2,x17V3,x17V4,x17V5,x17V6,x17V8,x17V9,x17V10,x18V1,x18V2,x18V3,x18V4,x18V5,x18V6,x18V7,x18V10,x19V3,x19V5,x19V8,x19V9,x19V10,x20V1,x20V2,x20V3,x20V6,x20V8,x20V10,x21V3,x21V5,x21V6,x21V7,x22V1,x22V3,x22V4,x22V6,x22V10,x23V4,x23V5,x24V1,x24V3,x24V4,x24V5,x24V7,x24V9,x24V10,x25V2,x25V4,x25V7,x25V9,x26V1,x26V6,x26V7,x26V9,x27V2,x27V4,x27V5,x28V1,x28V6,x28V7,x28V8,x28V9,x29V1,x29V2,x29V3,x29V4,x29V5,x29V8,x29V9,x30V1,x30V4,x31V1,x31V3,x31V5,x31V6,x31V7,x31V10,x32V1,x32V3,x32V8,x32V9,x32V10,x33V7,x33V10,x34V4,x34V6,x34V7,x34V8,x34V10,x35V3,x35V4,x35V8,x36V1,x36V2,x36V3,x36V4,x36V6,x36V9,x37V2,x37V3,x37V9,x38V8,x38V9,x39V2,x39V4,x39V8,x39V9,x40V1,x40V9,x41V1,x41V3,x41V5,x41V6,x41V7,x41V8,x41V9,x41V10,x42V1,x42V3,x42V4,x42V6,x42V7,x42V9,x42V10,x43V1,x43V3,x43V4,x43V5,x43V8,x43V10,x44V2,x44V4,x44V6,x44V7,x44V10,x45V1,x45V3,x45V8,x45V10,x46V1,x46V2,x46V3,x46V4,x46V5,x46V6,x47V1,x47V3,x47V5,x47V6,x47V8,x47V10,x48V2,x48V3,x48V4,x48V6,x48V7,x48V8,x49V2,x49V3,x49V4,x49V5,x49V9,x50V1,x50V3,x50V6,x50V8,x50V9,x51V1,x51V4,x51V6,x51V8,x51V9,x52V3,x52V8,x53V1,x53V2,x53V7,x53V8,x53V9,x53V10,x54V4,x54V10,x55V1,x55V2,x55V4,x55V6,x55V7,x55V8,x55V9,x55V10,x56V2,x56V3,x56V8,x57V1,x57V4,x57V5,x57V6,x57V8,x57V9,x57V10,x58V1,x58V2,x58V3,x58V4,x58V5,x58V6,x58V7,x58V8,x58V9,x58V10,x59V5,x59V7,x60V2,x60V4,x60V7,x60V9,x60V10,x61V2,x61V5,x61V7,x61V9,x62V2,x62V7,x63V3,x63V4,x63V5,x63V6,x63V7,x63V8,x63V9,x64V2,x64V4,x64V8,x65V1,x65V3,x65V4,x65V9,x65V10,x66V2,x66V3,x66V4,x66V5,x66V6,x66V8,x66V9,x67V4,x67V8,x68V1,x68V4,x68V5,x68V7,x68V8,x68V9,x69V1,x69V2,x69V4,x69V5,x69V7,x69V8,x69V9,x70V2,x70V3,x70V4,x70V6,x70V7,x70V10,x71V1,x71V3,x71V7,x71V8,x71V9,x72V1,x72V7,x73V1,x73V2,x73V6,x73V8,x73V9,x74V2,x74V3,x74V4,x74V6,x75V1,x75V4,x75V5,x75V6,x75V7,x75V9,x75V10,x76V2,x76V3,x76V4,x76V7,x76V8,x76V9,x76V10,x77V3,x77V4,x77V6,x77V10,x78V1,x78V2,x78V3,x78V4,x78V5,x78V6,x79V2,x79V6,x79V7,x80V1,x80V5,x80V9,x81V1,x81V3,x81V8,x81V9,x82V1,x82V2,x82V4,x82V6,x82V7,x82V9,x82V10,x83V1,x83V2,x83V9,x84V2,x84V4,x84V5,x84V6,x84V7,x84V8,x84V10,x85V1,x85V3,x85V4,x85V6,x85V7,x85V8,x85V9,x86V1,x86V2,x86V4,x86V6,x86V7,x86V8,x86V9,x86V10,x87V1,x87V6,x87V7,x87V8,x87V9,x88V1,x88V2,x88V4,x88V5,x88V6,x88V7,x88V8,x88V9,x88V10,x89V2,x89V6,x89V8,x90V1,x90V3,x90V4,x90V5,x90V7,x90V8,x90V9,x91V6,x91V8,x92V2,x92V4,x92V7,x92V10,x93V1,x93V2,x93V4,x93V5,x93V7,x93V8,x94V2,x94V3,x94V5,x94V7,x94V8,x94V9,x94V10,x95V1,x95V3,x95V5,x95V8,x95V9,x95V10,x96V3,x96V4,x96V5,x96V6,x96V7,x96V9,x96V10,x97V1,x97V2,x97V3,x97V6,x97V9,x98V1,x98V2,x98V3,x98V5,x98V6,x98V7,x98V9,x98V10,x99V3,x99V4,x99V9,x100V1,x100V3,x100V4,x100V6,x100V8,x100V9,x100V10,x101V2,x101V3,x101V5,x101V6,x101V7,x101V9,x102V1,x102V3,x102V4,x102V7,x102V9,x103V1,x103V2,x103V3,x103V4,x103V6,x103V9,x103V10,x104V6,x104V9,x105V6,x105V7,x106V3,x106V4,x106V6,x106V8,x106V9,x106V10,x107V1,x107V2,x107V5,x107V7,x107V9,x108V2,x108V5,x109V1,x109V4,x109V6,x109V7,x109V8,x110V7,x110V10,x111V1,x111V2,x111V3,x111V4,x111V7,x111V8,x111V9,x112V3,x112V4,x112V5,x112V6,x112V7,x112V9,x113V1,x113V4,x113V5,x113V6,x113V7,x113V9,x113V10,x114V1,x114V2,x114V3,x114V4,x114V6,x114V7,x114V8,x114V9,x114V10,x115V1,x115V4,x115V7,x115V9,x116V1,x116V2,x116V4,x116V5,x116V6,x116V8,x116V9,x116V10,x117V1,x117V3,x117V4,x117V6,x117V7,x117V9,x117V10,x118V1,x118V7,x118V8,x118V9,x118V10,x119V1,x119V9,x120V4,x120V7,x120V9,x121V5,x121V6,x122V5,x122V7,x122V8,x122V9,x122V10,x123V1,x123V2,x123V4,x123V6,x123V9,x123V10,x124V1,x124V5,x124V7,x125V2,x125V4,x125V5,x125V6,x125V7,x125V9,x125V10,x126V1,x126V2,x126V4,x126V6,x126V8,x126V9,x127V1,x127V2,x127V4,x127V5,x127V6,x127V7,x127V9,x127V10,x128V1,x128V4,x129V2,x129V5,x129V7,x129V9,x129V10,x130V1,x130V10,x131V1,x131V2,x131V3,x131V4,x131V5,x131V7,x132V1,x132V3,x132V4,x132V5,x132V6,x132V7,x132V9,x133V1,x133V2,x133V4,x133V5,x133V8,x133V10,x134V1,x134V2,x134V3,x134V5,x134V7,x134V8,x134V9,x134V10,x135V1,x135V3,x135V6,x135V7,x136V2,x136V3,x136V5,x136V6,x136V7,x137V2,x137V3,x137V4,x137V8,x138V2,x138V3,x138V4,x138V5,x138V6,x138V7,x138V8,x139V1,x139V3,x139V4,x139V5,x139V6,x139V8,x139V9,x140V1,x140V3,x140V4,x140V5,x140V7,x140V8,x140V9,x141V2,x141V4,x141V5,x141V6,x141V7,x141V8,x141V10,x142V1,x142V2,x142V3,x142V4,x142V6,x142V8,x142V9,x143V2,x143V6,x143V8,x143V9,x144V1,x144V7,x145V2,x145V5,x145V6,x145V7,x145V8,x145V10,x146V3,x146V5,x147V1,x147V4,x147V5,x147V6,x147V8,x147V10,x148V2,x148V4,x148V5,x148V7,x148V8,x149V4,x149V6,x150V4,x150V5,x150V6,x150V7,x150V8,x151V1,x151V2,x151V5,x151V6,x151V7,x151V9,x151V10,x152V2,x152V5,x152V7,x152V10,x153V6,x153V7,x153V9,x154V2,x154V4,x154V5,x154V6,x154V7,x154V9,x154V10,x155V3,x155V5,x155V7,x155V8,x155V10,x156V1,x156V3,x156V4,x156V6,x156V7,x156V9,x156V10,x157V1,x157V3,x157V4,x157V5,x157V6,x157V9,x158V1,x158V5,x158V6,x158V10,x159V7,x159V10,x160V2,x160V3,x160V4,x160V8,x160V9,x161V3,x161V5,x161V9,x162V1,x162V2,x162V3,x162V4,x162V6,x162V7,x162V8,x162V9,x162V10,x163V1,x163V4,x163V6,x163V7,x163V8,x163V10,x164V1,x164V3,x164V4,x164V5,x164V6,x164V9,x164V10,x165V2,x165V3,x165V6,x165V7,x165V8,x166V1,x166V3,x166V4,x166V6,x167V6,x167V9,x168V2,x168V3,x168V5,x168V6,x168V9,x168V10,x169V2,x169V4,x169V7,x169V8,x170V2,x170V4,x170V9,x171V3,x171V5,x171V6,x171V7,x172V1,x172V2,x172V4,x172V8,x172V9,x173V2,x173V3,x173V4,x173V6,x173V10,x174V1,x174V2,x174V3,x174V4,x174V6,x174V7,x174V8,x174V10,x175V2,x175V3,x175V4,x175V5,x175V6,x175V10,x176V3,x176V6,x177V2,x177V7,x178V1,x178V2,x178V3,x178V4,x178V5,x178V7,x178V10,x179V6,x179V7,x179V9,x180V4,x180V6,x180V7,x180V8,x180V9,x181V1,x181V5,x181V8,x181V10,x182V1,x182V2,x182V4,x182V8,x182V9,x182V10,x183V1,x183V2,x183V4,x183V8,x183V9,x184V1,x184V2,x184V4,x184V5,x184V6,x184V9,x184V10,x185V1,x185V2,x186V1,x186V2,x186V3,x186V4,x186V8,x186V9,x186V10,x187V3,x187V6,x187V7,x187V9,x187V10,x188V4,x188V5,x188V6,x188V7,x188V8,x188V9,x188V10,x189V1,x189V2,x189V4,x189V6,x189V10,x190V1,x190V2,x190V3,x190V4,x190V6,x190V8,x191V1,x191V2,x191V4,x191V8,x191V9,x192V1,x192V3,x192V4,x192V5,x192V6,x192V7,x192V9,x193V1,x193V5,x193V6,x193V7,x193V9,x193V10,x194V1,x194V3,x194V7,x194V10,x195V2,x195V4,x195V5,x195V8,x195V9,x195V10,x196V2,x196V3,x196V5,x196V7,x196V10,x197V7,x197V10,x198V1,x198V2,x198V5,x198V6,x198V9,x198V10,x199V1,x199V4,x199V5,x199V7,x199V8,x199V9,x199V10,x200V2,x200V3,x200V4,x200V5,x200V6,x200V10}
allVarsWithRange
({{x1V5,0,1},{x1V6,0,36},{x1V7,0,13}}
{{x2V2,0,3},{x2V5,0,112},{x2V6,0,114},{x2V7,0,15},{x2V9,0,228},{x2V10,0,17}}
{{x3V1,0,128},{x3V3,0,4}}
{{x4V1,0,25},{x4V2,0,27},{x4V4,0,331},{x4V8,0,335},{x4V9,0,333},{x4V10,0,329}}
{{x5V1,0,22},{x5V2,0,20},{x5V4,0,259},{x5V6,0,101},{x5V7,0,101},{x5V8,0,22},{x5V9,0,257}}
{{x6V1,0,390},{x6V4,0,71},{x6V8,0,390},{x6V10,0,35}}
{{x7V2,0,144},{x7V4,0,56},{x7V6,0,144},{x7V10,0,150}}
{{x8V3,0,532},{x8V7,0,104},{x8V8,0,538},{x8V10,0,532}}
{{x9V1,0,24},{x9V2,0,18},{x9V4,0,103},{x9V6,0,105},{x9V8,0,22},{x9V9,0,51},{x9V10,0,22}}
{{x10V5,0,320},{x10V7,0,326},{x10V8,0,63},{x10V10,0,324}}
{{x11V1,0,112},{x11V2,0,239},{x11V3,0,20},{x11V4,0,20},{x11V9,0,46},{x11V10,0,40}}
{{x12V1,0,19},{x12V3,0,235},{x12V4,0,229},{x12V5,0,235},{x12V8,0,231},{x12V9,0,17},{x12V10,0,472}}
{{x13V1,0,91},{x13V3,0,17},{x13V4,0,5},{x13V7,0,95},{x13V10,0,5}}
{{x14V1,0,202},{x14V2,0,96},{x14V3,0,204},{x14V4,0,200},{x14V5,0,14},{x14V6,0,33},{x14V7,0,33},{x14V10,0,200}}
{{x15V1,0,553},{x15V2,0,108},{x15V5,0,104}}
{{x16V1,0,93},{x16V3,0,91}}
{{x17V2,0,20},{x17V3,0,437},{x17V4,0,42},{x17V5,0,80},{x17V6,0,40},{x17V8,0,437},{x17V9,0,84},{x17V10,0,20}}
{{x18V1,0,22},{x18V2,0,485},{x18V3,0,239},{x18V4,0,93},{x18V5,0,22},{x18V6,0,91},{x18V7,0,46},{x18V10,0,481}}
{{x19V3,0,16},{x19V5,0,14},{x19V8,0,395},{x19V9,0,12},{x19V10,0,78}}
{{x20V1,0,9},{x20V2,0,274},{x20V3,0,53},{x20V6,0,47},{x20V8,0,11},{x20V10,0,276}}
{{x21V3,0,1},{x21V5,0,12},{x21V6,0,63},{x21V7,0,3}}
{{x22V1,0,106},{x22V3,0,18},{x22V4,0,1},{x22V6,0,112},{x22V10,0,106}}
{{x23V4,0,71},{x23V5,0,35}}
{{x24V1,0,1},{x24V3,0,13},{x24V4,0,1},{x24V5,0,6},{x24V7,0,4},{x24V9,0,6},{x24V10,0,1}}
{{x25V2,0,1},{x25V4,0,2},{x25V7,0,9},{x25V9,0,1}}
{{x26V1,0,221},{x26V6,0,225},{x26V7,0,16},{x26V9,0,227}}
{{x27V2,0,14},{x27V4,0,93},{x27V5,0,12}}
{{x28V1,0,32},{x28V6,0,199},{x28V7,0,394},{x28V8,0,34},{x28V9,0,394}}
{{x29V1,0,35},{x29V2,0,70},{x29V3,0,29},{x29V4,0,68},{x29V5,0,14},{x29V8,0,72},{x29V9,0,14}}
{{x30V1,0,2},{x30V4,0,41}}
{{x31V1,0,1},{x31V3,0,137},{x31V5,0,6},{x31V6,0,6},{x31V7,0,24},{x31V10,0,3}}
{{x32V1,0,23},{x32V3,0,17},{x32V8,0,21},{x32V9,0,44},{x32V10,0,502}}
{{x33V7,0,324},{x33V10,0,324}}
{{x34V4,0,278},{x34V6,0,138},{x34V7,0,136},{x34V8,0,280},{x34V10,0,278}}
{{x35V3,0,50},{x35V4,0,253},{x35V8,0,98}}
{{x36V1,0,9},{x36V2,0,11},{x36V3,0,15},{x36V4,0,27},{x36V6,0,171},{x36V9,0,173}}
{{x37V2,0,569},{x37V3,0,567},{x37V9,0,281}}
{{x38V8,0,170},{x38V9,0,29}}
{{x39V2,0,49},{x39V4,0,45},{x39V8,0,45},{x39V9,0,123}}
{{x40V1,0,378},{x40V9,0,11}}
{{x41V1,0,110},{x41V3,0,54},{x41V5,0,284},{x41V6,0,23},{x41V7,0,25},{x41V8,0,288},{x41V9,0,290},{x41V10,0,582}}
{{x42V1,0,19},{x42V3,0,47},{x42V4,0,19},{x42V6,0,21},{x42V7,0,269},{x42V9,0,104},{x42V10,0,108}}
{{x43V1,0,34},{x43V3,0,32},{x43V4,0,3},{x43V5,0,88},{x43V8,0,16},{x43V10,0,14}}
{{x44V2,0,47},{x44V4,0,266},{x44V6,0,262},{x44V7,0,49},{x44V10,0,45}}
{{x45V1,0,8},{x45V3,0,119},{x45V8,0,125},{x45V10,0,19}}
{{x46V1,0,49},{x46V2,0,20},{x46V3,0,140},{x46V4,0,49},{x46V5,0,26},{x46V6,0,10}}
{{x47V1,0,13},{x47V3,0,422},{x47V5,0,35},{x47V6,0,426},{x47V8,0,41},{x47V10,0,41}}
{{x48V2,0,1},{x48V3,0,1},{x48V4,0,13},{x48V6,0,1},{x48V7,0,1},{x48V8,0,1}}
{{x49V2,0,133},{x49V3,0,7},{x49V4,0,269},{x49V5,0,23},{x49V9,0,52}}
{{x50V1,0,22},{x50V3,0,131},{x50V6,0,5},{x50V8,0,5},{x50V9,0,60}}
{{x51V1,0,71},{x51V4,0,1},{x51V6,0,13},{x51V8,0,11},{x51V9,0,5}}
{{x52V3,0,517},{x52V8,0,48}}
{{x53V1,0,46},{x53V2,0,23},{x53V7,0,25},{x53V8,0,267},{x53V9,0,104},{x53V10,0,269}}
{{x54V4,0,51},{x54V10,0,289}}
{{x55V1,0,33},{x55V2,0,33},{x55V4,0,68},{x55V6,0,1},{x55V7,0,1},{x55V8,0,27},{x55V9,0,29},{x55V10,0,1}}
{{x56V2,0,522},{x56V3,0,96},{x56V8,0,18}}
{{x57V1,0,1},{x57V4,0,1},{x57V5,0,14},{x57V6,0,43},{x57V8,0,90},{x57V9,0,1},{x57V10,0,1}}
{{x58V1,0,71},{x58V2,0,13},{x58V3,0,31},{x58V4,0,77},{x58V5,0,33},{x58V6,0,394},{x58V7,0,190},{x58V8,0,194},{x58V9,0,190},{x58V10,0,11}}
{{x59V5,0,586},{x59V7,0,115}}
{{x60V2,0,109},{x60V4,0,21},{x60V7,0,583},{x60V9,0,290},{x60V10,0,579}}
{{x61V2,0,82},{x61V5,0,16},{x61V7,0,219},{x61V9,0,82}}
{{x62V2,0,49},{x62V7,0,100}}
{{x63V3,0,66},{x63V4,0,32},{x63V5,0,64},{x63V6,0,28},{x63V7,0,168},{x63V8,0,339},{x63V9,0,337}}
{{x64V2,0,67},{x64V4,0,176},{x64V8,0,357}}
{{x65V1,0,7},{x65V3,0,9},{x65V4,0,24},{x65V9,0,5},{x65V10,0,1}}
{{x66V2,0,5},{x66V3,0,29},{x66V4,0,183},{x66V5,0,33},{x66V6,0,179},{x66V8,0,85},{x66V9,0,10}}
{{x67V4,0,240},{x67V8,0,22}}
{{x68V1,0,1},{x68V4,0,1},{x68V5,0,11},{x68V7,0,1},{x68V8,0,7},{x68V9,0,1}}
{{x69V1,0,242},{x69V2,0,121},{x69V4,0,117},{x69V5,0,42},{x69V7,0,21},{x69V8,0,46},{x69V9,0,244}}
{{x70V2,0,546},{x70V3,0,272},{x70V4,0,272},{x70V6,0,106},{x70V7,0,544},{x70V10,0,544}}
{{x71V1,0,496},{x71V3,0,92},{x71V7,0,17},{x71V8,0,248},{x71V9,0,494}}
{{x72V1,0,1},{x72V7,0,1}}
{{x73V1,0,21},{x73V2,0,266},{x73V6,0,272},{x73V8,0,131},{x73V9,0,23}}
{{x74V2,0,375},{x74V3,0,30},{x74V4,0,184},{x74V6,0,379}}
{{x75V1,0,1},{x75V4,0,92},{x75V5,0,3},{x75V6,0,40},{x75V7,0,3},{x75V9,0,94},{x75V10,0,94}}
{{x76V2,0,16},{x76V3,0,78},{x76V4,0,32},{x76V7,0,394},{x76V8,0,194},{x76V9,0,12},{x76V10,0,72}}
{{x77V3,0,1},{x77V4,0,1},{x77V6,0,1},{x77V10,0,16}}
{{x78V1,0,409},{x78V2,0,199},{x78V3,0,199},{x78V4,0,409},{x78V5,0,203},{x78V6,0,203}}
{{x79V2,0,210},{x79V6,0,15},{x79V7,0,83}}
{{x80V1,0,282},{x80V5,0,22},{x80V9,0,278}}
{{x81V1,0,200},{x81V3,0,38},{x81V8,0,36},{x81V9,0,36}}
{{x82V1,0,29},{x82V2,0,13},{x82V4,0,59},{x82V6,0,25},{x82V7,0,157},{x82V9,0,55},{x82V10,0,153}}
{{x83V1,0,28},{x83V2,0,296},{x83V9,0,26}}
{{x84V2,0,229},{x84V4,0,42},{x84V5,0,44},{x84V6,0,229},{x84V7,0,229},{x84V8,0,229},{x84V10,0,40}}
{{x85V1,0,32},{x85V3,0,32},{x85V4,0,170},{x85V6,0,11},{x85V7,0,340},{x85V8,0,66},{x85V9,0,15}}
{{x86V1,0,248},{x86V2,0,21},{x86V4,0,120},{x86V6,0,10},{x86V7,0,120},{x86V8,0,126},{x86V9,0,252},{x86V10,0,49}}
{{x87V1,0,11},{x87V6,0,152},{x87V7,0,25},{x87V8,0,70},{x87V9,0,25}}
{{x88V1,0,64},{x88V2,0,20},{x88V4,0,26},{x88V5,0,142},{x88V6,0,22},{x88V7,0,22},{x88V8,0,5},{x88V9,0,26},{x88V10,0,3}}
{{x89V2,0,5},{x89V6,0,9},{x89V8,0,17}}
{{x90V1,0,226},{x90V3,0,88},{x90V4,0,222},{x90V5,0,88},{x90V7,0,38},{x90V8,0,84},{x90V9,0,21}}
{{x91V6,0,14},{x91V8,0,194}}
{{x92V2,0,1},{x92V4,0,81},{x92V7,0,8},{x92V10,0,10}}
{{x93V1,0,1},{x93V2,0,1},{x93V4,0,35},{x93V5,0,1},{x93V7,0,17},{x93V8,0,37}}
{{x94V2,0,110},{x94V3,0,37},{x94V5,0,39},{x94V7,0,14},{x94V8,0,7},{x94V9,0,16},{x94V10,0,224}}
{{x95V1,0,28},{x95V3,0,13},{x95V5,0,6},{x95V8,0,13},{x95V9,0,28},{x95V10,0,170}}
{{x96V3,0,31},{x96V4,0,33},{x96V5,0,176},{x96V6,0,349},{x96V7,0,63},{x96V9,0,355},{x96V10,0,65}}
{{x97V1,0,419},{x97V2,0,415},{x97V3,0,78},{x97V6,0,15},{x97V9,0,82}}
{{x98V1,0,1},{x98V2,0,1},{x98V3,0,20},{x98V5,0,22},{x98V6,0,44},{x98V7,0,5},{x98V9,0,40},{x98V10,0,1}}
{{x99V3,0,9},{x99V4,0,17},{x99V9,0,1}}
{{x100V1,0,118},{x100V3,0,2},{x100V4,0,55},{x100V6,0,116},{x100V8,0,1},{x100V9,0,120},{x100V10,0,1}}
{{x101V2,0,65},{x101V3,0,4},{x101V5,0,2},{x101V6,0,128},{x101V7,0,63},{x101V9,0,59}}
{{x102V1,0,309},{x102V3,0,59},{x102V4,0,149},{x102V7,0,311},{x102V9,0,149}}
{{x103V1,0,406},{x103V2,0,202},{x103V3,0,16},{x103V4,0,16},{x103V6,0,39},{x103V9,0,35},{x103V10,0,406}}
{{x104V6,0,41},{x104V9,0,45}}
{{x105V6,0,384},{x105V7,0,15}}
{{x106V3,0,585},{x106V4,0,287},{x106V6,0,23},{x106V8,0,287},{x106V9,0,291},{x106V10,0,116}}
{{x107V1,0,4},{x107V2,0,60},{x107V5,0,125},{x107V7,0,4},{x107V9,0,121}}
{{x108V2,0,13},{x108V5,0,194}}
{{x109V1,0,112},{x109V4,0,112},{x109V6,0,8},{x109V7,0,42},{x109V8,0,234}}
{{x110V7,0,98},{x110V10,0,22}}
{{x111V1,0,275},{x111V2,0,565},{x111V3,0,277},{x111V4,0,24},{x111V7,0,48},{x111V8,0,107},{x111V9,0,107}}
{{x112V3,0,166},{x112V4,0,332},{x112V5,0,66},{x112V6,0,11},{x112V7,0,338},{x112V9,0,60}}
{{x113V1,0,55},{x113V4,0,287},{x113V5,0,283},{x113V6,0,10},{x113V7,0,22},{x113V9,0,8},{x113V10,0,6}}
{{x114V1,0,231},{x114V2,0,17},{x114V3,0,19},{x114V4,0,235},{x114V6,0,471},{x114V7,0,87},{x114V8,0,21},{x114V9,0,233},{x114V10,0,39}}
{{x115V1,0,35},{x115V4,0,11},{x115V7,0,11},{x115V9,0,192}}
{{x116V1,0,459},{x116V2,0,459},{x116V4,0,228},{x116V5,0,230},{x116V6,0,226},{x116V8,0,228},{x116V9,0,230},{x116V10,0,44}}
{{x117V1,0,10},{x117V3,0,69},{x117V4,0,14},{x117V6,0,16},{x117V7,0,31},{x117V9,0,14},{x117V10,0,73}}
{{x118V1,0,97},{x118V7,0,495},{x118V8,0,41},{x118V9,0,493},{x118V10,0,245}}
{{x119V1,0,1},{x119V9,0,16}}
{{x120V4,0,23},{x120V7,0,21},{x120V9,0,274}}
{{x121V5,0,108},{x121V6,0,110}}
{{x122V5,0,123},{x122V7,0,245},{x122V8,0,10},{x122V9,0,117},{x122V10,0,19}}
{{x123V1,0,31},{x123V2,0,37},{x123V4,0,35},{x123V6,0,1},{x123V9,0,35},{x123V10,0,1}}
{{x124V1,0,10},{x124V5,0,10},{x124V7,0,12}}
{{x125V2,0,487},{x125V4,0,483},{x125V5,0,20},{x125V6,0,485},{x125V7,0,243},{x125V9,0,92},{x125V10,0,241}}
{{x126V1,0,1},{x126V2,0,1},{x126V4,0,2},{x126V6,0,56},{x126V8,0,1},{x126V9,0,1}}
{{x127V1,0,21},{x127V2,0,585},{x127V4,0,291},{x127V5,0,287},{x127V6,0,589},{x127V7,0,110},{x127V9,0,287},{x127V10,0,112}}
{{x128V1,0,19},{x128V4,0,5}}
{{x129V2,0,385},{x129V5,0,15},{x129V7,0,35},{x129V9,0,191},{x129V10,0,72}}
{{x130V1,0,199},{x130V10,0,4}}
{{x131V1,0,273},{x131V2,0,271},{x131V3,0,132},{x131V4,0,136},{x131V5,0,49},{x131V7,0,19}}
{{x132V1,0,11},{x132V3,0,339},{x132V4,0,337},{x132V5,0,9},{x132V6,0,162},{x132V7,0,337},{x132V9,0,60}}
{{x133V1,0,87},{x133V2,0,465},{x133V4,0,91},{x133V5,0,85},{x133V8,0,89},{x133V10,0,465}}
{{x134V1,0,80},{x134V2,0,80},{x134V3,0,11},{x134V5,0,86},{x134V7,0,80},{x134V8,0,13},{x134V9,0,1},{x134V10,0,1}}
{{x135V1,0,1},{x135V3,0,35},{x135V6,0,1},{x135V7,0,13}}
{{x136V2,0,35},{x136V3,0,35},{x136V5,0,192},{x136V6,0,188},{x136V7,0,192}}
{{x137V2,0,19},{x137V3,0,93},{x137V4,0,41},{x137V8,0,231}}
{{x138V2,0,23},{x138V3,0,6},{x138V4,0,27},{x138V5,0,57},{x138V6,0,23},{x138V7,0,292},{x138V8,0,6}}
{{x139V1,0,312},{x139V3,0,318},{x139V4,0,8},{x139V5,0,30},{x139V6,0,56},{x139V8,0,10},{x139V9,0,14}}
{{x140V1,0,1},{x140V3,0,1},{x140V4,0,1},{x140V5,0,1},{x140V7,0,1},{x140V8,0,1},{x140V9,0,1}}
{{x141V2,0,153},{x141V4,0,155},{x141V5,0,61},{x141V6,0,23},{x141V7,0,61},{x141V8,0,9},{x141V10,0,151}}
{{x142V1,0,21},{x142V2,0,48},{x142V3,0,264},{x142V4,0,264},{x142V6,0,48},{x142V8,0,104},{x142V9,0,538}}
{{x143V2,0,427},{x143V6,0,77},{x143V8,0,427},{x143V9,0,40}}
{{x144V1,0,32},{x144V7,0,14}}
{{x145V2,0,497},{x145V5,0,499},{x145V6,0,48},{x145V7,0,95},{x145V8,0,44},{x145V10,0,501}}
{{x146V3,0,13},{x146V5,0,58}}
{{x147V1,0,229},{x147V4,0,231},{x147V5,0,233},{x147V6,0,227},{x147V8,0,109},{x147V10,0,21}}
{{x148V2,0,47},{x148V4,0,43},{x148V5,0,124},{x148V7,0,120},{x148V8,0,252}}
{{x149V4,0,12},{x149V6,0,16}}
{{x150V4,0,5},{x150V5,0,9},{x150V6,0,7},{x150V7,0,214},{x150V8,0,14}}
{{x151V1,0,7},{x151V2,0,22},{x151V5,0,125},{x151V6,0,46},{x151V7,0,258},{x151V9,0,24},{x151V10,0,50}}
{{x152V2,0,487},{x152V5,0,18},{x152V7,0,45},{x152V10,0,94}}
{{x153V6,0,1},{x153V7,0,22},{x153V9,0,44}}
{{x154V2,0,20},{x154V4,0,20},{x154V5,0,97},{x154V6,0,20},{x154V7,0,20},{x154V9,0,255},{x154V10,0,521}}
{{x155V3,0,17},{x155V5,0,232},{x155V7,0,38},{x155V8,0,109},{x155V10,0,19}}
{{x156V1,0,91},{x156V3,0,97},{x156V4,0,31},{x156V6,0,197},{x156V7,0,35},{x156V9,0,95},{x156V10,0,195}}
{{x157V1,0,1},{x157V3,0,6},{x157V4,0,8},{x157V5,0,14},{x157V6,0,1},{x157V9,0,43}}
{{x158V1,0,113},{x158V5,0,39},{x158V6,0,17},{x158V10,0,231}}
{{x159V7,0,95},{x159V10,0,44}}
{{x160V2,0,296},{x160V3,0,54},{x160V4,0,9},{x160V8,0,7},{x160V9,0,24}}
{{x161V3,0,69},{x161V5,0,10},{x161V9,0,370}}
{{x162V1,0,15},{x162V2,0,227},{x162V3,0,38},{x162V4,0,225},{x162V6,0,227},{x162V7,0,464},{x162V8,0,38},{x162V9,0,85},{x162V10,0,19}}
{{x163V1,0,3},{x163V4,0,11},{x163V6,0,13},{x163V7,0,31},{x163V8,0,3},{x163V10,0,1}}
{{x164V1,0,63},{x164V3,0,332},{x164V4,0,61},{x164V5,0,59},{x164V6,0,160},{x164V9,0,164},{x164V10,0,332}}
{{x165V2,0,146},{x165V3,0,146},{x165V6,0,306},{x165V7,0,57},{x165V8,0,24}}
{{x166V1,0,104},{x166V3,0,23},{x166V4,0,21},{x166V6,0,543}}
{{x167V6,0,73},{x167V9,0,5}}
{{x168V2,0,9},{x168V3,0,1},{x168V5,0,5},{x168V6,0,128},{x168V9,0,20},{x168V10,0,7}}
{{x169V2,0,2},{x169V4,0,4},{x169V7,0,2},{x169V8,0,6}}
{{x170V2,0,58},{x170V4,0,54},{x170V9,0,121}}
{{x171V3,0,6},{x171V5,0,25},{x171V6,0,3},{x171V7,0,25}}
{{x172V1,0,52},{x172V2,0,19},{x172V4,0,48},{x172V8,0,104},{x172V9,0,1}}
{{x173V2,0,44},{x173V3,0,40},{x173V4,0,455},{x173V6,0,459},{x173V10,0,44}}
{{x174V1,0,1},{x174V2,0,26},{x174V3,0,13},{x174V4,0,2},{x174V6,0,4},{x174V7,0,4},{x174V8,0,11},{x174V10,0,1}}
{{x175V2,0,276},{x175V3,0,51},{x175V4,0,274},{x175V5,0,49},{x175V6,0,11},{x175V10,0,272}}
{{x176V3,0,89},{x176V6,0,32}}
{{x177V2,0,1},{x177V7,0,30}}
{{x178V1,0,252},{x178V2,0,248},{x178V3,0,122},{x178V4,0,254},{x178V5,0,21},{x178V7,0,248},{x178V10,0,4}}
{{x179V6,0,268},{x179V7,0,127},{x179V9,0,264}}
{{x180V4,0,83},{x180V6,0,32},{x180V7,0,28},{x180V8,0,176},{x180V9,0,180}}
{{x181V1,0,1},{x181V5,0,27},{x181V8,0,1},{x181V10,0,2}}
{{x182V1,0,56},{x182V2,0,579},{x182V4,0,52},{x182V8,0,21},{x182V9,0,23},{x182V10,0,579}}
{{x183V1,0,9},{x183V2,0,11},{x183V4,0,5},{x183V8,0,133},{x183V9,0,25}}
{{x184V1,0,114},{x184V2,0,25},{x184V4,0,108},{x184V5,0,56},{x184V6,0,578},{x184V9,0,54},{x184V10,0,574}}
{{x185V1,0,17},{x185V2,0,386}}
{{x186V1,0,1},{x186V2,0,1},{x186V3,0,1},{x186V4,0,1},{x186V8,0,1},{x186V9,0,1},{x186V10,0,1}}
{{x187V3,0,20},{x187V6,0,279},{x187V7,0,558},{x187V9,0,26},{x187V10,0,110}}
{{x188V4,0,8},{x188V5,0,143},{x188V6,0,57},{x188V7,0,147},{x188V8,0,21},{x188V9,0,143},{x188V10,0,51}}
{{x189V1,0,40},{x189V2,0,230},{x189V4,0,17},{x189V6,0,461},{x189V10,0,42}}
{{x190V1,0,184},{x190V2,0,364},{x190V3,0,366},{x190V4,0,370},{x190V6,0,29},{x190V8,0,180}}
{{x191V1,0,46},{x191V2,0,48},{x191V4,0,96},{x191V8,0,259},{x191V9,0,514}}
{{x192V1,0,84},{x192V3,0,1},{x192V4,0,179},{x192V5,0,177},{x192V6,0,14},{x192V7,0,31},{x192V9,0,90}}
{{x193V1,0,111},{x193V5,0,286},{x193V6,0,25},{x193V7,0,290},{x193V9,0,580},{x193V10,0,27}}
{{x194V1,0,104},{x194V3,0,275},{x194V7,0,48},{x194V10,0,20}}
{{x195V2,0,256},{x195V4,0,24},{x195V5,0,22},{x195V8,0,11},{x195V9,0,18},{x195V10,0,128}}
{{x196V2,0,6},{x196V3,0,5},{x196V5,0,24},{x196V7,0,1},{x196V10,0,63}}
{{x197V7,0,1},{x197V10,0,2}}
{{x198V1,0,92},{x198V2,0,181},{x198V5,0,185},{x198V6,0,7},{x198V9,0,3},{x198V10,0,181}}
{{x199V1,0,11},{x199V4,0,15},{x199V5,0,32},{x199V7,0,184},{x199V8,0,30},{x199V9,0,184},{x199V10,0,186}}
{{x200V2,0,14},{x200V3,0,74},{x200V4,0,29},{x200V5,0,6},{x200V6,0,163},{x200V10,0,25}}

)
allVarsWithRangeLinear
{{x1V5,0,1},{x1V6,0,36},{x1V7,0,13},{x2V2,0,3},{x2V5,0,112},{x2V6,0,114},{x2V7,0,15},{x2V9,0,228},{x2V10,0,17},{x3V1,0,128},{x3V3,0,4},{x4V1,0,25},{x4V2,0,27},{x4V4,0,331},{x4V8,0,335},{x4V9,0,333},{x4V10,0,329},{x5V1,0,22},{x5V2,0,20},{x5V4,0,259},{x5V6,0,101},{x5V7,0,101},{x5V8,0,22},{x5V9,0,257},{x6V1,0,390},{x6V4,0,71},{x6V8,0,390},{x6V10,0,35},{x7V2,0,144},{x7V4,0,56},{x7V6,0,144},{x7V10,0,150},{x8V3,0,532},{x8V7,0,104},{x8V8,0,538},{x8V10,0,532},{x9V1,0,24},{x9V2,0,18},{x9V4,0,103},{x9V6,0,105},{x9V8,0,22},{x9V9,0,51},{x9V10,0,22},{x10V5,0,320},{x10V7,0,326},{x10V8,0,63},{x10V10,0,324},{x11V1,0,112},{x11V2,0,239},{x11V3,0,20},{x11V4,0,20},{x11V9,0,46},{x11V10,0,40},{x12V1,0,19},{x12V3,0,235},{x12V4,0,229},{x12V5,0,235},{x12V8,0,231},{x12V9,0,17},{x12V10,0,472},{x13V1,0,91},{x13V3,0,17},{x13V4,0,5},{x13V7,0,95},{x13V10,0,5},{x14V1,0,202},{x14V2,0,96},{x14V3,0,204},{x14V4,0,200},{x14V5,0,14},{x14V6,0,33},{x14V7,0,33},{x14V10,0,200},{x15V1,0,553},{x15V2,0,108},{x15V5,0,104},{x16V1,0,93},{x16V3,0,91},{x17V2,0,20},{x17V3,0,437},{x17V4,0,42},{x17V5,0,80},{x17V6,0,40},{x17V8,0,437},{x17V9,0,84},{x17V10,0,20},{x18V1,0,22},{x18V2,0,485},{x18V3,0,239},{x18V4,0,93},{x18V5,0,22},{x18V6,0,91},{x18V7,0,46},{x18V10,0,481},{x19V3,0,16},{x19V5,0,14},{x19V8,0,395},{x19V9,0,12},{x19V10,0,78},{x20V1,0,9},{x20V2,0,274},{x20V3,0,53},{x20V6,0,47},{x20V8,0,11},{x20V10,0,276},{x21V3,0,1},{x21V5,0,12},{x21V6,0,63},{x21V7,0,3},{x22V1,0,106},{x22V3,0,18},{x22V4,0,1},{x22V6,0,112},{x22V10,0,106},{x23V4,0,71},{x23V5,0,35},{x24V1,0,1},{x24V3,0,13},{x24V4,0,1},{x24V5,0,6},{x24V7,0,4},{x24V9,0,6},{x24V10,0,1},{x25V2,0,1},{x25V4,0,2},{x25V7,0,9},{x25V9,0,1},{x26V1,0,221},{x26V6,0,225},{x26V7,0,16},{x26V9,0,227},{x27V2,0,14},{x27V4,0,93},{x27V5,0,12},{x28V1,0,32},{x28V6,0,199},{x28V7,0,394},{x28V8,0,34},{x28V9,0,394},{x29V1,0,35},{x29V2,0,70},{x29V3,0,29},{x29V4,0,68},{x29V5,0,14},{x29V8,0,72},{x29V9,0,14},{x30V1,0,2},{x30V4,0,41},{x31V1,0,1},{x31V3,0,137},{x31V5,0,6},{x31V6,0,6},{x31V7,0,24},{x31V10,0,3},{x32V1,0,23},{x32V3,0,17},{x32V8,0,21},{x32V9,0,44},{x32V10,0,502},{x33V7,0,324},{x33V10,0,324},{x34V4,0,278},{x34V6,0,138},{x34V7,0,136},{x34V8,0,280},{x34V10,0,278},{x35V3,0,50},{x35V4,0,253},{x35V8,0,98},{x36V1,0,9},{x36V2,0,11},{x36V3,0,15},{x36V4,0,27},{x36V6,0,171},{x36V9,0,173},{x37V2,0,569},{x37V3,0,567},{x37V9,0,281},{x38V8,0,170},{x38V9,0,29},{x39V2,0,49},{x39V4,0,45},{x39V8,0,45},{x39V9,0,123},{x40V1,0,378},{x40V9,0,11},{x41V1,0,110},{x41V3,0,54},{x41V5,0,284},{x41V6,0,23},{x41V7,0,25},{x41V8,0,288},{x41V9,0,290},{x41V10,0,582},{x42V1,0,19},{x42V3,0,47},{x42V4,0,19},{x42V6,0,21},{x42V7,0,269},{x42V9,0,104},{x42V10,0,108},{x43V1,0,34},{x43V3,0,32},{x43V4,0,3},{x43V5,0,88},{x43V8,0,16},{x43V10,0,14},{x44V2,0,47},{x44V4,0,266},{x44V6,0,262},{x44V7,0,49},{x44V10,0,45},{x45V1,0,8},{x45V3,0,119},{x45V8,0,125},{x45V10,0,19},{x46V1,0,49},{x46V2,0,20},{x46V3,0,140},{x46V4,0,49},{x46V5,0,26},{x46V6,0,10},{x47V1,0,13},{x47V3,0,422},{x47V5,0,35},{x47V6,0,426},{x47V8,0,41},{x47V10,0,41},{x48V2,0,1},{x48V3,0,1},{x48V4,0,13},{x48V6,0,1},{x48V7,0,1},{x48V8,0,1},{x49V2,0,133},{x49V3,0,7},{x49V4,0,269},{x49V5,0,23},{x49V9,0,52},{x50V1,0,22},{x50V3,0,131},{x50V6,0,5},{x50V8,0,5},{x50V9,0,60},{x51V1,0,71},{x51V4,0,1},{x51V6,0,13},{x51V8,0,11},{x51V9,0,5},{x52V3,0,517},{x52V8,0,48},{x53V1,0,46},{x53V2,0,23},{x53V7,0,25},{x53V8,0,267},{x53V9,0,104},{x53V10,0,269},{x54V4,0,51},{x54V10,0,289},{x55V1,0,33},{x55V2,0,33},{x55V4,0,68},{x55V6,0,1},{x55V7,0,1},{x55V8,0,27},{x55V9,0,29},{x55V10,0,1},{x56V2,0,522},{x56V3,0,96},{x56V8,0,18},{x57V1,0,1},{x57V4,0,1},{x57V5,0,14},{x57V6,0,43},{x57V8,0,90},{x57V9,0,1},{x57V10,0,1},{x58V1,0,71},{x58V2,0,13},{x58V3,0,31},{x58V4,0,77},{x58V5,0,33},{x58V6,0,394},{x58V7,0,190},{x58V8,0,194},{x58V9,0,190},{x58V10,0,11},{x59V5,0,586},{x59V7,0,115},{x60V2,0,109},{x60V4,0,21},{x60V7,0,583},{x60V9,0,290},{x60V10,0,579},{x61V2,0,82},{x61V5,0,16},{x61V7,0,219},{x61V9,0,82},{x62V2,0,49},{x62V7,0,100},{x63V3,0,66},{x63V4,0,32},{x63V5,0,64},{x63V6,0,28},{x63V7,0,168},{x63V8,0,339},{x63V9,0,337},{x64V2,0,67},{x64V4,0,176},{x64V8,0,357},{x65V1,0,7},{x65V3,0,9},{x65V4,0,24},{x65V9,0,5},{x65V10,0,1},{x66V2,0,5},{x66V3,0,29},{x66V4,0,183},{x66V5,0,33},{x66V6,0,179},{x66V8,0,85},{x66V9,0,10},{x67V4,0,240},{x67V8,0,22},{x68V1,0,1},{x68V4,0,1},{x68V5,0,11},{x68V7,0,1},{x68V8,0,7},{x68V9,0,1},{x69V1,0,242},{x69V2,0,121},{x69V4,0,117},{x69V5,0,42},{x69V7,0,21},{x69V8,0,46},{x69V9,0,244},{x70V2,0,546},{x70V3,0,272},{x70V4,0,272},{x70V6,0,106},{x70V7,0,544},{x70V10,0,544},{x71V1,0,496},{x71V3,0,92},{x71V7,0,17},{x71V8,0,248},{x71V9,0,494},{x72V1,0,1},{x72V7,0,1},{x73V1,0,21},{x73V2,0,266},{x73V6,0,272},{x73V8,0,131},{x73V9,0,23},{x74V2,0,375},{x74V3,0,30},{x74V4,0,184},{x74V6,0,379},{x75V1,0,1},{x75V4,0,92},{x75V5,0,3},{x75V6,0,40},{x75V7,0,3},{x75V9,0,94},{x75V10,0,94},{x76V2,0,16},{x76V3,0,78},{x76V4,0,32},{x76V7,0,394},{x76V8,0,194},{x76V9,0,12},{x76V10,0,72},{x77V3,0,1},{x77V4,0,1},{x77V6,0,1},{x77V10,0,16},{x78V1,0,409},{x78V2,0,199},{x78V3,0,199},{x78V4,0,409},{x78V5,0,203},{x78V6,0,203},{x79V2,0,210},{x79V6,0,15},{x79V7,0,83},{x80V1,0,282},{x80V5,0,22},{x80V9,0,278},{x81V1,0,200},{x81V3,0,38},{x81V8,0,36},{x81V9,0,36},{x82V1,0,29},{x82V2,0,13},{x82V4,0,59},{x82V6,0,25},{x82V7,0,157},{x82V9,0,55},{x82V10,0,153},{x83V1,0,28},{x83V2,0,296},{x83V9,0,26},{x84V2,0,229},{x84V4,0,42},{x84V5,0,44},{x84V6,0,229},{x84V7,0,229},{x84V8,0,229},{x84V10,0,40},{x85V1,0,32},{x85V3,0,32},{x85V4,0,170},{x85V6,0,11},{x85V7,0,340},{x85V8,0,66},{x85V9,0,15},{x86V1,0,248},{x86V2,0,21},{x86V4,0,120},{x86V6,0,10},{x86V7,0,120},{x86V8,0,126},{x86V9,0,252},{x86V10,0,49},{x87V1,0,11},{x87V6,0,152},{x87V7,0,25},{x87V8,0,70},{x87V9,0,25},{x88V1,0,64},{x88V2,0,20},{x88V4,0,26},{x88V5,0,142},{x88V6,0,22},{x88V7,0,22},{x88V8,0,5},{x88V9,0,26},{x88V10,0,3},{x89V2,0,5},{x89V6,0,9},{x89V8,0,17},{x90V1,0,226},{x90V3,0,88},{x90V4,0,222},{x90V5,0,88},{x90V7,0,38},{x90V8,0,84},{x90V9,0,21},{x91V6,0,14},{x91V8,0,194},{x92V2,0,1},{x92V4,0,81},{x92V7,0,8},{x92V10,0,10},{x93V1,0,1},{x93V2,0,1},{x93V4,0,35},{x93V5,0,1},{x93V7,0,17},{x93V8,0,37},{x94V2,0,110},{x94V3,0,37},{x94V5,0,39},{x94V7,0,14},{x94V8,0,7},{x94V9,0,16},{x94V10,0,224},{x95V1,0,28},{x95V3,0,13},{x95V5,0,6},{x95V8,0,13},{x95V9,0,28},{x95V10,0,170},{x96V3,0,31},{x96V4,0,33},{x96V5,0,176},{x96V6,0,349},{x96V7,0,63},{x96V9,0,355},{x96V10,0,65},{x97V1,0,419},{x97V2,0,415},{x97V3,0,78},{x97V6,0,15},{x97V9,0,82},{x98V1,0,1},{x98V2,0,1},{x98V3,0,20},{x98V5,0,22},{x98V6,0,44},{x98V7,0,5},{x98V9,0,40},{x98V10,0,1},{x99V3,0,9},{x99V4,0,17},{x99V9,0,1},{x100V1,0,118},{x100V3,0,2},{x100V4,0,55},{x100V6,0,116},{x100V8,0,1},{x100V9,0,120},{x100V10,0,1},{x101V2,0,65},{x101V3,0,4},{x101V5,0,2},{x101V6,0,128},{x101V7,0,63},{x101V9,0,59},{x102V1,0,309},{x102V3,0,59},{x102V4,0,149},{x102V7,0,311},{x102V9,0,149},{x103V1,0,406},{x103V2,0,202},{x103V3,0,16},{x103V4,0,16},{x103V6,0,39},{x103V9,0,35},{x103V10,0,406},{x104V6,0,41},{x104V9,0,45},{x105V6,0,384},{x105V7,0,15},{x106V3,0,585},{x106V4,0,287},{x106V6,0,23},{x106V8,0,287},{x106V9,0,291},{x106V10,0,116},{x107V1,0,4},{x107V2,0,60},{x107V5,0,125},{x107V7,0,4},{x107V9,0,121},{x108V2,0,13},{x108V5,0,194},{x109V1,0,112},{x109V4,0,112},{x109V6,0,8},{x109V7,0,42},{x109V8,0,234},{x110V7,0,98},{x110V10,0,22},{x111V1,0,275},{x111V2,0,565},{x111V3,0,277},{x111V4,0,24},{x111V7,0,48},{x111V8,0,107},{x111V9,0,107},{x112V3,0,166},{x112V4,0,332},{x112V5,0,66},{x112V6,0,11},{x112V7,0,338},{x112V9,0,60},{x113V1,0,55},{x113V4,0,287},{x113V5,0,283},{x113V6,0,10},{x113V7,0,22},{x113V9,0,8},{x113V10,0,6},{x114V1,0,231},{x114V2,0,17},{x114V3,0,19},{x114V4,0,235},{x114V6,0,471},{x114V7,0,87},{x114V8,0,21},{x114V9,0,233},{x114V10,0,39},{x115V1,0,35},{x115V4,0,11},{x115V7,0,11},{x115V9,0,192},{x116V1,0,459},{x116V2,0,459},{x116V4,0,228},{x116V5,0,230},{x116V6,0,226},{x116V8,0,228},{x116V9,0,230},{x116V10,0,44},{x117V1,0,10},{x117V3,0,69},{x117V4,0,14},{x117V6,0,16},{x117V7,0,31},{x117V9,0,14},{x117V10,0,73},{x118V1,0,97},{x118V7,0,495},{x118V8,0,41},{x118V9,0,493},{x118V10,0,245},{x119V1,0,1},{x119V9,0,16},{x120V4,0,23},{x120V7,0,21},{x120V9,0,274},{x121V5,0,108},{x121V6,0,110},{x122V5,0,123},{x122V7,0,245},{x122V8,0,10},{x122V9,0,117},{x122V10,0,19},{x123V1,0,31},{x123V2,0,37},{x123V4,0,35},{x123V6,0,1},{x123V9,0,35},{x123V10,0,1},{x124V1,0,10},{x124V5,0,10},{x124V7,0,12},{x125V2,0,487},{x125V4,0,483},{x125V5,0,20},{x125V6,0,485},{x125V7,0,243},{x125V9,0,92},{x125V10,0,241},{x126V1,0,1},{x126V2,0,1},{x126V4,0,2},{x126V6,0,56},{x126V8,0,1},{x126V9,0,1},{x127V1,0,21},{x127V2,0,585},{x127V4,0,291},{x127V5,0,287},{x127V6,0,589},{x127V7,0,110},{x127V9,0,287},{x127V10,0,112},{x128V1,0,19},{x128V4,0,5},{x129V2,0,385},{x129V5,0,15},{x129V7,0,35},{x129V9,0,191},{x129V10,0,72},{x130V1,0,199},{x130V10,0,4},{x131V1,0,273},{x131V2,0,271},{x131V3,0,132},{x131V4,0,136},{x131V5,0,49},{x131V7,0,19},{x132V1,0,11},{x132V3,0,339},{x132V4,0,337},{x132V5,0,9},{x132V6,0,162},{x132V7,0,337},{x132V9,0,60},{x133V1,0,87},{x133V2,0,465},{x133V4,0,91},{x133V5,0,85},{x133V8,0,89},{x133V10,0,465},{x134V1,0,80},{x134V2,0,80},{x134V3,0,11},{x134V5,0,86},{x134V7,0,80},{x134V8,0,13},{x134V9,0,1},{x134V10,0,1},{x135V1,0,1},{x135V3,0,35},{x135V6,0,1},{x135V7,0,13},{x136V2,0,35},{x136V3,0,35},{x136V5,0,192},{x136V6,0,188},{x136V7,0,192},{x137V2,0,19},{x137V3,0,93},{x137V4,0,41},{x137V8,0,231},{x138V2,0,23},{x138V3,0,6},{x138V4,0,27},{x138V5,0,57},{x138V6,0,23},{x138V7,0,292},{x138V8,0,6},{x139V1,0,312},{x139V3,0,318},{x139V4,0,8},{x139V5,0,30},{x139V6,0,56},{x139V8,0,10},{x139V9,0,14},{x140V1,0,1},{x140V3,0,1},{x140V4,0,1},{x140V5,0,1},{x140V7,0,1},{x140V8,0,1},{x140V9,0,1},{x141V2,0,153},{x141V4,0,155},{x141V5,0,61},{x141V6,0,23},{x141V7,0,61},{x141V8,0,9},{x141V10,0,151},{x142V1,0,21},{x142V2,0,48},{x142V3,0,264},{x142V4,0,264},{x142V6,0,48},{x142V8,0,104},{x142V9,0,538},{x143V2,0,427},{x143V6,0,77},{x143V8,0,427},{x143V9,0,40},{x144V1,0,32},{x144V7,0,14},{x145V2,0,497},{x145V5,0,499},{x145V6,0,48},{x145V7,0,95},{x145V8,0,44},{x145V10,0,501},{x146V3,0,13},{x146V5,0,58},{x147V1,0,229},{x147V4,0,231},{x147V5,0,233},{x147V6,0,227},{x147V8,0,109},{x147V10,0,21},{x148V2,0,47},{x148V4,0,43},{x148V5,0,124},{x148V7,0,120},{x148V8,0,252},{x149V4,0,12},{x149V6,0,16},{x150V4,0,5},{x150V5,0,9},{x150V6,0,7},{x150V7,0,214},{x150V8,0,14},{x151V1,0,7},{x151V2,0,22},{x151V5,0,125},{x151V6,0,46},{x151V7,0,258},{x151V9,0,24},{x151V10,0,50},{x152V2,0,487},{x152V5,0,18},{x152V7,0,45},{x152V10,0,94},{x153V6,0,1},{x153V7,0,22},{x153V9,0,44},{x154V2,0,20},{x154V4,0,20},{x154V5,0,97},{x154V6,0,20},{x154V7,0,20},{x154V9,0,255},{x154V10,0,521},{x155V3,0,17},{x155V5,0,232},{x155V7,0,38},{x155V8,0,109},{x155V10,0,19},{x156V1,0,91},{x156V3,0,97},{x156V4,0,31},{x156V6,0,197},{x156V7,0,35},{x156V9,0,95},{x156V10,0,195},{x157V1,0,1},{x157V3,0,6},{x157V4,0,8},{x157V5,0,14},{x157V6,0,1},{x157V9,0,43},{x158V1,0,113},{x158V5,0,39},{x158V6,0,17},{x158V10,0,231},{x159V7,0,95},{x159V10,0,44},{x160V2,0,296},{x160V3,0,54},{x160V4,0,9},{x160V8,0,7},{x160V9,0,24},{x161V3,0,69},{x161V5,0,10},{x161V9,0,370},{x162V1,0,15},{x162V2,0,227},{x162V3,0,38},{x162V4,0,225},{x162V6,0,227},{x162V7,0,464},{x162V8,0,38},{x162V9,0,85},{x162V10,0,19},{x163V1,0,3},{x163V4,0,11},{x163V6,0,13},{x163V7,0,31},{x163V8,0,3},{x163V10,0,1},{x164V1,0,63},{x164V3,0,332},{x164V4,0,61},{x164V5,0,59},{x164V6,0,160},{x164V9,0,164},{x164V10,0,332},{x165V2,0,146},{x165V3,0,146},{x165V6,0,306},{x165V7,0,57},{x165V8,0,24},{x166V1,0,104},{x166V3,0,23},{x166V4,0,21},{x166V6,0,543},{x167V6,0,73},{x167V9,0,5},{x168V2,0,9},{x168V3,0,1},{x168V5,0,5},{x168V6,0,128},{x168V9,0,20},{x168V10,0,7},{x169V2,0,2},{x169V4,0,4},{x169V7,0,2},{x169V8,0,6},{x170V2,0,58},{x170V4,0,54},{x170V9,0,121},{x171V3,0,6},{x171V5,0,25},{x171V6,0,3},{x171V7,0,25},{x172V1,0,52},{x172V2,0,19},{x172V4,0,48},{x172V8,0,104},{x172V9,0,1},{x173V2,0,44},{x173V3,0,40},{x173V4,0,455},{x173V6,0,459},{x173V10,0,44},{x174V1,0,1},{x174V2,0,26},{x174V3,0,13},{x174V4,0,2},{x174V6,0,4},{x174V7,0,4},{x174V8,0,11},{x174V10,0,1},{x175V2,0,276},{x175V3,0,51},{x175V4,0,274},{x175V5,0,49},{x175V6,0,11},{x175V10,0,272},{x176V3,0,89},{x176V6,0,32},{x177V2,0,1},{x177V7,0,30},{x178V1,0,252},{x178V2,0,248},{x178V3,0,122},{x178V4,0,254},{x178V5,0,21},{x178V7,0,248},{x178V10,0,4},{x179V6,0,268},{x179V7,0,127},{x179V9,0,264},{x180V4,0,83},{x180V6,0,32},{x180V7,0,28},{x180V8,0,176},{x180V9,0,180},{x181V1,0,1},{x181V5,0,27},{x181V8,0,1},{x181V10,0,2},{x182V1,0,56},{x182V2,0,579},{x182V4,0,52},{x182V8,0,21},{x182V9,0,23},{x182V10,0,579},{x183V1,0,9},{x183V2,0,11},{x183V4,0,5},{x183V8,0,133},{x183V9,0,25},{x184V1,0,114},{x184V2,0,25},{x184V4,0,108},{x184V5,0,56},{x184V6,0,578},{x184V9,0,54},{x184V10,0,574},{x185V1,0,17},{x185V2,0,386},{x186V1,0,1},{x186V2,0,1},{x186V3,0,1},{x186V4,0,1},{x186V8,0,1},{x186V9,0,1},{x186V10,0,1},{x187V3,0,20},{x187V6,0,279},{x187V7,0,558},{x187V9,0,26},{x187V10,0,110},{x188V4,0,8},{x188V5,0,143},{x188V6,0,57},{x188V7,0,147},{x188V8,0,21},{x188V9,0,143},{x188V10,0,51},{x189V1,0,40},{x189V2,0,230},{x189V4,0,17},{x189V6,0,461},{x189V10,0,42},{x190V1,0,184},{x190V2,0,364},{x190V3,0,366},{x190V4,0,370},{x190V6,0,29},{x190V8,0,180},{x191V1,0,46},{x191V2,0,48},{x191V4,0,96},{x191V8,0,259},{x191V9,0,514},{x192V1,0,84},{x192V3,0,1},{x192V4,0,179},{x192V5,0,177},{x192V6,0,14},{x192V7,0,31},{x192V9,0,90},{x193V1,0,111},{x193V5,0,286},{x193V6,0,25},{x193V7,0,290},{x193V9,0,580},{x193V10,0,27},{x194V1,0,104},{x194V3,0,275},{x194V7,0,48},{x194V10,0,20},{x195V2,0,256},{x195V4,0,24},{x195V5,0,22},{x195V8,0,11},{x195V9,0,18},{x195V10,0,128},{x196V2,0,6},{x196V3,0,5},{x196V5,0,24},{x196V7,0,1},{x196V10,0,63},{x197V7,0,1},{x197V10,0,2},{x198V1,0,92},{x198V2,0,181},{x198V5,0,185},{x198V6,0,7},{x198V9,0,3},{x198V10,0,181},{x199V1,0,11},{x199V4,0,15},{x199V5,0,32},{x199V7,0,184},{x199V8,0,30},{x199V9,0,184},{x199V10,0,186},{x200V2,0,14},{x200V3,0,74},{x200V4,0,29},{x200V5,0,6},{x200V6,0,163},{x200V10,0,25}}
nonNegativeCond
x1V5>=0&&x1V6>=0&&x1V7>=0&&x2V2>=0&&x2V5>=0&&x2V6>=0&&x2V7>=0&&x2V9>=0&&x2V10>=0&&x3V1>=0&&x3V3>=0&&x4V1>=0&&x4V2>=0&&x4V4>=0&&x4V8>=0&&x4V9>=0&&x4V10>=0&&x5V1>=0&&x5V2>=0&&x5V4>=0&&x5V6>=0&&x5V7>=0&&x5V8>=0&&x5V9>=0&&x6V1>=0&&x6V4>=0&&x6V8>=0&&x6V10>=0&&x7V2>=0&&x7V4>=0&&x7V6>=0&&x7V10>=0&&x8V3>=0&&x8V7>=0&&x8V8>=0&&x8V10>=0&&x9V1>=0&&x9V2>=0&&x9V4>=0&&x9V6>=0&&x9V8>=0&&x9V9>=0&&x9V10>=0&&x10V5>=0&&x10V7>=0&&x10V8>=0&&x10V10>=0&&x11V1>=0&&x11V2>=0&&x11V3>=0&&x11V4>=0&&x11V9>=0&&x11V10>=0&&x12V1>=0&&x12V3>=0&&x12V4>=0&&x12V5>=0&&x12V8>=0&&x12V9>=0&&x12V10>=0&&x13V1>=0&&x13V3>=0&&x13V4>=0&&x13V7>=0&&x13V10>=0&&x14V1>=0&&x14V2>=0&&x14V3>=0&&x14V4>=0&&x14V5>=0&&x14V6>=0&&x14V7>=0&&x14V10>=0&&x15V1>=0&&x15V2>=0&&x15V5>=0&&x16V1>=0&&x16V3>=0&&x17V2>=0&&x17V3>=0&&x17V4>=0&&x17V5>=0&&x17V6>=0&&x17V8>=0&&x17V9>=0&&x17V10>=0&&x18V1>=0&&x18V2>=0&&x18V3>=0&&x18V4>=0&&x18V5>=0&&x18V6>=0&&x18V7>=0&&x18V10>=0&&x19V3>=0&&x19V5>=0&&x19V8>=0&&x19V9>=0&&x19V10>=0&&x20V1>=0&&x20V2>=0&&x20V3>=0&&x20V6>=0&&x20V8>=0&&x20V10>=0&&x21V3>=0&&x21V5>=0&&x21V6>=0&&x21V7>=0&&x22V1>=0&&x22V3>=0&&x22V4>=0&&x22V6>=0&&x22V10>=0&&x23V4>=0&&x23V5>=0&&x24V1>=0&&x24V3>=0&&x24V4>=0&&x24V5>=0&&x24V7>=0&&x24V9>=0&&x24V10>=0&&x25V2>=0&&x25V4>=0&&x25V7>=0&&x25V9>=0&&x26V1>=0&&x26V6>=0&&x26V7>=0&&x26V9>=0&&x27V2>=0&&x27V4>=0&&x27V5>=0&&x28V1>=0&&x28V6>=0&&x28V7>=0&&x28V8>=0&&x28V9>=0&&x29V1>=0&&x29V2>=0&&x29V3>=0&&x29V4>=0&&x29V5>=0&&x29V8>=0&&x29V9>=0&&x30V1>=0&&x30V4>=0&&x31V1>=0&&x31V3>=0&&x31V5>=0&&x31V6>=0&&x31V7>=0&&x31V10>=0&&x32V1>=0&&x32V3>=0&&x32V8>=0&&x32V9>=0&&x32V10>=0&&x33V7>=0&&x33V10>=0&&x34V4>=0&&x34V6>=0&&x34V7>=0&&x34V8>=0&&x34V10>=0&&x35V3>=0&&x35V4>=0&&x35V8>=0&&x36V1>=0&&x36V2>=0&&x36V3>=0&&x36V4>=0&&x36V6>=0&&x36V9>=0&&x37V2>=0&&x37V3>=0&&x37V9>=0&&x38V8>=0&&x38V9>=0&&x39V2>=0&&x39V4>=0&&x39V8>=0&&x39V9>=0&&x40V1>=0&&x40V9>=0&&x41V1>=0&&x41V3>=0&&x41V5>=0&&x41V6>=0&&x41V7>=0&&x41V8>=0&&x41V9>=0&&x41V10>=0&&x42V1>=0&&x42V3>=0&&x42V4>=0&&x42V6>=0&&x42V7>=0&&x42V9>=0&&x42V10>=0&&x43V1>=0&&x43V3>=0&&x43V4>=0&&x43V5>=0&&x43V8>=0&&x43V10>=0&&x44V2>=0&&x44V4>=0&&x44V6>=0&&x44V7>=0&&x44V10>=0&&x45V1>=0&&x45V3>=0&&x45V8>=0&&x45V10>=0&&x46V1>=0&&x46V2>=0&&x46V3>=0&&x46V4>=0&&x46V5>=0&&x46V6>=0&&x47V1>=0&&x47V3>=0&&x47V5>=0&&x47V6>=0&&x47V8>=0&&x47V10>=0&&x48V2>=0&&x48V3>=0&&x48V4>=0&&x48V6>=0&&x48V7>=0&&x48V8>=0&&x49V2>=0&&x49V3>=0&&x49V4>=0&&x49V5>=0&&x49V9>=0&&x50V1>=0&&x50V3>=0&&x50V6>=0&&x50V8>=0&&x50V9>=0&&x51V1>=0&&x51V4>=0&&x51V6>=0&&x51V8>=0&&x51V9>=0&&x52V3>=0&&x52V8>=0&&x53V1>=0&&x53V2>=0&&x53V7>=0&&x53V8>=0&&x53V9>=0&&x53V10>=0&&x54V4>=0&&x54V10>=0&&x55V1>=0&&x55V2>=0&&x55V4>=0&&x55V6>=0&&x55V7>=0&&x55V8>=0&&x55V9>=0&&x55V10>=0&&x56V2>=0&&x56V3>=0&&x56V8>=0&&x57V1>=0&&x57V4>=0&&x57V5>=0&&x57V6>=0&&x57V8>=0&&x57V9>=0&&x57V10>=0&&x58V1>=0&&x58V2>=0&&x58V3>=0&&x58V4>=0&&x58V5>=0&&x58V6>=0&&x58V7>=0&&x58V8>=0&&x58V9>=0&&x58V10>=0&&x59V5>=0&&x59V7>=0&&x60V2>=0&&x60V4>=0&&x60V7>=0&&x60V9>=0&&x60V10>=0&&x61V2>=0&&x61V5>=0&&x61V7>=0&&x61V9>=0&&x62V2>=0&&x62V7>=0&&x63V3>=0&&x63V4>=0&&x63V5>=0&&x63V6>=0&&x63V7>=0&&x63V8>=0&&x63V9>=0&&x64V2>=0&&x64V4>=0&&x64V8>=0&&x65V1>=0&&x65V3>=0&&x65V4>=0&&x65V9>=0&&x65V10>=0&&x66V2>=0&&x66V3>=0&&x66V4>=0&&x66V5>=0&&x66V6>=0&&x66V8>=0&&x66V9>=0&&x67V4>=0&&x67V8>=0&&x68V1>=0&&x68V4>=0&&x68V5>=0&&x68V7>=0&&x68V8>=0&&x68V9>=0&&x69V1>=0&&x69V2>=0&&x69V4>=0&&x69V5>=0&&x69V7>=0&&x69V8>=0&&x69V9>=0&&x70V2>=0&&x70V3>=0&&x70V4>=0&&x70V6>=0&&x70V7>=0&&x70V10>=0&&x71V1>=0&&x71V3>=0&&x71V7>=0&&x71V8>=0&&x71V9>=0&&x72V1>=0&&x72V7>=0&&x73V1>=0&&x73V2>=0&&x73V6>=0&&x73V8>=0&&x73V9>=0&&x74V2>=0&&x74V3>=0&&x74V4>=0&&x74V6>=0&&x75V1>=0&&x75V4>=0&&x75V5>=0&&x75V6>=0&&x75V7>=0&&x75V9>=0&&x75V10>=0&&x76V2>=0&&x76V3>=0&&x76V4>=0&&x76V7>=0&&x76V8>=0&&x76V9>=0&&x76V10>=0&&x77V3>=0&&x77V4>=0&&x77V6>=0&&x77V10>=0&&x78V1>=0&&x78V2>=0&&x78V3>=0&&x78V4>=0&&x78V5>=0&&x78V6>=0&&x79V2>=0&&x79V6>=0&&x79V7>=0&&x80V1>=0&&x80V5>=0&&x80V9>=0&&x81V1>=0&&x81V3>=0&&x81V8>=0&&x81V9>=0&&x82V1>=0&&x82V2>=0&&x82V4>=0&&x82V6>=0&&x82V7>=0&&x82V9>=0&&x82V10>=0&&x83V1>=0&&x83V2>=0&&x83V9>=0&&x84V2>=0&&x84V4>=0&&x84V5>=0&&x84V6>=0&&x84V7>=0&&x84V8>=0&&x84V10>=0&&x85V1>=0&&x85V3>=0&&x85V4>=0&&x85V6>=0&&x85V7>=0&&x85V8>=0&&x85V9>=0&&x86V1>=0&&x86V2>=0&&x86V4>=0&&x86V6>=0&&x86V7>=0&&x86V8>=0&&x86V9>=0&&x86V10>=0&&x87V1>=0&&x87V6>=0&&x87V7>=0&&x87V8>=0&&x87V9>=0&&x88V1>=0&&x88V2>=0&&x88V4>=0&&x88V5>=0&&x88V6>=0&&x88V7>=0&&x88V8>=0&&x88V9>=0&&x88V10>=0&&x89V2>=0&&x89V6>=0&&x89V8>=0&&x90V1>=0&&x90V3>=0&&x90V4>=0&&x90V5>=0&&x90V7>=0&&x90V8>=0&&x90V9>=0&&x91V6>=0&&x91V8>=0&&x92V2>=0&&x92V4>=0&&x92V7>=0&&x92V10>=0&&x93V1>=0&&x93V2>=0&&x93V4>=0&&x93V5>=0&&x93V7>=0&&x93V8>=0&&x94V2>=0&&x94V3>=0&&x94V5>=0&&x94V7>=0&&x94V8>=0&&x94V9>=0&&x94V10>=0&&x95V1>=0&&x95V3>=0&&x95V5>=0&&x95V8>=0&&x95V9>=0&&x95V10>=0&&x96V3>=0&&x96V4>=0&&x96V5>=0&&x96V6>=0&&x96V7>=0&&x96V9>=0&&x96V10>=0&&x97V1>=0&&x97V2>=0&&x97V3>=0&&x97V6>=0&&x97V9>=0&&x98V1>=0&&x98V2>=0&&x98V3>=0&&x98V5>=0&&x98V6>=0&&x98V7>=0&&x98V9>=0&&x98V10>=0&&x99V3>=0&&x99V4>=0&&x99V9>=0&&x100V1>=0&&x100V3>=0&&x100V4>=0&&x100V6>=0&&x100V8>=0&&x100V9>=0&&x100V10>=0&&x101V2>=0&&x101V3>=0&&x101V5>=0&&x101V6>=0&&x101V7>=0&&x101V9>=0&&x102V1>=0&&x102V3>=0&&x102V4>=0&&x102V7>=0&&x102V9>=0&&x103V1>=0&&x103V2>=0&&x103V3>=0&&x103V4>=0&&x103V6>=0&&x103V9>=0&&x103V10>=0&&x104V6>=0&&x104V9>=0&&x105V6>=0&&x105V7>=0&&x106V3>=0&&x106V4>=0&&x106V6>=0&&x106V8>=0&&x106V9>=0&&x106V10>=0&&x107V1>=0&&x107V2>=0&&x107V5>=0&&x107V7>=0&&x107V9>=0&&x108V2>=0&&x108V5>=0&&x109V1>=0&&x109V4>=0&&x109V6>=0&&x109V7>=0&&x109V8>=0&&x110V7>=0&&x110V10>=0&&x111V1>=0&&x111V2>=0&&x111V3>=0&&x111V4>=0&&x111V7>=0&&x111V8>=0&&x111V9>=0&&x112V3>=0&&x112V4>=0&&x112V5>=0&&x112V6>=0&&x112V7>=0&&x112V9>=0&&x113V1>=0&&x113V4>=0&&x113V5>=0&&x113V6>=0&&x113V7>=0&&x113V9>=0&&x113V10>=0&&x114V1>=0&&x114V2>=0&&x114V3>=0&&x114V4>=0&&x114V6>=0&&x114V7>=0&&x114V8>=0&&x114V9>=0&&x114V10>=0&&x115V1>=0&&x115V4>=0&&x115V7>=0&&x115V9>=0&&x116V1>=0&&x116V2>=0&&x116V4>=0&&x116V5>=0&&x116V6>=0&&x116V8>=0&&x116V9>=0&&x116V10>=0&&x117V1>=0&&x117V3>=0&&x117V4>=0&&x117V6>=0&&x117V7>=0&&x117V9>=0&&x117V10>=0&&x118V1>=0&&x118V7>=0&&x118V8>=0&&x118V9>=0&&x118V10>=0&&x119V1>=0&&x119V9>=0&&x120V4>=0&&x120V7>=0&&x120V9>=0&&x121V5>=0&&x121V6>=0&&x122V5>=0&&x122V7>=0&&x122V8>=0&&x122V9>=0&&x122V10>=0&&x123V1>=0&&x123V2>=0&&x123V4>=0&&x123V6>=0&&x123V9>=0&&x123V10>=0&&x124V1>=0&&x124V5>=0&&x124V7>=0&&x125V2>=0&&x125V4>=0&&x125V5>=0&&x125V6>=0&&x125V7>=0&&x125V9>=0&&x125V10>=0&&x126V1>=0&&x126V2>=0&&x126V4>=0&&x126V6>=0&&x126V8>=0&&x126V9>=0&&x127V1>=0&&x127V2>=0&&x127V4>=0&&x127V5>=0&&x127V6>=0&&x127V7>=0&&x127V9>=0&&x127V10>=0&&x128V1>=0&&x128V4>=0&&x129V2>=0&&x129V5>=0&&x129V7>=0&&x129V9>=0&&x129V10>=0&&x130V1>=0&&x130V10>=0&&x131V1>=0&&x131V2>=0&&x131V3>=0&&x131V4>=0&&x131V5>=0&&x131V7>=0&&x132V1>=0&&x132V3>=0&&x132V4>=0&&x132V5>=0&&x132V6>=0&&x132V7>=0&&x132V9>=0&&x133V1>=0&&x133V2>=0&&x133V4>=0&&x133V5>=0&&x133V8>=0&&x133V10>=0&&x134V1>=0&&x134V2>=0&&x134V3>=0&&x134V5>=0&&x134V7>=0&&x134V8>=0&&x134V9>=0&&x134V10>=0&&x135V1>=0&&x135V3>=0&&x135V6>=0&&x135V7>=0&&x136V2>=0&&x136V3>=0&&x136V5>=0&&x136V6>=0&&x136V7>=0&&x137V2>=0&&x137V3>=0&&x137V4>=0&&x137V8>=0&&x138V2>=0&&x138V3>=0&&x138V4>=0&&x138V5>=0&&x138V6>=0&&x138V7>=0&&x138V8>=0&&x139V1>=0&&x139V3>=0&&x139V4>=0&&x139V5>=0&&x139V6>=0&&x139V8>=0&&x139V9>=0&&x140V1>=0&&x140V3>=0&&x140V4>=0&&x140V5>=0&&x140V7>=0&&x140V8>=0&&x140V9>=0&&x141V2>=0&&x141V4>=0&&x141V5>=0&&x141V6>=0&&x141V7>=0&&x141V8>=0&&x141V10>=0&&x142V1>=0&&x142V2>=0&&x142V3>=0&&x142V4>=0&&x142V6>=0&&x142V8>=0&&x142V9>=0&&x143V2>=0&&x143V6>=0&&x143V8>=0&&x143V9>=0&&x144V1>=0&&x144V7>=0&&x145V2>=0&&x145V5>=0&&x145V6>=0&&x145V7>=0&&x145V8>=0&&x145V10>=0&&x146V3>=0&&x146V5>=0&&x147V1>=0&&x147V4>=0&&x147V5>=0&&x147V6>=0&&x147V8>=0&&x147V10>=0&&x148V2>=0&&x148V4>=0&&x148V5>=0&&x148V7>=0&&x148V8>=0&&x149V4>=0&&x149V6>=0&&x150V4>=0&&x150V5>=0&&x150V6>=0&&x150V7>=0&&x150V8>=0&&x151V1>=0&&x151V2>=0&&x151V5>=0&&x151V6>=0&&x151V7>=0&&x151V9>=0&&x151V10>=0&&x152V2>=0&&x152V5>=0&&x152V7>=0&&x152V10>=0&&x153V6>=0&&x153V7>=0&&x153V9>=0&&x154V2>=0&&x154V4>=0&&x154V5>=0&&x154V6>=0&&x154V7>=0&&x154V9>=0&&x154V10>=0&&x155V3>=0&&x155V5>=0&&x155V7>=0&&x155V8>=0&&x155V10>=0&&x156V1>=0&&x156V3>=0&&x156V4>=0&&x156V6>=0&&x156V7>=0&&x156V9>=0&&x156V10>=0&&x157V1>=0&&x157V3>=0&&x157V4>=0&&x157V5>=0&&x157V6>=0&&x157V9>=0&&x158V1>=0&&x158V5>=0&&x158V6>=0&&x158V10>=0&&x159V7>=0&&x159V10>=0&&x160V2>=0&&x160V3>=0&&x160V4>=0&&x160V8>=0&&x160V9>=0&&x161V3>=0&&x161V5>=0&&x161V9>=0&&x162V1>=0&&x162V2>=0&&x162V3>=0&&x162V4>=0&&x162V6>=0&&x162V7>=0&&x162V8>=0&&x162V9>=0&&x162V10>=0&&x163V1>=0&&x163V4>=0&&x163V6>=0&&x163V7>=0&&x163V8>=0&&x163V10>=0&&x164V1>=0&&x164V3>=0&&x164V4>=0&&x164V5>=0&&x164V6>=0&&x164V9>=0&&x164V10>=0&&x165V2>=0&&x165V3>=0&&x165V6>=0&&x165V7>=0&&x165V8>=0&&x166V1>=0&&x166V3>=0&&x166V4>=0&&x166V6>=0&&x167V6>=0&&x167V9>=0&&x168V2>=0&&x168V3>=0&&x168V5>=0&&x168V6>=0&&x168V9>=0&&x168V10>=0&&x169V2>=0&&x169V4>=0&&x169V7>=0&&x169V8>=0&&x170V2>=0&&x170V4>=0&&x170V9>=0&&x171V3>=0&&x171V5>=0&&x171V6>=0&&x171V7>=0&&x172V1>=0&&x172V2>=0&&x172V4>=0&&x172V8>=0&&x172V9>=0&&x173V2>=0&&x173V3>=0&&x173V4>=0&&x173V6>=0&&x173V10>=0&&x174V1>=0&&x174V2>=0&&x174V3>=0&&x174V4>=0&&x174V6>=0&&x174V7>=0&&x174V8>=0&&x174V10>=0&&x175V2>=0&&x175V3>=0&&x175V4>=0&&x175V5>=0&&x175V6>=0&&x175V10>=0&&x176V3>=0&&x176V6>=0&&x177V2>=0&&x177V7>=0&&x178V1>=0&&x178V2>=0&&x178V3>=0&&x178V4>=0&&x178V5>=0&&x178V7>=0&&x178V10>=0&&x179V6>=0&&x179V7>=0&&x179V9>=0&&x180V4>=0&&x180V6>=0&&x180V7>=0&&x180V8>=0&&x180V9>=0&&x181V1>=0&&x181V5>=0&&x181V8>=0&&x181V10>=0&&x182V1>=0&&x182V2>=0&&x182V4>=0&&x182V8>=0&&x182V9>=0&&x182V10>=0&&x183V1>=0&&x183V2>=0&&x183V4>=0&&x183V8>=0&&x183V9>=0&&x184V1>=0&&x184V2>=0&&x184V4>=0&&x184V5>=0&&x184V6>=0&&x184V9>=0&&x184V10>=0&&x185V1>=0&&x185V2>=0&&x186V1>=0&&x186V2>=0&&x186V3>=0&&x186V4>=0&&x186V8>=0&&x186V9>=0&&x186V10>=0&&x187V3>=0&&x187V6>=0&&x187V7>=0&&x187V9>=0&&x187V10>=0&&x188V4>=0&&x188V5>=0&&x188V6>=0&&x188V7>=0&&x188V8>=0&&x188V9>=0&&x188V10>=0&&x189V1>=0&&x189V2>=0&&x189V4>=0&&x189V6>=0&&x189V10>=0&&x190V1>=0&&x190V2>=0&&x190V3>=0&&x190V4>=0&&x190V6>=0&&x190V8>=0&&x191V1>=0&&x191V2>=0&&x191V4>=0&&x191V8>=0&&x191V9>=0&&x192V1>=0&&x192V3>=0&&x192V4>=0&&x192V5>=0&&x192V6>=0&&x192V7>=0&&x192V9>=0&&x193V1>=0&&x193V5>=0&&x193V6>=0&&x193V7>=0&&x193V9>=0&&x193V10>=0&&x194V1>=0&&x194V3>=0&&x194V7>=0&&x194V10>=0&&x195V2>=0&&x195V4>=0&&x195V5>=0&&x195V8>=0&&x195V9>=0&&x195V10>=0&&x196V2>=0&&x196V3>=0&&x196V5>=0&&x196V7>=0&&x196V10>=0&&x197V7>=0&&x197V10>=0&&x198V1>=0&&x198V2>=0&&x198V5>=0&&x198V6>=0&&x198V9>=0&&x198V10>=0&&x199V1>=0&&x199V4>=0&&x199V5>=0&&x199V7>=0&&x199V8>=0&&x199V9>=0&&x199V10>=0&&x200V2>=0&&x200V3>=0&&x200V4>=0&&x200V5>=0&&x200V6>=0&&x200V10>=0
upperBoundCond
x1V5<=1&&x1V6<=36&&x1V7<=13&&x2V2<=3&&x2V5<=112&&x2V6<=114&&x2V7<=15&&x2V9<=228&&x2V10<=17&&x3V1<=128&&x3V3<=4&&x4V1<=25&&x4V2<=27&&x4V4<=331&&x4V8<=335&&x4V9<=333&&x4V10<=329&&x5V1<=22&&x5V2<=20&&x5V4<=259&&x5V6<=101&&x5V7<=101&&x5V8<=22&&x5V9<=257&&x6V1<=390&&x6V4<=71&&x6V8<=390&&x6V10<=35&&x7V2<=144&&x7V4<=56&&x7V6<=144&&x7V10<=150&&x8V3<=532&&x8V7<=104&&x8V8<=538&&x8V10<=532&&x9V1<=24&&x9V2<=18&&x9V4<=103&&x9V6<=105&&x9V8<=22&&x9V9<=51&&x9V10<=22&&x10V5<=320&&x10V7<=326&&x10V8<=63&&x10V10<=324&&x11V1<=112&&x11V2<=239&&x11V3<=20&&x11V4<=20&&x11V9<=46&&x11V10<=40&&x12V1<=19&&x12V3<=235&&x12V4<=229&&x12V5<=235&&x12V8<=231&&x12V9<=17&&x12V10<=472&&x13V1<=91&&x13V3<=17&&x13V4<=5&&x13V7<=95&&x13V10<=5&&x14V1<=202&&x14V2<=96&&x14V3<=204&&x14V4<=200&&x14V5<=14&&x14V6<=33&&x14V7<=33&&x14V10<=200&&x15V1<=553&&x15V2<=108&&x15V5<=104&&x16V1<=93&&x16V3<=91&&x17V2<=20&&x17V3<=437&&x17V4<=42&&x17V5<=80&&x17V6<=40&&x17V8<=437&&x17V9<=84&&x17V10<=20&&x18V1<=22&&x18V2<=485&&x18V3<=239&&x18V4<=93&&x18V5<=22&&x18V6<=91&&x18V7<=46&&x18V10<=481&&x19V3<=16&&x19V5<=14&&x19V8<=395&&x19V9<=12&&x19V10<=78&&x20V1<=9&&x20V2<=274&&x20V3<=53&&x20V6<=47&&x20V8<=11&&x20V10<=276&&x21V3<=1&&x21V5<=12&&x21V6<=63&&x21V7<=3&&x22V1<=106&&x22V3<=18&&x22V4<=1&&x22V6<=112&&x22V10<=106&&x23V4<=71&&x23V5<=35&&x24V1<=1&&x24V3<=13&&x24V4<=1&&x24V5<=6&&x24V7<=4&&x24V9<=6&&x24V10<=1&&x25V2<=1&&x25V4<=2&&x25V7<=9&&x25V9<=1&&x26V1<=221&&x26V6<=225&&x26V7<=16&&x26V9<=227&&x27V2<=14&&x27V4<=93&&x27V5<=12&&x28V1<=32&&x28V6<=199&&x28V7<=394&&x28V8<=34&&x28V9<=394&&x29V1<=35&&x29V2<=70&&x29V3<=29&&x29V4<=68&&x29V5<=14&&x29V8<=72&&x29V9<=14&&x30V1<=2&&x30V4<=41&&x31V1<=1&&x31V3<=137&&x31V5<=6&&x31V6<=6&&x31V7<=24&&x31V10<=3&&x32V1<=23&&x32V3<=17&&x32V8<=21&&x32V9<=44&&x32V10<=502&&x33V7<=324&&x33V10<=324&&x34V4<=278&&x34V6<=138&&x34V7<=136&&x34V8<=280&&x34V10<=278&&x35V3<=50&&x35V4<=253&&x35V8<=98&&x36V1<=9&&x36V2<=11&&x36V3<=15&&x36V4<=27&&x36V6<=171&&x36V9<=173&&x37V2<=569&&x37V3<=567&&x37V9<=281&&x38V8<=170&&x38V9<=29&&x39V2<=49&&x39V4<=45&&x39V8<=45&&x39V9<=123&&x40V1<=378&&x40V9<=11&&x41V1<=110&&x41V3<=54&&x41V5<=284&&x41V6<=23&&x41V7<=25&&x41V8<=288&&x41V9<=290&&x41V10<=582&&x42V1<=19&&x42V3<=47&&x42V4<=19&&x42V6<=21&&x42V7<=269&&x42V9<=104&&x42V10<=108&&x43V1<=34&&x43V3<=32&&x43V4<=3&&x43V5<=88&&x43V8<=16&&x43V10<=14&&x44V2<=47&&x44V4<=266&&x44V6<=262&&x44V7<=49&&x44V10<=45&&x45V1<=8&&x45V3<=119&&x45V8<=125&&x45V10<=19&&x46V1<=49&&x46V2<=20&&x46V3<=140&&x46V4<=49&&x46V5<=26&&x46V6<=10&&x47V1<=13&&x47V3<=422&&x47V5<=35&&x47V6<=426&&x47V8<=41&&x47V10<=41&&x48V2<=1&&x48V3<=1&&x48V4<=13&&x48V6<=1&&x48V7<=1&&x48V8<=1&&x49V2<=133&&x49V3<=7&&x49V4<=269&&x49V5<=23&&x49V9<=52&&x50V1<=22&&x50V3<=131&&x50V6<=5&&x50V8<=5&&x50V9<=60&&x51V1<=71&&x51V4<=1&&x51V6<=13&&x51V8<=11&&x51V9<=5&&x52V3<=517&&x52V8<=48&&x53V1<=46&&x53V2<=23&&x53V7<=25&&x53V8<=267&&x53V9<=104&&x53V10<=269&&x54V4<=51&&x54V10<=289&&x55V1<=33&&x55V2<=33&&x55V4<=68&&x55V6<=1&&x55V7<=1&&x55V8<=27&&x55V9<=29&&x55V10<=1&&x56V2<=522&&x56V3<=96&&x56V8<=18&&x57V1<=1&&x57V4<=1&&x57V5<=14&&x57V6<=43&&x57V8<=90&&x57V9<=1&&x57V10<=1&&x58V1<=71&&x58V2<=13&&x58V3<=31&&x58V4<=77&&x58V5<=33&&x58V6<=394&&x58V7<=190&&x58V8<=194&&x58V9<=190&&x58V10<=11&&x59V5<=586&&x59V7<=115&&x60V2<=109&&x60V4<=21&&x60V7<=583&&x60V9<=290&&x60V10<=579&&x61V2<=82&&x61V5<=16&&x61V7<=219&&x61V9<=82&&x62V2<=49&&x62V7<=100&&x63V3<=66&&x63V4<=32&&x63V5<=64&&x63V6<=28&&x63V7<=168&&x63V8<=339&&x63V9<=337&&x64V2<=67&&x64V4<=176&&x64V8<=357&&x65V1<=7&&x65V3<=9&&x65V4<=24&&x65V9<=5&&x65V10<=1&&x66V2<=5&&x66V3<=29&&x66V4<=183&&x66V5<=33&&x66V6<=179&&x66V8<=85&&x66V9<=10&&x67V4<=240&&x67V8<=22&&x68V1<=1&&x68V4<=1&&x68V5<=11&&x68V7<=1&&x68V8<=7&&x68V9<=1&&x69V1<=242&&x69V2<=121&&x69V4<=117&&x69V5<=42&&x69V7<=21&&x69V8<=46&&x69V9<=244&&x70V2<=546&&x70V3<=272&&x70V4<=272&&x70V6<=106&&x70V7<=544&&x70V10<=544&&x71V1<=496&&x71V3<=92&&x71V7<=17&&x71V8<=248&&x71V9<=494&&x72V1<=1&&x72V7<=1&&x73V1<=21&&x73V2<=266&&x73V6<=272&&x73V8<=131&&x73V9<=23&&x74V2<=375&&x74V3<=30&&x74V4<=184&&x74V6<=379&&x75V1<=1&&x75V4<=92&&x75V5<=3&&x75V6<=40&&x75V7<=3&&x75V9<=94&&x75V10<=94&&x76V2<=16&&x76V3<=78&&x76V4<=32&&x76V7<=394&&x76V8<=194&&x76V9<=12&&x76V10<=72&&x77V3<=1&&x77V4<=1&&x77V6<=1&&x77V10<=16&&x78V1<=409&&x78V2<=199&&x78V3<=199&&x78V4<=409&&x78V5<=203&&x78V6<=203&&x79V2<=210&&x79V6<=15&&x79V7<=83&&x80V1<=282&&x80V5<=22&&x80V9<=278&&x81V1<=200&&x81V3<=38&&x81V8<=36&&x81V9<=36&&x82V1<=29&&x82V2<=13&&x82V4<=59&&x82V6<=25&&x82V7<=157&&x82V9<=55&&x82V10<=153&&x83V1<=28&&x83V2<=296&&x83V9<=26&&x84V2<=229&&x84V4<=42&&x84V5<=44&&x84V6<=229&&x84V7<=229&&x84V8<=229&&x84V10<=40&&x85V1<=32&&x85V3<=32&&x85V4<=170&&x85V6<=11&&x85V7<=340&&x85V8<=66&&x85V9<=15&&x86V1<=248&&x86V2<=21&&x86V4<=120&&x86V6<=10&&x86V7<=120&&x86V8<=126&&x86V9<=252&&x86V10<=49&&x87V1<=11&&x87V6<=152&&x87V7<=25&&x87V8<=70&&x87V9<=25&&x88V1<=64&&x88V2<=20&&x88V4<=26&&x88V5<=142&&x88V6<=22&&x88V7<=22&&x88V8<=5&&x88V9<=26&&x88V10<=3&&x89V2<=5&&x89V6<=9&&x89V8<=17&&x90V1<=226&&x90V3<=88&&x90V4<=222&&x90V5<=88&&x90V7<=38&&x90V8<=84&&x90V9<=21&&x91V6<=14&&x91V8<=194&&x92V2<=1&&x92V4<=81&&x92V7<=8&&x92V10<=10&&x93V1<=1&&x93V2<=1&&x93V4<=35&&x93V5<=1&&x93V7<=17&&x93V8<=37&&x94V2<=110&&x94V3<=37&&x94V5<=39&&x94V7<=14&&x94V8<=7&&x94V9<=16&&x94V10<=224&&x95V1<=28&&x95V3<=13&&x95V5<=6&&x95V8<=13&&x95V9<=28&&x95V10<=170&&x96V3<=31&&x96V4<=33&&x96V5<=176&&x96V6<=349&&x96V7<=63&&x96V9<=355&&x96V10<=65&&x97V1<=419&&x97V2<=415&&x97V3<=78&&x97V6<=15&&x97V9<=82&&x98V1<=1&&x98V2<=1&&x98V3<=20&&x98V5<=22&&x98V6<=44&&x98V7<=5&&x98V9<=40&&x98V10<=1&&x99V3<=9&&x99V4<=17&&x99V9<=1&&x100V1<=118&&x100V3<=2&&x100V4<=55&&x100V6<=116&&x100V8<=1&&x100V9<=120&&x100V10<=1&&x101V2<=65&&x101V3<=4&&x101V5<=2&&x101V6<=128&&x101V7<=63&&x101V9<=59&&x102V1<=309&&x102V3<=59&&x102V4<=149&&x102V7<=311&&x102V9<=149&&x103V1<=406&&x103V2<=202&&x103V3<=16&&x103V4<=16&&x103V6<=39&&x103V9<=35&&x103V10<=406&&x104V6<=41&&x104V9<=45&&x105V6<=384&&x105V7<=15&&x106V3<=585&&x106V4<=287&&x106V6<=23&&x106V8<=287&&x106V9<=291&&x106V10<=116&&x107V1<=4&&x107V2<=60&&x107V5<=125&&x107V7<=4&&x107V9<=121&&x108V2<=13&&x108V5<=194&&x109V1<=112&&x109V4<=112&&x109V6<=8&&x109V7<=42&&x109V8<=234&&x110V7<=98&&x110V10<=22&&x111V1<=275&&x111V2<=565&&x111V3<=277&&x111V4<=24&&x111V7<=48&&x111V8<=107&&x111V9<=107&&x112V3<=166&&x112V4<=332&&x112V5<=66&&x112V6<=11&&x112V7<=338&&x112V9<=60&&x113V1<=55&&x113V4<=287&&x113V5<=283&&x113V6<=10&&x113V7<=22&&x113V9<=8&&x113V10<=6&&x114V1<=231&&x114V2<=17&&x114V3<=19&&x114V4<=235&&x114V6<=471&&x114V7<=87&&x114V8<=21&&x114V9<=233&&x114V10<=39&&x115V1<=35&&x115V4<=11&&x115V7<=11&&x115V9<=192&&x116V1<=459&&x116V2<=459&&x116V4<=228&&x116V5<=230&&x116V6<=226&&x116V8<=228&&x116V9<=230&&x116V10<=44&&x117V1<=10&&x117V3<=69&&x117V4<=14&&x117V6<=16&&x117V7<=31&&x117V9<=14&&x117V10<=73&&x118V1<=97&&x118V7<=495&&x118V8<=41&&x118V9<=493&&x118V10<=245&&x119V1<=1&&x119V9<=16&&x120V4<=23&&x120V7<=21&&x120V9<=274&&x121V5<=108&&x121V6<=110&&x122V5<=123&&x122V7<=245&&x122V8<=10&&x122V9<=117&&x122V10<=19&&x123V1<=31&&x123V2<=37&&x123V4<=35&&x123V6<=1&&x123V9<=35&&x123V10<=1&&x124V1<=10&&x124V5<=10&&x124V7<=12&&x125V2<=487&&x125V4<=483&&x125V5<=20&&x125V6<=485&&x125V7<=243&&x125V9<=92&&x125V10<=241&&x126V1<=1&&x126V2<=1&&x126V4<=2&&x126V6<=56&&x126V8<=1&&x126V9<=1&&x127V1<=21&&x127V2<=585&&x127V4<=291&&x127V5<=287&&x127V6<=589&&x127V7<=110&&x127V9<=287&&x127V10<=112&&x128V1<=19&&x128V4<=5&&x129V2<=385&&x129V5<=15&&x129V7<=35&&x129V9<=191&&x129V10<=72&&x130V1<=199&&x130V10<=4&&x131V1<=273&&x131V2<=271&&x131V3<=132&&x131V4<=136&&x131V5<=49&&x131V7<=19&&x132V1<=11&&x132V3<=339&&x132V4<=337&&x132V5<=9&&x132V6<=162&&x132V7<=337&&x132V9<=60&&x133V1<=87&&x133V2<=465&&x133V4<=91&&x133V5<=85&&x133V8<=89&&x133V10<=465&&x134V1<=80&&x134V2<=80&&x134V3<=11&&x134V5<=86&&x134V7<=80&&x134V8<=13&&x134V9<=1&&x134V10<=1&&x135V1<=1&&x135V3<=35&&x135V6<=1&&x135V7<=13&&x136V2<=35&&x136V3<=35&&x136V5<=192&&x136V6<=188&&x136V7<=192&&x137V2<=19&&x137V3<=93&&x137V4<=41&&x137V8<=231&&x138V2<=23&&x138V3<=6&&x138V4<=27&&x138V5<=57&&x138V6<=23&&x138V7<=292&&x138V8<=6&&x139V1<=312&&x139V3<=318&&x139V4<=8&&x139V5<=30&&x139V6<=56&&x139V8<=10&&x139V9<=14&&x140V1<=1&&x140V3<=1&&x140V4<=1&&x140V5<=1&&x140V7<=1&&x140V8<=1&&x140V9<=1&&x141V2<=153&&x141V4<=155&&x141V5<=61&&x141V6<=23&&x141V7<=61&&x141V8<=9&&x141V10<=151&&x142V1<=21&&x142V2<=48&&x142V3<=264&&x142V4<=264&&x142V6<=48&&x142V8<=104&&x142V9<=538&&x143V2<=427&&x143V6<=77&&x143V8<=427&&x143V9<=40&&x144V1<=32&&x144V7<=14&&x145V2<=497&&x145V5<=499&&x145V6<=48&&x145V7<=95&&x145V8<=44&&x145V10<=501&&x146V3<=13&&x146V5<=58&&x147V1<=229&&x147V4<=231&&x147V5<=233&&x147V6<=227&&x147V8<=109&&x147V10<=21&&x148V2<=47&&x148V4<=43&&x148V5<=124&&x148V7<=120&&x148V8<=252&&x149V4<=12&&x149V6<=16&&x150V4<=5&&x150V5<=9&&x150V6<=7&&x150V7<=214&&x150V8<=14&&x151V1<=7&&x151V2<=22&&x151V5<=125&&x151V6<=46&&x151V7<=258&&x151V9<=24&&x151V10<=50&&x152V2<=487&&x152V5<=18&&x152V7<=45&&x152V10<=94&&x153V6<=1&&x153V7<=22&&x153V9<=44&&x154V2<=20&&x154V4<=20&&x154V5<=97&&x154V6<=20&&x154V7<=20&&x154V9<=255&&x154V10<=521&&x155V3<=17&&x155V5<=232&&x155V7<=38&&x155V8<=109&&x155V10<=19&&x156V1<=91&&x156V3<=97&&x156V4<=31&&x156V6<=197&&x156V7<=35&&x156V9<=95&&x156V10<=195&&x157V1<=1&&x157V3<=6&&x157V4<=8&&x157V5<=14&&x157V6<=1&&x157V9<=43&&x158V1<=113&&x158V5<=39&&x158V6<=17&&x158V10<=231&&x159V7<=95&&x159V10<=44&&x160V2<=296&&x160V3<=54&&x160V4<=9&&x160V8<=7&&x160V9<=24&&x161V3<=69&&x161V5<=10&&x161V9<=370&&x162V1<=15&&x162V2<=227&&x162V3<=38&&x162V4<=225&&x162V6<=227&&x162V7<=464&&x162V8<=38&&x162V9<=85&&x162V10<=19&&x163V1<=3&&x163V4<=11&&x163V6<=13&&x163V7<=31&&x163V8<=3&&x163V10<=1&&x164V1<=63&&x164V3<=332&&x164V4<=61&&x164V5<=59&&x164V6<=160&&x164V9<=164&&x164V10<=332&&x165V2<=146&&x165V3<=146&&x165V6<=306&&x165V7<=57&&x165V8<=24&&x166V1<=104&&x166V3<=23&&x166V4<=21&&x166V6<=543&&x167V6<=73&&x167V9<=5&&x168V2<=9&&x168V3<=1&&x168V5<=5&&x168V6<=128&&x168V9<=20&&x168V10<=7&&x169V2<=2&&x169V4<=4&&x169V7<=2&&x169V8<=6&&x170V2<=58&&x170V4<=54&&x170V9<=121&&x171V3<=6&&x171V5<=25&&x171V6<=3&&x171V7<=25&&x172V1<=52&&x172V2<=19&&x172V4<=48&&x172V8<=104&&x172V9<=1&&x173V2<=44&&x173V3<=40&&x173V4<=455&&x173V6<=459&&x173V10<=44&&x174V1<=1&&x174V2<=26&&x174V3<=13&&x174V4<=2&&x174V6<=4&&x174V7<=4&&x174V8<=11&&x174V10<=1&&x175V2<=276&&x175V3<=51&&x175V4<=274&&x175V5<=49&&x175V6<=11&&x175V10<=272&&x176V3<=89&&x176V6<=32&&x177V2<=1&&x177V7<=30&&x178V1<=252&&x178V2<=248&&x178V3<=122&&x178V4<=254&&x178V5<=21&&x178V7<=248&&x178V10<=4&&x179V6<=268&&x179V7<=127&&x179V9<=264&&x180V4<=83&&x180V6<=32&&x180V7<=28&&x180V8<=176&&x180V9<=180&&x181V1<=1&&x181V5<=27&&x181V8<=1&&x181V10<=2&&x182V1<=56&&x182V2<=579&&x182V4<=52&&x182V8<=21&&x182V9<=23&&x182V10<=579&&x183V1<=9&&x183V2<=11&&x183V4<=5&&x183V8<=133&&x183V9<=25&&x184V1<=114&&x184V2<=25&&x184V4<=108&&x184V5<=56&&x184V6<=578&&x184V9<=54&&x184V10<=574&&x185V1<=17&&x185V2<=386&&x186V1<=1&&x186V2<=1&&x186V3<=1&&x186V4<=1&&x186V8<=1&&x186V9<=1&&x186V10<=1&&x187V3<=20&&x187V6<=279&&x187V7<=558&&x187V9<=26&&x187V10<=110&&x188V4<=8&&x188V5<=143&&x188V6<=57&&x188V7<=147&&x188V8<=21&&x188V9<=143&&x188V10<=51&&x189V1<=40&&x189V2<=230&&x189V4<=17&&x189V6<=461&&x189V10<=42&&x190V1<=184&&x190V2<=364&&x190V3<=366&&x190V4<=370&&x190V6<=29&&x190V8<=180&&x191V1<=46&&x191V2<=48&&x191V4<=96&&x191V8<=259&&x191V9<=514&&x192V1<=84&&x192V3<=1&&x192V4<=179&&x192V5<=177&&x192V6<=14&&x192V7<=31&&x192V9<=90&&x193V1<=111&&x193V5<=286&&x193V6<=25&&x193V7<=290&&x193V9<=580&&x193V10<=27&&x194V1<=104&&x194V3<=275&&x194V7<=48&&x194V10<=20&&x195V2<=256&&x195V4<=24&&x195V5<=22&&x195V8<=11&&x195V9<=18&&x195V10<=128&&x196V2<=6&&x196V3<=5&&x196V5<=24&&x196V7<=1&&x196V10<=63&&x197V7<=1&&x197V10<=2&&x198V1<=92&&x198V2<=181&&x198V5<=185&&x198V6<=7&&x198V9<=3&&x198V10<=181&&x199V1<=11&&x199V4<=15&&x199V5<=32&&x199V7<=184&&x199V8<=30&&x199V9<=184&&x199V10<=186&&x200V2<=14&&x200V3<=74&&x200V4<=29&&x200V5<=6&&x200V6<=163&&x200V10<=25
nonNegBalanceCond
31699-If[x100V1>0,30+5 (-1+x100V1),0]-If[x102V1>0,40+5 (-1+x102V1),0]-If[x103V1>0,40+5 (-1+x103V1),0]-If[x107V1>0,400+100 (-1+x107V1),0]-If[x109V1>0,100+10 (-1+x109V1),0]-If[x111V1>0,100+10 (-1+x111V1),0]-If[x113V1>0,100+25 (-1+x113V1),0]-If[x114V1>0,100+10 (-1+x114V1),0]-If[x115V1>0,300+50 (-1+x115V1),0]-If[x116V1>0,40+5 (-1+x116V1),0]-If[x117V1>0,1000+100 (-1+x117V1),0]-If[x118V1>0,100+25 (-1+x118V1),0]-If[x119V1>0,600+100 (-1+x119V1),0]-If[x11V1>0,100+10 (-1+x11V1),0]-If[x123V1>0,50+5 (-1+x123V1),0]-If[x124V1>0,1000+100 (-1+x124V1),0]-If[x126V1>0,400+50 (-1+x126V1),0]-If[x127V1>0,1000+100 (-1+x127V1),0]-If[x128V1>0,150+25 (-1+x128V1),0]-If[x12V1>0,600+100 (-1+x12V1),0]-If[x130V1>0,20+5 (-1+x130V1),0]-If[x131V1>0,30+5 (-1+x131V1),0]-If[x132V1>0,800+100 (-1+x132V1),0]-If[x133V1>0,200+25 (-1+x133V1),0]-If[x134V1>0,50+5 (-1+x134V1),0]-If[x135V1>0,1000+100 (-1+x135V1),0]-If[x139V1>0,50+5 (-1+x139V1),0]-If[x13V1>0,100+10 (-1+x13V1),0]-If[x140V1>0,40+5 (-1+x140V1),0]-If[x142V1>0,800+100 (-1+x142V1),0]-If[x144V1>0,250+25 (-1+x144V1),0]-If[x147V1>0,40+5 (-1+x147V1),0]-If[x14V1>0,40+5 (-1+x14V1),0]-If[x151V1>0,800+100 (-1+x151V1),0]-If[x156V1>0,100+10 (-1+x156V1),0]-If[x157V1>0,600+100 (-1+x157V1),0]-If[x158V1>0,60+10 (-1+x158V1),0]-If[x15V1>0,50+5 (-1+x15V1),0]-If[x162V1>0,1000+100 (-1+x162V1),0]-If[x163V1>0,300+50 (-1+x163V1),0]-If[x164V1>0,150+25 (-1+x164V1),0]-If[x166V1>0,200+25 (-1+x166V1),0]-If[x16V1>0,150+25 (-1+x16V1),0]-If[x172V1>0,40+10 (-1+x172V1),0]-If[x174V1>0,250+25 (-1+x174V1),0]-If[x178V1>0,30+5 (-1+x178V1),0]-If[x181V1>0,1000+100 (-1+x181V1),0]-If[x182V1>0,200+50 (-1+x182V1),0]-If[x183V1>0,600+100 (-1+x183V1),0]-If[x184V1>0,100+25 (-1+x184V1),0]-If[x185V1>0,400+100 (-1+x185V1),0]-If[x186V1>0,150+25 (-1+x186V1),0]-If[x189V1>0,400+50 (-1+x189V1),0]-If[x18V1>0,400+100 (-1+x18V1),0]-If[x190V1>0,40+10 (-1+x190V1),0]-If[x191V1>0,400+50 (-1+x191V1),0]-If[x192V1>0,100+10 (-1+x192V1),0]-If[x193V1>0,200+25 (-1+x193V1),0]-If[x194V1>0,250+25 (-1+x194V1),0]-If[x198V1>0,40+10 (-1+x198V1),0]-If[x199V1>0,1000+100 (-1+x199V1),0]-If[x20V1>0,600+100 (-1+x20V1),0]-If[x22V1>0,50+5 (-1+x22V1),0]-If[x24V1>0,500+50 (-1+x24V1),0]-If[x26V1>0,50+5 (-1+x26V1),0]-If[x28V1>0,500+50 (-1+x28V1),0]-If[x29V1>0,200+50 (-1+x29V1),0]-If[x30V1>0,1000+100 (-1+x30V1),0]-If[x31V1>0,1000+100 (-1+x31V1),0]-If[x32V1>0,400+100 (-1+x32V1),0]-If[x36V1>0,500+50 (-1+x36V1),0]-If[x3V1>0,20+5 (-1+x3V1),0]-If[x40V1>0,30+5 (-1+x40V1),0]-If[x41V1>0,200+25 (-1+x41V1),0]-If[x42V1>0,1000+100 (-1+x42V1),0]-If[x43V1>0,100+25 (-1+x43V1),0]-If[x45V1>0,600+100 (-1+x45V1),0]-If[x46V1>0,250+25 (-1+x46V1),0]-If[x47V1>0,1000+100 (-1+x47V1),0]-If[x4V1>0,500+50 (-1+x4V1),0]-If[x50V1>0,150+25 (-1+x50V1),0]-If[x51V1>0,100+10 (-1+x51V1),0]-If[x53V1>0,500+50 (-1+x53V1),0]-If[x55V1>0,40+10 (-1+x55V1),0]-If[x57V1>0,500+50 (-1+x57V1),0]-If[x58V1>0,250+25 (-1+x58V1),0]-If[x5V1>0,600+100 (-1+x5V1),0]-If[x65V1>0,400+50 (-1+x65V1),0]-If[x68V1>0,150+25 (-1+x68V1),0]-If[x69V1>0,50+5 (-1+x69V1),0]-If[x6V1>0,50+5 (-1+x6V1),0]-If[x71V1>0,30+5 (-1+x71V1),0]-If[x72V1>0,800+100 (-1+x72V1),0]-If[x73V1>0,400+50 (-1+x73V1),0]-If[x75V1>0,500+50 (-1+x75V1),0]-If[x78V1>0,20+5 (-1+x78V1),0]-If[x80V1>0,60+10 (-1+x80V1),0]-If[x81V1>0,20+5 (-1+x81V1),0]-If[x82V1>0,200+50 (-1+x82V1),0]-If[x83V1>0,200+50 (-1+x83V1),0]-If[x85V1>0,200+50 (-1+x85V1),0]-If[x86V1>0,50+5 (-1+x86V1),0]-If[x87V1>0,300+50 (-1+x87V1),0]-If[x88V1>0,100+10 (-1+x88V1),0]-If[x90V1>0,60+10 (-1+x90V1),0]-If[x93V1>0,200+25 (-1+x93V1),0]-If[x95V1>0,200+25 (-1+x95V1),0]-If[x97V1>0,30+5 (-1+x97V1),0]-If[x98V1>0,400+100 (-1+x98V1),0]-If[x9V1>0,400+100 (-1+x9V1),0]>=0&&54687-If[x101V2>0,40+10 (-1+x101V2),0]-If[x103V2>0,60+10 (-1+x103V2),0]-If[x107V2>0,60+10 (-1+x107V2),0]-If[x108V2>0,800+100 (-1+x108V2),0]-If[x111V2>0,20+5 (-1+x111V2),0]-If[x114V2>0,800+100 (-1+x114V2),0]-If[x116V2>0,40+5 (-1+x116V2),0]-If[x11V2>0,20+5 (-1+x11V2),0]-If[x123V2>0,20+5 (-1+x123V2),0]-If[x125V2>0,30+5 (-1+x125V2),0]-If[x126V2>0,600+100 (-1+x126V2),0]-If[x127V2>0,40+5 (-1+x127V2),0]-If[x129V2>0,40+5 (-1+x129V2),0]-If[x131V2>0,40+5 (-1+x131V2),0]-If[x133V2>0,30+5 (-1+x133V2),0]-If[x134V2>0,50+5 (-1+x134V2),0]-If[x136V2>0,150+25 (-1+x136V2),0]-If[x137V2>0,600+100 (-1+x137V2),0]-If[x138V2>0,400+50 (-1+x138V2),0]-If[x141V2>0,80+10 (-1+x141V2),0]-If[x142V2>0,400+50 (-1+x142V2),0]-If[x143V2>0,20+5 (-1+x143V2),0]-If[x145V2>0,50+5 (-1+x145V2),0]-If[x148V2>0,150+25 (-1+x148V2),0]-If[x14V2>0,100+10 (-1+x14V2),0]-If[x151V2>0,300+50 (-1+x151V2),0]-If[x152V2>0,30+5 (-1+x152V2),0]-If[x154V2>0,800+100 (-1+x154V2),0]-If[x15V2>0,150+25 (-1+x15V2),0]-If[x160V2>0,50+5 (-1+x160V2),0]-If[x162V2>0,80+10 (-1+x162V2),0]-If[x165V2>0,100+10 (-1+x165V2),0]-If[x168V2>0,300+50 (-1+x168V2),0]-If[x169V2>0,800+100 (-1+x169V2),0]-If[x170V2>0,60+10 (-1+x170V2),0]-If[x172V2>0,100+25 (-1+x172V2),0]-If[x173V2>0,200+50 (-1+x173V2),0]-If[x174V2>0,30+5 (-1+x174V2),0]-If[x175V2>0,20+5 (-1+x175V2),0]-If[x177V2>0,800+100 (-1+x177V2),0]-If[x178V2>0,50+5 (-1+x178V2),0]-If[x17V2>0,400+100 (-1+x17V2),0]-If[x182V2>0,30+5 (-1+x182V2),0]-If[x183V2>0,400+100 (-1+x183V2),0]-If[x184V2>0,600+100 (-1+x184V2),0]-If[x185V2>0,30+5 (-1+x185V2),0]-If[x186V2>0,100+25 (-1+x186V2),0]-If[x189V2>0,60+10 (-1+x189V2),0]-If[x18V2>0,20+5 (-1+x18V2),0]-If[x190V2>0,50+5 (-1+x190V2),0]-If[x191V2>0,300+50 (-1+x191V2),0]-If[x195V2>0,50+5 (-1+x195V2),0]-If[x196V2>0,500+50 (-1+x196V2),0]-If[x198V2>0,50+5 (-1+x198V2),0]-If[x200V2>0,200+50 (-1+x200V2),0]-If[x20V2>0,30+5 (-1+x20V2),0]-If[x25V2>0,1000+100 (-1+x25V2),0]-If[x27V2>0,200+25 (-1+x27V2),0]-If[x29V2>0,150+25 (-1+x29V2),0]-If[x2V2>0,1000+100 (-1+x2V2),0]-If[x36V2>0,400+50 (-1+x36V2),0]-If[x37V2>0,20+5 (-1+x37V2),0]-If[x39V2>0,100+25 (-1+x39V2),0]-If[x44V2>0,400+50 (-1+x44V2),0]-If[x46V2>0,500+50 (-1+x46V2),0]-If[x48V2>0,300+50 (-1+x48V2),0]-If[x49V2>0,40+10 (-1+x49V2),0]-If[x4V2>0,400+50 (-1+x4V2),0]-If[x53V2>0,600+100 (-1+x53V2),0]-If[x55V2>0,40+10 (-1+x55V2),0]-If[x56V2>0,20+5 (-1+x56V2),0]-If[x58V2>0,800+100 (-1+x58V2),0]-If[x5V2>0,800+100 (-1+x5V2),0]-If[x60V2>0,250+25 (-1+x60V2),0]-If[x61V2>0,250+25 (-1+x61V2),0]-If[x62V2>0,300+50 (-1+x62V2),0]-If[x64V2>0,200+25 (-1+x64V2),0]-If[x66V2>0,600+100 (-1+x66V2),0]-If[x69V2>0,60+10 (-1+x69V2),0]-If[x70V2>0,40+5 (-1+x70V2),0]-If[x73V2>0,50+5 (-1+x73V2),0]-If[x74V2>0,40+5 (-1+x74V2),0]-If[x76V2>0,600+100 (-1+x76V2),0]-If[x78V2>0,80+10 (-1+x78V2),0]-If[x79V2>0,100+10 (-1+x79V2),0]-If[x7V2>0,100+10 (-1+x7V2),0]-If[x82V2>0,400+100 (-1+x82V2),0]-If[x83V2>0,50+5 (-1+x83V2),0]-If[x84V2>0,60+10 (-1+x84V2),0]-If[x86V2>0,300+50 (-1+x86V2),0]-If[x88V2>0,250+25 (-1+x88V2),0]-If[x89V2>0,800+100 (-1+x89V2),0]-If[x92V2>0,800+100 (-1+x92V2),0]-If[x93V2>0,300+50 (-1+x93V2),0]-If[x94V2>0,60+10 (-1+x94V2),0]-If[x97V2>0,50+5 (-1+x97V2),0]-If[x98V2>0,1000+100 (-1+x98V2),0]-If[x9V2>0,1000+100 (-1+x9V2),0]>=0&&85057-If[x100V3>0,600+100 (-1+x100V3),0]-If[x101V3>0,400+100 (-1+x101V3),0]-If[x102V3>0,150+25 (-1+x102V3),0]-If[x103V3>0,600+100 (-1+x103V3),0]-If[x106V3>0,40+5 (-1+x106V3),0]-If[x111V3>0,80+10 (-1+x111V3),0]-If[x112V3>0,60+10 (-1+x112V3),0]-If[x114V3>0,600+100 (-1+x114V3),0]-If[x117V3>0,200+25 (-1+x117V3),0]-If[x11V3>0,300+50 (-1+x11V3),0]-If[x12V3>0,40+10 (-1+x12V3),0]-If[x131V3>0,80+10 (-1+x131V3),0]-If[x132V3>0,20+5 (-1+x132V3),0]-If[x134V3>0,200+25 (-1+x134V3),0]-If[x135V3>0,40+10 (-1+x135V3),0]-If[x136V3>0,150+25 (-1+x136V3),0]-If[x137V3>0,100+25 (-1+x137V3),0]-If[x138V3>0,1000+100 (-1+x138V3),0]-If[x139V3>0,20+5 (-1+x139V3),0]-If[x13V3>0,200+50 (-1+x13V3),0]-If[x140V3>0,40+5 (-1+x140V3),0]-If[x142V3>0,100+10 (-1+x142V3),0]-If[x146V3>0,400+100 (-1+x146V3),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x155V3>0,400+50 (-1+x155V3),0]-If[x156V3>0,40+10 (-1+x156V3),0]-If[x157V3>0,300+50 (-1+x157V3),0]-If[x160V3>0,200+25 (-1+x160V3),0]-If[x161V3>0,200+25 (-1+x161V3),0]-If[x162V3>0,500+50 (-1+x162V3),0]-If[x164V3>0,30+5 (-1+x164V3),0]-If[x165V3>0,100+10 (-1+x165V3),0]-If[x166V3>0,600+100 (-1+x166V3),0]-If[x168V3>0,800+100 (-1+x168V3),0]-If[x16V3>0,200+25 (-1+x16V3),0]-If[x171V3>0,150+25 (-1+x171V3),0]-If[x173V3>0,400+50 (-1+x173V3),0]-If[x174V3>0,40+10 (-1+x174V3),0]-If[x175V3>0,150+25 (-1+x175V3),0]-If[x176V3>0,80+10 (-1+x176V3),0]-If[x178V3>0,80+10 (-1+x178V3),0]-If[x17V3>0,30+5 (-1+x17V3),0]-If[x186V3>0,400+100 (-1+x186V3),0]-If[x187V3>0,1000+100 (-1+x187V3),0]-If[x18V3>0,60+10 (-1+x18V3),0]-If[x190V3>0,40+5 (-1+x190V3),0]-If[x192V3>0,1000+100 (-1+x192V3),0]-If[x194V3>0,80+10 (-1+x194V3),0]-If[x196V3>0,400+100 (-1+x196V3),0]-If[x19V3>0,600+100 (-1+x19V3),0]-If[x200V3>0,100+10 (-1+x200V3),0]-If[x20V3>0,100+25 (-1+x20V3),0]-If[x21V3>0,600+100 (-1+x21V3),0]-If[x22V3>0,150+25 (-1+x22V3),0]-If[x24V3>0,150+25 (-1+x24V3),0]-If[x29V3>0,500+50 (-1+x29V3),0]-If[x31V3>0,40+5 (-1+x31V3),0]-If[x32V3>0,1000+100 (-1+x32V3),0]-If[x35V3>0,200+50 (-1+x35V3),0]-If[x36V3>0,200+50 (-1+x36V3),0]-If[x37V3>0,30+5 (-1+x37V3),0]-If[x3V3>0,400+100 (-1+x3V3),0]-If[x41V3>0,300+50 (-1+x41V3),0]-If[x42V3>0,500+50 (-1+x42V3),0]-If[x43V3>0,150+25 (-1+x43V3),0]-If[x45V3>0,100+10 (-1+x45V3),0]-If[x46V3>0,60+10 (-1+x46V3),0]-If[x47V3>0,50+5 (-1+x47V3),0]-If[x48V3>0,200+50 (-1+x48V3),0]-If[x49V3>0,800+100 (-1+x49V3),0]-If[x50V3>0,20+5 (-1+x50V3),0]-If[x52V3>0,50+5 (-1+x52V3),0]-If[x56V3>0,250+25 (-1+x56V3),0]-If[x58V3>0,500+50 (-1+x58V3),0]-If[x63V3>0,100+25 (-1+x63V3),0]-If[x65V3>0,300+50 (-1+x65V3),0]-If[x66V3>0,250+25 (-1+x66V3),0]-If[x70V3>0,60+10 (-1+x70V3),0]-If[x71V3>0,250+25 (-1+x71V3),0]-If[x74V3>0,500+50 (-1+x74V3),0]-If[x76V3>0,100+25 (-1+x76V3),0]-If[x77V3>0,1000+100 (-1+x77V3),0]-If[x78V3>0,80+10 (-1+x78V3),0]-If[x81V3>0,100+25 (-1+x81V3),0]-If[x85V3>0,200+50 (-1+x85V3),0]-If[x8V3>0,50+5 (-1+x8V3),0]-If[x90V3>0,150+25 (-1+x90V3),0]-If[x94V3>0,250+25 (-1+x94V3),0]-If[x95V3>0,300+50 (-1+x95V3),0]-If[x96V3>0,300+50 (-1+x96V3),0]-If[x97V3>0,200+25 (-1+x97V3),0]-If[x98V3>0,60+10 (-1+x98V3),0]-If[x99V3>0,40+10 (-1+x99V3),0]>=0&&149097-If[x100V4>0,80+10 (-1+x100V4),0]-If[x102V4>0,100+10 (-1+x102V4),0]-If[x103V4>0,600+100 (-1+x103V4),0]-If[x106V4>0,100+10 (-1+x106V4),0]-If[x109V4>0,100+10 (-1+x109V4),0]-If[x111V4>0,600+100 (-1+x111V4),0]-If[x112V4>0,50+5 (-1+x112V4),0]-If[x113V4>0,20+5 (-1+x113V4),0]-If[x114V4>0,60+10 (-1+x114V4),0]-If[x115V4>0,1000+100 (-1+x115V4),0]-If[x116V4>0,60+10 (-1+x116V4),0]-If[x117V4>0,600+100 (-1+x117V4),0]-If[x11V4>0,300+50 (-1+x11V4),0]-If[x120V4>0,600+100 (-1+x120V4),0]-If[x123V4>0,30+5 (-1+x123V4),0]-If[x125V4>0,50+5 (-1+x125V4),0]-If[x126V4>0,300+50 (-1+x126V4),0]-If[x127V4>0,60+10 (-1+x127V4),0]-If[x128V4>0,400+50 (-1+x128V4),0]-If[x12V4>0,100+10 (-1+x12V4),0]-If[x131V4>0,40+10 (-1+x131V4),0]-If[x132V4>0,30+5 (-1+x132V4),0]-If[x133V4>0,100+25 (-1+x133V4),0]-If[x137V4>0,400+50 (-1+x137V4),0]-If[x138V4>0,200+50 (-1+x138V4),0]-If[x139V4>0,1000+100 (-1+x139V4),0]-If[x13V4>0,600+100 (-1+x13V4),0]-If[x140V4>0,80+10 (-1+x140V4),0]-If[x141V4>0,60+10 (-1+x141V4),0]-If[x142V4>0,100+10 (-1+x142V4),0]-If[x147V4>0,30+5 (-1+x147V4),0]-If[x148V4>0,250+25 (-1+x148V4),0]-If[x149V4>0,800+100 (-1+x149V4),0]-If[x14V4>0,50+5 (-1+x14V4),0]-If[x150V4>0,800+100 (-1+x150V4),0]-If[x154V4>0,800+100 (-1+x154V4),0]-If[x156V4>0,250+25 (-1+x156V4),0]-If[x157V4>0,200+50 (-1+x157V4),0]-If[x160V4>0,800+100 (-1+x160V4),0]-If[x162V4>0,100+10 (-1+x162V4),0]-If[x163V4>0,150+25 (-1+x163V4),0]-If[x164V4>0,200+25 (-1+x164V4),0]-If[x166V4>0,800+100 (-1+x166V4),0]-If[x169V4>0,600+100 (-1+x169V4),0]-If[x170V4>0,100+10 (-1+x170V4),0]-If[x172V4>0,80+10 (-1+x172V4),0]-If[x173V4>0,40+5 (-1+x173V4),0]-If[x174V4>0,150+25 (-1+x174V4),0]-If[x175V4>0,30+5 (-1+x175V4),0]-If[x178V4>0,20+5 (-1+x178V4),0]-If[x17V4>0,200+50 (-1+x17V4),0]-If[x180V4>0,100+10 (-1+x180V4),0]-If[x182V4>0,400+50 (-1+x182V4),0]-If[x183V4>0,1000+100 (-1+x183V4),0]-If[x184V4>0,250+25 (-1+x184V4),0]-If[x186V4>0,500+50 (-1+x186V4),0]-If[x188V4>0,800+100 (-1+x188V4),0]-If[x189V4>0,800+100 (-1+x189V4),0]-If[x18V4>0,150+25 (-1+x18V4),0]-If[x190V4>0,20+5 (-1+x190V4),0]-If[x191V4>0,250+25 (-1+x191V4),0]-If[x192V4>0,40+5 (-1+x192V4),0]-If[x195V4>0,200+50 (-1+x195V4),0]-If[x199V4>0,600+100 (-1+x199V4),0]-If[x200V4>0,150+25 (-1+x200V4),0]-If[x22V4>0,800+100 (-1+x22V4),0]-If[x23V4>0,250+25 (-1+x23V4),0]-If[x24V4>0,500+50 (-1+x24V4),0]-If[x25V4>0,80+10 (-1+x25V4),0]-If[x27V4>0,50+5 (-1+x27V4),0]-If[x29V4>0,200+25 (-1+x29V4),0]-If[x30V4>0,100+25 (-1+x30V4),0]-If[x34V4>0,40+5 (-1+x34V4),0]-If[x35V4>0,100+10 (-1+x35V4),0]-If[x36V4>0,250+25 (-1+x36V4),0]-If[x39V4>0,200+25 (-1+x39V4),0]-If[x42V4>0,1000+100 (-1+x42V4),0]-If[x43V4>0,800+100 (-1+x43V4),0]-If[x44V4>0,40+10 (-1+x44V4),0]-If[x46V4>0,250+25 (-1+x46V4),0]-If[x48V4>0,30+5 (-1+x48V4),0]-If[x49V4>0,20+5 (-1+x49V4),0]-If[x4V4>0,40+5 (-1+x4V4),0]-If[x51V4>0,800+100 (-1+x51V4),0]-If[x54V4>0,500+50 (-1+x54V4),0]-If[x55V4>0,20+5 (-1+x55V4),0]-If[x57V4>0,1000+100 (-1+x57V4),0]-If[x58V4>0,100+25 (-1+x58V4),0]-If[x5V4>0,60+10 (-1+x5V4),0]-If[x60V4>0,1000+100 (-1+x60V4),0]-If[x63V4>0,200+50 (-1+x63V4),0]-If[x64V4>0,80+10 (-1+x64V4),0]-If[x65V4>0,100+25 (-1+x65V4),0]-If[x66V4>0,30+5 (-1+x66V4),0]-If[x67V4>0,60+10 (-1+x67V4),0]-If[x68V4>0,400+100 (-1+x68V4),0]-If[x69V4>0,100+10 (-1+x69V4),0]-If[x6V4>0,250+25 (-1+x6V4),0]-If[x70V4>0,60+10 (-1+x70V4),0]-If[x74V4>0,80+10 (-1+x74V4),0]-If[x75V4>0,30+5 (-1+x75V4),0]-If[x76V4>0,500+50 (-1+x76V4),0]-If[x77V4>0,400+100 (-1+x77V4),0]-If[x78V4>0,20+5 (-1+x78V4),0]-If[x7V4>0,150+25 (-1+x7V4),0]-If[x82V4>0,150+25 (-1+x82V4),0]-If[x84V4>0,300+50 (-1+x84V4),0]-If[x85V4>0,40+10 (-1+x85V4),0]-If[x86V4>0,100+10 (-1+x86V4),0]-If[x88V4>0,100+25 (-1+x88V4),0]-If[x90V4>0,100+10 (-1+x90V4),0]-If[x92V4>0,20+5 (-1+x92V4),0]-If[x93V4>0,30+5 (-1+x93V4),0]-If[x96V4>0,200+50 (-1+x96V4),0]-If[x99V4>0,40+5 (-1+x99V4),0]-If[x9V4>0,150+25 (-1+x9V4),0]>=0&&55197-If[x101V5>0,600+100 (-1+x101V5),0]-If[x107V5>0,30+5 (-1+x107V5),0]-If[x108V5>0,60+10 (-1+x108V5),0]-If[x10V5>0,50+5 (-1+x10V5),0]-If[x112V5>0,100+25 (-1+x112V5),0]-If[x113V5>0,40+5 (-1+x113V5),0]-If[x116V5>0,40+10 (-1+x116V5),0]-If[x121V5>0,80+10 (-1+x121V5),0]-If[x122V5>0,40+10 (-1+x122V5),0]-If[x124V5>0,1000+100 (-1+x124V5),0]-If[x125V5>0,600+100 (-1+x125V5),0]-If[x127V5>0,100+10 (-1+x127V5),0]-If[x129V5>0,600+100 (-1+x129V5),0]-If[x12V5>0,40+10 (-1+x12V5),0]-If[x131V5>0,200+25 (-1+x131V5),0]-If[x132V5>0,1000+100 (-1+x132V5),0]-If[x133V5>0,250+25 (-1+x133V5),0]-If[x134V5>0,20+5 (-1+x134V5),0]-If[x136V5>0,30+5 (-1+x136V5),0]-If[x138V5>0,100+25 (-1+x138V5),0]-If[x139V5>0,200+50 (-1+x139V5),0]-If[x140V5>0,800+100 (-1+x140V5),0]-If[x141V5>0,100+25 (-1+x141V5),0]-If[x145V5>0,40+5 (-1+x145V5),0]-If[x146V5>0,150+25 (-1+x146V5),0]-If[x147V5>0,20+5 (-1+x147V5),0]-If[x148V5>0,60+10 (-1+x148V5),0]-If[x14V5>0,400+50 (-1+x14V5),0]-If[x150V5>0,400+100 (-1+x150V5),0]-If[x151V5>0,80+10 (-1+x151V5),0]-If[x152V5>0,800+100 (-1+x152V5),0]-If[x154V5>0,250+25 (-1+x154V5),0]-If[x155V5>0,20+5 (-1+x155V5),0]-If[x157V5>0,200+25 (-1+x157V5),0]-If[x158V5>0,250+25 (-1+x158V5),0]-If[x15V5>0,250+25 (-1+x15V5),0]-If[x161V5>0,1000+100 (-1+x161V5),0]-If[x164V5>0,250+25 (-1+x164V5),0]-If[x168V5>0,500+50 (-1+x168V5),0]-If[x171V5>0,40+10 (-1+x171V5),0]-If[x175V5>0,200+25 (-1+x175V5),0]-If[x178V5>0,300+50 (-1+x178V5),0]-If[x17V5>0,250+25 (-1+x17V5),0]-If[x181V5>0,250+25 (-1+x181V5),0]-If[x184V5>0,200+50 (-1+x184V5),0]-If[x188V5>0,80+10 (-1+x188V5),0]-If[x18V5>0,400+100 (-1+x18V5),0]-If[x192V5>0,50+5 (-1+x192V5),0]-If[x193V5>0,100+10 (-1+x193V5),0]-If[x195V5>0,300+50 (-1+x195V5),0]-If[x196V5>0,150+25 (-1+x196V5),0]-If[x198V5>0,30+5 (-1+x198V5),0]-If[x199V5>0,400+50 (-1+x199V5),0]-If[x19V5>0,800+100 (-1+x19V5),0]-If[x1V5>0,200+50 (-1+x1V5),0]-If[x200V5>0,400+100 (-1+x200V5),0]-If[x21V5>0,100+25 (-1+x21V5),0]-If[x23V5>0,300+50 (-1+x23V5),0]-If[x24V5>0,200+50 (-1+x24V5),0]-If[x27V5>0,250+25 (-1+x27V5),0]-If[x29V5>0,600+100 (-1+x29V5),0]-If[x2V5>0,60+10 (-1+x2V5),0]-If[x31V5>0,500+50 (-1+x31V5),0]-If[x41V5>0,100+10 (-1+x41V5),0]-If[x43V5>0,40+10 (-1+x43V5),0]-If[x46V5>0,200+50 (-1+x46V5),0]-If[x47V5>0,500+50 (-1+x47V5),0]-If[x49V5>0,300+50 (-1+x49V5),0]-If[x57V5>0,150+25 (-1+x57V5),0]-If[x58V5>0,400+50 (-1+x58V5),0]-If[x59V5>0,20+5 (-1+x59V5),0]-If[x61V5>0,800+100 (-1+x61V5),0]-If[x63V5>0,150+25 (-1+x63V5),0]-If[x66V5>0,150+25 (-1+x66V5),0]-If[x68V5>0,30+5 (-1+x68V5),0]-If[x69V5>0,250+25 (-1+x69V5),0]-If[x75V5>0,400+50 (-1+x75V5),0]-If[x78V5>0,40+10 (-1+x78V5),0]-If[x80V5>0,800+100 (-1+x80V5),0]-If[x84V5>0,200+50 (-1+x84V5),0]-If[x88V5>0,20+5 (-1+x88V5),0]-If[x90V5>0,150+25 (-1+x90V5),0]-If[x93V5>0,1000+100 (-1+x93V5),0]-If[x94V5>0,200+25 (-1+x94V5),0]-If[x95V5>0,400+100 (-1+x95V5),0]-If[x96V5>0,40+10 (-1+x96V5),0]-If[x98V5>0,40+10 (-1+x98V5),0]>=0&&35370-If[x100V6>0,40+5 (-1+x100V6),0]-If[x101V6>0,40+5 (-1+x101V6),0]-If[x103V6>0,200+50 (-1+x103V6),0]-If[x104V6>0,80+10 (-1+x104V6),0]-If[x105V6>0,30+5 (-1+x105V6),0]-If[x106V6>0,800+100 (-1+x106V6),0]-If[x109V6>0,600+100 (-1+x109V6),0]-If[x112V6>0,800+100 (-1+x112V6),0]-If[x113V6>0,600+100 (-1+x113V6),0]-If[x114V6>0,50+5 (-1+x114V6),0]-If[x116V6>0,80+10 (-1+x116V6),0]-If[x117V6>0,400+100 (-1+x117V6),0]-If[x121V6>0,60+10 (-1+x121V6),0]-If[x123V6>0,500+50 (-1+x123V6),0]-If[x125V6>0,40+5 (-1+x125V6),0]-If[x126V6>0,50+5 (-1+x126V6),0]-If[x127V6>0,20+5 (-1+x127V6),0]-If[x132V6>0,100+10 (-1+x132V6),0]-If[x135V6>0,1000+100 (-1+x135V6),0]-If[x136V6>0,50+5 (-1+x136V6),0]-If[x138V6>0,400+50 (-1+x138V6),0]-If[x139V6>0,250+25 (-1+x139V6),0]-If[x141V6>0,500+50 (-1+x141V6),0]-If[x142V6>0,400+50 (-1+x142V6),0]-If[x143V6>0,250+25 (-1+x143V6),0]-If[x145V6>0,200+50 (-1+x145V6),0]-If[x147V6>0,50+5 (-1+x147V6),0]-If[x149V6>0,400+100 (-1+x149V6),0]-If[x14V6>0,250+25 (-1+x14V6),0]-If[x150V6>0,600+100 (-1+x150V6),0]-If[x151V6>0,200+25 (-1+x151V6),0]-If[x153V6>0,400+50 (-1+x153V6),0]-If[x154V6>0,800+100 (-1+x154V6),0]-If[x156V6>0,20+5 (-1+x156V6),0]-If[x157V6>0,1000+100 (-1+x157V6),0]-If[x158V6>0,400+50 (-1+x158V6),0]-If[x162V6>0,80+10 (-1+x162V6),0]-If[x163V6>0,100+25 (-1+x163V6),0]-If[x164V6>0,100+10 (-1+x164V6),0]-If[x165V6>0,20+5 (-1+x165V6),0]-If[x166V6>0,50+5 (-1+x166V6),0]-If[x167V6>0,20+5 (-1+x167V6),0]-If[x168V6>0,40+5 (-1+x168V6),0]-If[x171V6>0,200+50 (-1+x171V6),0]-If[x173V6>0,20+5 (-1+x173V6),0]-If[x174V6>0,100+25 (-1+x174V6),0]-If[x175V6>0,400+100 (-1+x175V6),0]-If[x176V6>0,200+25 (-1+x176V6),0]-If[x179V6>0,20+5 (-1+x179V6),0]-If[x17V6>0,300+50 (-1+x17V6),0]-If[x180V6>0,150+25 (-1+x180V6),0]-If[x184V6>0,30+5 (-1+x184V6),0]-If[x187V6>0,40+10 (-1+x187V6),0]-If[x188V6>0,100+25 (-1+x188V6),0]-If[x189V6>0,50+5 (-1+x189V6),0]-If[x18V6>0,200+25 (-1+x18V6),0]-If[x190V6>0,500+50 (-1+x190V6),0]-If[x192V6>0,300+50 (-1+x192V6),0]-If[x193V6>0,600+100 (-1+x193V6),0]-If[x198V6>0,400+100 (-1+x198V6),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x200V6>0,20+5 (-1+x200V6),0]-If[x20V6>0,250+25 (-1+x20V6),0]-If[x21V6>0,50+5 (-1+x21V6),0]-If[x22V6>0,20+5 (-1+x22V6),0]-If[x26V6>0,30+5 (-1+x26V6),0]-If[x28V6>0,40+10 (-1+x28V6),0]-If[x2V6>0,40+10 (-1+x2V6),0]-If[x31V6>0,500+50 (-1+x31V6),0]-If[x34V6>0,60+10 (-1+x34V6),0]-If[x36V6>0,50+5 (-1+x36V6),0]-If[x41V6>0,800+100 (-1+x41V6),0]-If[x42V6>0,800+100 (-1+x42V6),0]-If[x44V6>0,80+10 (-1+x44V6),0]-If[x46V6>0,600+100 (-1+x46V6),0]-If[x47V6>0,30+5 (-1+x47V6),0]-If[x48V6>0,100+10 (-1+x48V6),0]-If[x50V6>0,500+50 (-1+x50V6),0]-If[x51V6>0,200+50 (-1+x51V6),0]-If[x55V6>0,1000+100 (-1+x55V6),0]-If[x57V6>0,60+10 (-1+x57V6),0]-If[x58V6>0,20+5 (-1+x58V6),0]-If[x5V6>0,150+25 (-1+x5V6),0]-If[x63V6>0,400+50 (-1+x63V6),0]-If[x66V6>0,50+5 (-1+x66V6),0]-If[x70V6>0,150+25 (-1+x70V6),0]-If[x73V6>0,20+5 (-1+x73V6),0]-If[x74V6>0,20+5 (-1+x74V6),0]-If[x75V6>0,100+10 (-1+x75V6),0]-If[x77V6>0,400+100 (-1+x77V6),0]-If[x78V6>0,40+10 (-1+x78V6),0]-If[x79V6>0,800+100 (-1+x79V6),0]-If[x7V6>0,100+10 (-1+x7V6),0]-If[x82V6>0,400+50 (-1+x82V6),0]-If[x84V6>0,60+10 (-1+x84V6),0]-If[x85V6>0,800+100 (-1+x85V6),0]-If[x86V6>0,400+100 (-1+x86V6),0]-If[x87V6>0,30+5 (-1+x87V6),0]-If[x88V6>0,200+25 (-1+x88V6),0]-If[x89V6>0,400+100 (-1+x89V6),0]-If[x91V6>0,800+100 (-1+x91V6),0]-If[x96V6>0,50+5 (-1+x96V6),0]-If[x97V6>0,800+100 (-1+x97V6),0]-If[x98V6>0,30+5 (-1+x98V6),0]-If[x9V6>0,100+25 (-1+x9V6),0]>=0&&135474-If[x101V7>0,60+10 (-1+x101V7),0]-If[x102V7>0,30+5 (-1+x102V7),0]-If[x105V7>0,600+100 (-1+x105V7),0]-If[x107V7>0,500+50 (-1+x107V7),0]-If[x109V7>0,200+25 (-1+x109V7),0]-If[x10V7>0,20+5 (-1+x10V7),0]-If[x110V7>0,250+25 (-1+x110V7),0]-If[x111V7>0,500+50 (-1+x111V7),0]-If[x112V7>0,20+5 (-1+x112V7),0]-If[x113V7>0,400+50 (-1+x113V7),0]-If[x114V7>0,250+25 (-1+x114V7),0]-If[x115V7>0,1000+100 (-1+x115V7),0]-If[x117V7>0,400+50 (-1+x117V7),0]-If[x118V7>0,30+5 (-1+x118V7),0]-If[x120V7>0,800+100 (-1+x120V7),0]-If[x122V7>0,40+5 (-1+x122V7),0]-If[x124V7>0,800+100 (-1+x124V7),0]-If[x125V7>0,40+10 (-1+x125V7),0]-If[x127V7>0,250+25 (-1+x127V7),0]-If[x129V7>0,300+50 (-1+x129V7),0]-If[x131V7>0,500+50 (-1+x131V7),0]-If[x132V7>0,30+5 (-1+x132V7),0]-If[x134V7>0,50+5 (-1+x134V7),0]-If[x135V7>0,100+25 (-1+x135V7),0]-If[x136V7>0,30+5 (-1+x136V7),0]-If[x138V7>0,30+5 (-1+x138V7),0]-If[x13V7>0,60+10 (-1+x13V7),0]-If[x140V7>0,100+25 (-1+x140V7),0]-If[x141V7>0,100+25 (-1+x141V7),0]-If[x144V7>0,400+50 (-1+x144V7),0]-If[x145V7>0,200+25 (-1+x145V7),0]-If[x148V7>0,100+10 (-1+x148V7),0]-If[x14V7>0,250+25 (-1+x14V7),0]-If[x150V7>0,50+5 (-1+x150V7),0]-If[x151V7>0,30+5 (-1+x151V7),0]-If[x152V7>0,300+50 (-1+x152V7),0]-If[x153V7>0,40+10 (-1+x153V7),0]-If[x154V7>0,800+100 (-1+x154V7),0]-If[x155V7>0,250+25 (-1+x155V7),0]-If[x156V7>0,150+25 (-1+x156V7),0]-If[x159V7>0,200+25 (-1+x159V7),0]-If[x162V7>0,20+5 (-1+x162V7),0]-If[x163V7>0,80+10 (-1+x163V7),0]-If[x165V7>0,150+25 (-1+x165V7),0]-If[x169V7>0,800+100 (-1+x169V7),0]-If[x171V7>0,40+10 (-1+x171V7),0]-If[x174V7>0,100+25 (-1+x174V7),0]-If[x177V7>0,50+5 (-1+x177V7),0]-If[x178V7>0,50+5 (-1+x178V7),0]-If[x179V7>0,100+10 (-1+x179V7),0]-If[x180V7>0,250+25 (-1+x180V7),0]-If[x187V7>0,30+5 (-1+x187V7),0]-If[x188V7>0,40+10 (-1+x188V7),0]-If[x18V7>0,200+50 (-1+x18V7),0]-If[x192V7>0,200+25 (-1+x192V7),0]-If[x193V7>0,60+10 (-1+x193V7),0]-If[x194V7>0,500+50 (-1+x194V7),0]-If[x196V7>0,1000+100 (-1+x196V7),0]-If[x197V7>0,1000+100 (-1+x197V7),0]-If[x199V7>0,100+10 (-1+x199V7),0]-If[x1V7>0,80+10 (-1+x1V7),0]-If[x21V7>0,300+50 (-1+x21V7),0]-If[x24V7>0,300+50 (-1+x24V7),0]-If[x25V7>0,50+5 (-1+x25V7),0]-If[x26V7>0,400+50 (-1+x26V7),0]-If[x28V7>0,50+5 (-1+x28V7),0]-If[x2V7>0,500+50 (-1+x2V7),0]-If[x31V7>0,150+25 (-1+x31V7),0]-If[x33V7>0,40+5 (-1+x33V7),0]-If[x34V7>0,80+10 (-1+x34V7),0]-If[x41V7>0,600+100 (-1+x41V7),0]-If[x42V7>0,80+10 (-1+x42V7),0]-If[x44V7>0,300+50 (-1+x44V7),0]-If[x48V7>0,200+25 (-1+x48V7),0]-If[x53V7>0,400+100 (-1+x53V7),0]-If[x55V7>0,500+50 (-1+x55V7),0]-If[x58V7>0,100+10 (-1+x58V7),0]-If[x59V7>0,100+25 (-1+x59V7),0]-If[x5V7>0,150+25 (-1+x5V7),0]-If[x60V7>0,20+5 (-1+x60V7),0]-If[x61V7>0,100+10 (-1+x61V7),0]-If[x62V7>0,200+25 (-1+x62V7),0]-If[x63V7>0,40+10 (-1+x63V7),0]-If[x68V7>0,500+50 (-1+x68V7),0]-If[x69V7>0,300+50 (-1+x69V7),0]-If[x70V7>0,50+5 (-1+x70V7),0]-If[x71V7>0,1000+100 (-1+x71V7),0]-If[x72V7>0,400+100 (-1+x72V7),0]-If[x75V7>0,400+50 (-1+x75V7),0]-If[x76V7>0,40+5 (-1+x76V7),0]-If[x79V7>0,150+25 (-1+x79V7),0]-If[x82V7>0,40+10 (-1+x82V7),0]-If[x84V7>0,60+10 (-1+x84V7),0]-If[x85V7>0,30+5 (-1+x85V7),0]-If[x86V7>0,100+10 (-1+x86V7),0]-If[x87V7>0,200+25 (-1+x87V7),0]-If[x88V7>0,200+25 (-1+x88V7),0]-If[x8V7>0,150+25 (-1+x8V7),0]-If[x90V7>0,500+50 (-1+x90V7),0]-If[x92V7>0,250+25 (-1+x92V7),0]-If[x93V7>0,40+10 (-1+x93V7),0]-If[x94V7>0,500+50 (-1+x94V7),0]-If[x96V7>0,250+25 (-1+x96V7),0]-If[x98V7>0,150+25 (-1+x98V7),0]>=0&&80689-If[x100V8>0,1000+100 (-1+x100V8),0]-If[x106V8>0,100+10 (-1+x106V8),0]-If[x109V8>0,40+5 (-1+x109V8),0]-If[x10V8>0,100+25 (-1+x10V8),0]-If[x111V8>0,200+25 (-1+x111V8),0]-If[x114V8>0,400+100 (-1+x114V8),0]-If[x116V8>0,60+10 (-1+x116V8),0]-If[x118V8>0,500+50 (-1+x118V8),0]-If[x122V8>0,400+100 (-1+x122V8),0]-If[x126V8>0,800+100 (-1+x126V8),0]-If[x12V8>0,80+10 (-1+x12V8),0]-If[x133V8>0,150+25 (-1+x133V8),0]-If[x134V8>0,150+25 (-1+x134V8),0]-If[x137V8>0,100+10 (-1+x137V8),0]-If[x138V8>0,1000+100 (-1+x138V8),0]-If[x139V8>0,800+100 (-1+x139V8),0]-If[x140V8>0,600+100 (-1+x140V8),0]-If[x141V8>0,800+100 (-1+x141V8),0]-If[x142V8>0,150+25 (-1+x142V8),0]-If[x143V8>0,20+5 (-1+x143V8),0]-If[x145V8>0,400+50 (-1+x145V8),0]-If[x147V8>0,100+10 (-1+x147V8),0]-If[x148V8>0,30+5 (-1+x148V8),0]-If[x150V8>0,500+50 (-1+x150V8),0]-If[x155V8>0,100+10 (-1+x155V8),0]-If[x160V8>0,1000+100 (-1+x160V8),0]-If[x162V8>0,500+50 (-1+x162V8),0]-If[x163V8>0,300+50 (-1+x163V8),0]-If[x165V8>0,400+50 (-1+x165V8),0]-If[x169V8>0,400+100 (-1+x169V8),0]-If[x172V8>0,30+5 (-1+x172V8),0]-If[x174V8>0,60+10 (-1+x174V8),0]-If[x17V8>0,30+5 (-1+x17V8),0]-If[x180V8>0,40+5 (-1+x180V8),0]-If[x181V8>0,1000+100 (-1+x181V8),0]-If[x182V8>0,1000+100 (-1+x182V8),0]-If[x183V8>0,40+10 (-1+x183V8),0]-If[x186V8>0,500+50 (-1+x186V8),0]-If[x188V8>0,500+50 (-1+x188V8),0]-If[x190V8>0,80+10 (-1+x190V8),0]-If[x191V8>0,40+10 (-1+x191V8),0]-If[x195V8>0,400+100 (-1+x195V8),0]-If[x199V8>0,500+50 (-1+x199V8),0]-If[x19V8>0,50+5 (-1+x19V8),0]-If[x20V8>0,400+100 (-1+x20V8),0]-If[x28V8>0,400+50 (-1+x28V8),0]-If[x29V8>0,100+25 (-1+x29V8),0]-If[x32V8>0,600+100 (-1+x32V8),0]-If[x34V8>0,30+5 (-1+x34V8),0]-If[x35V8>0,200+25 (-1+x35V8),0]-If[x38V8>0,40+5 (-1+x38V8),0]-If[x39V8>0,200+25 (-1+x39V8),0]-If[x41V8>0,60+10 (-1+x41V8),0]-If[x43V8>0,200+50 (-1+x43V8),0]-If[x45V8>0,40+10 (-1+x45V8),0]-If[x47V8>0,200+50 (-1+x47V8),0]-If[x48V8>0,500+50 (-1+x48V8),0]-If[x4V8>0,20+5 (-1+x4V8),0]-If[x50V8>0,500+50 (-1+x50V8),0]-If[x51V8>0,300+50 (-1+x51V8),0]-If[x52V8>0,300+50 (-1+x52V8),0]-If[x53V8>0,60+10 (-1+x53V8),0]-If[x55V8>0,100+10 (-1+x55V8),0]-If[x56V8>0,1000+100 (-1+x56V8),0]-If[x57V8>0,30+5 (-1+x57V8),0]-If[x58V8>0,60+10 (-1+x58V8),0]-If[x5V8>0,600+100 (-1+x5V8),0]-If[x63V8>0,20+5 (-1+x63V8),0]-If[x64V8>0,50+5 (-1+x64V8),0]-If[x66V8>0,100+10 (-1+x66V8),0]-If[x67V8>0,400+100 (-1+x67V8),0]-If[x68V8>0,50+5 (-1+x68V8),0]-If[x69V8>0,150+25 (-1+x69V8),0]-If[x6V8>0,50+5 (-1+x6V8),0]-If[x71V8>0,40+10 (-1+x71V8),0]-If[x73V8>0,80+10 (-1+x73V8),0]-If[x76V8>0,80+10 (-1+x76V8),0]-If[x81V8>0,150+25 (-1+x81V8),0]-If[x84V8>0,60+10 (-1+x84V8),0]-If[x85V8>0,100+25 (-1+x85V8),0]-If[x86V8>0,40+10 (-1+x86V8),0]-If[x87V8>0,100+10 (-1+x87V8),0]-If[x88V8>0,400+100 (-1+x88V8),0]-If[x89V8>0,400+50 (-1+x89V8),0]-If[x8V8>0,20+5 (-1+x8V8),0]-If[x90V8>0,250+25 (-1+x90V8),0]-If[x91V8>0,100+10 (-1+x91V8),0]-If[x93V8>0,20+5 (-1+x93V8),0]-If[x94V8>0,600+100 (-1+x94V8),0]-If[x95V8>0,300+50 (-1+x95V8),0]-If[x9V8>0,600+100 (-1+x9V8),0]>=0&&11090-If[x100V9>0,20+5 (-1+x100V9),0]-If[x101V9>0,100+10 (-1+x101V9),0]-If[x102V9>0,100+10 (-1+x102V9),0]-If[x103V9>0,400+50 (-1+x103V9),0]-If[x104V9>0,40+10 (-1+x104V9),0]-If[x106V9>0,60+10 (-1+x106V9),0]-If[x107V9>0,50+5 (-1+x107V9),0]-If[x111V9>0,200+25 (-1+x111V9),0]-If[x112V9>0,250+25 (-1+x112V9),0]-If[x113V9>0,800+100 (-1+x113V9),0]-If[x114V9>0,80+10 (-1+x114V9),0]-If[x115V9>0,80+10 (-1+x115V9),0]-If[x116V9>0,40+10 (-1+x116V9),0]-If[x117V9>0,600+100 (-1+x117V9),0]-If[x118V9>0,40+5 (-1+x118V9),0]-If[x119V9>0,60+10 (-1+x119V9),0]-If[x11V9>0,100+25 (-1+x11V9),0]-If[x120V9>0,40+10 (-1+x120V9),0]-If[x122V9>0,100+10 (-1+x122V9),0]-If[x123V9>0,30+5 (-1+x123V9),0]-If[x125V9>0,200+25 (-1+x125V9),0]-If[x126V9>0,500+50 (-1+x126V9),0]-If[x127V9>0,100+10 (-1+x127V9),0]-If[x129V9>0,60+10 (-1+x129V9),0]-If[x12V9>0,800+100 (-1+x12V9),0]-If[x132V9>0,250+25 (-1+x132V9),0]-If[x134V9>0,800+100 (-1+x134V9),0]-If[x139V9>0,400+100 (-1+x139V9),0]-If[x140V9>0,800+100 (-1+x140V9),0]-If[x142V9>0,40+5 (-1+x142V9),0]-If[x143V9>0,200+50 (-1+x143V9),0]-If[x151V9>0,200+50 (-1+x151V9),0]-If[x153V9>0,30+5 (-1+x153V9),0]-If[x154V9>0,100+10 (-1+x154V9),0]-If[x156V9>0,60+10 (-1+x156V9),0]-If[x157V9>0,100+10 (-1+x157V9),0]-If[x160V9>0,400+50 (-1+x160V9),0]-If[x161V9>0,50+5 (-1+x161V9),0]-If[x162V9>0,250+25 (-1+x162V9),0]-If[x164V9>0,60+10 (-1+x164V9),0]-If[x167V9>0,200+50 (-1+x167V9),0]-If[x168V9>0,200+25 (-1+x168V9),0]-If[x170V9>0,30+5 (-1+x170V9),0]-If[x172V9>0,1000+100 (-1+x172V9),0]-If[x179V9>0,40+5 (-1+x179V9),0]-If[x17V9>0,150+25 (-1+x17V9),0]-If[x180V9>0,20+5 (-1+x180V9),0]-If[x182V9>0,800+100 (-1+x182V9),0]-If[x183V9>0,200+50 (-1+x183V9),0]-If[x184V9>0,300+50 (-1+x184V9),0]-If[x186V9>0,80+10 (-1+x186V9),0]-If[x187V9>0,400+100 (-1+x187V9),0]-If[x188V9>0,80+10 (-1+x188V9),0]-If[x191V9>0,50+5 (-1+x191V9),0]-If[x192V9>0,40+10 (-1+x192V9),0]-If[x193V9>0,50+5 (-1+x193V9),0]-If[x195V9>0,500+50 (-1+x195V9),0]-If[x198V9>0,800+100 (-1+x198V9),0]-If[x199V9>0,100+10 (-1+x199V9),0]-If[x19V9>0,1000+100 (-1+x19V9),0]-If[x24V9>0,200+50 (-1+x24V9),0]-If[x25V9>0,600+100 (-1+x25V9),0]-If[x26V9>0,20+5 (-1+x26V9),0]-If[x28V9>0,50+5 (-1+x28V9),0]-If[x29V9>0,600+100 (-1+x29V9),0]-If[x2V9>0,30+5 (-1+x2V9),0]-If[x32V9>0,400+50 (-1+x32V9),0]-If[x36V9>0,40+5 (-1+x36V9),0]-If[x37V9>0,60+10 (-1+x37V9),0]-If[x38V9>0,200+25 (-1+x38V9),0]-If[x39V9>0,80+10 (-1+x39V9),0]-If[x40V9>0,1000+100 (-1+x40V9),0]-If[x41V9>0,40+10 (-1+x41V9),0]-If[x42V9>0,200+25 (-1+x42V9),0]-If[x49V9>0,100+25 (-1+x49V9),0]-If[x4V9>0,30+5 (-1+x4V9),0]-If[x50V9>0,80+10 (-1+x50V9),0]-If[x51V9>0,400+100 (-1+x51V9),0]-If[x53V9>0,150+25 (-1+x53V9),0]-If[x55V9>0,80+10 (-1+x55V9),0]-If[x57V9>0,800+100 (-1+x57V9),0]-If[x58V9>0,100+10 (-1+x58V9),0]-If[x5V9>0,80+10 (-1+x5V9),0]-If[x60V9>0,40+10 (-1+x60V9),0]-If[x61V9>0,250+25 (-1+x61V9),0]-If[x63V9>0,30+5 (-1+x63V9),0]-If[x65V9>0,500+50 (-1+x65V9),0]-If[x66V9>0,500+50 (-1+x66V9),0]-If[x68V9>0,150+25 (-1+x68V9),0]-If[x69V9>0,40+5 (-1+x69V9),0]-If[x71V9>0,40+5 (-1+x71V9),0]-If[x73V9>0,300+50 (-1+x73V9),0]-If[x75V9>0,20+5 (-1+x75V9),0]-If[x76V9>0,1000+100 (-1+x76V9),0]-If[x80V9>0,100+10 (-1+x80V9),0]-If[x81V9>0,150+25 (-1+x81V9),0]-If[x82V9>0,250+25 (-1+x82V9),0]-If[x83V9>0,300+50 (-1+x83V9),0]-If[x85V9>0,400+100 (-1+x85V9),0]-If[x86V9>0,30+5 (-1+x86V9),0]-If[x87V9>0,200+25 (-1+x87V9),0]-If[x88V9>0,100+25 (-1+x88V9),0]-If[x90V9>0,400+100 (-1+x90V9),0]-If[x94V9>0,400+50 (-1+x94V9),0]-If[x95V9>0,200+25 (-1+x95V9),0]-If[x96V9>0,20+5 (-1+x96V9),0]-If[x97V9>0,100+25 (-1+x97V9),0]-If[x98V9>0,50+5 (-1+x98V9),0]-If[x99V9>0,600+100 (-1+x99V9),0]-If[x9V9>0,200+50 (-1+x9V9),0]>=0&&97419-If[x100V10>0,1000+100 (-1+x100V10),0]-If[x103V10>0,40+5 (-1+x103V10),0]-If[x106V10>0,100+25 (-1+x106V10),0]-If[x10V10>0,30+5 (-1+x10V10),0]-If[x110V10>0,600+100 (-1+x110V10),0]-If[x113V10>0,1000+100 (-1+x113V10),0]-If[x114V10>0,500+50 (-1+x114V10),0]-If[x116V10>0,200+50 (-1+x116V10),0]-If[x117V10>0,100+25 (-1+x117V10),0]-If[x118V10>0,60+10 (-1+x118V10),0]-If[x11V10>0,250+25 (-1+x11V10),0]-If[x122V10>0,400+50 (-1+x122V10),0]-If[x123V10>0,800+100 (-1+x123V10),0]-If[x125V10>0,60+10 (-1+x125V10),0]-If[x127V10>0,200+25 (-1+x127V10),0]-If[x129V10>0,200+25 (-1+x129V10),0]-If[x12V10>0,20+5 (-1+x12V10),0]-If[x130V10>0,800+100 (-1+x130V10),0]-If[x133V10>0,30+5 (-1+x133V10),0]-If[x134V10>0,500+50 (-1+x134V10),0]-If[x13V10>0,600+100 (-1+x13V10),0]-If[x141V10>0,100+10 (-1+x141V10),0]-If[x145V10>0,30+5 (-1+x145V10),0]-If[x147V10>0,200+50 (-1+x147V10),0]-If[x14V10>0,50+5 (-1+x14V10),0]-If[x151V10>0,100+25 (-1+x151V10),0]-If[x152V10>0,150+25 (-1+x152V10),0]-If[x154V10>0,40+5 (-1+x154V10),0]-If[x155V10>0,300+50 (-1+x155V10),0]-If[x156V10>0,30+5 (-1+x156V10),0]-If[x158V10>0,30+5 (-1+x158V10),0]-If[x159V10>0,400+50 (-1+x159V10),0]-If[x162V10>0,600+100 (-1+x162V10),0]-If[x163V10>0,1000+100 (-1+x163V10),0]-If[x164V10>0,30+5 (-1+x164V10),0]-If[x168V10>0,400+50 (-1+x168V10),0]-If[x173V10>0,200+50 (-1+x173V10),0]-If[x174V10>0,300+50 (-1+x174V10),0]-If[x175V10>0,40+5 (-1+x175V10),0]-If[x178V10>0,1000+100 (-1+x178V10),0]-If[x17V10>0,400+100 (-1+x17V10),0]-If[x181V10>0,800+100 (-1+x181V10),0]-If[x182V10>0,30+5 (-1+x182V10),0]-If[x184V10>0,50+5 (-1+x184V10),0]-If[x186V10>0,400+50 (-1+x186V10),0]-If[x187V10>0,100+25 (-1+x187V10),0]-If[x188V10>0,250+25 (-1+x188V10),0]-If[x189V10>0,300+50 (-1+x189V10),0]-If[x18V10>0,40+5 (-1+x18V10),0]-If[x193V10>0,400+100 (-1+x193V10),0]-If[x194V10>0,1000+100 (-1+x194V10),0]-If[x195V10>0,60+10 (-1+x195V10),0]-If[x196V10>0,100+10 (-1+x196V10),0]-If[x197V10>0,400+100 (-1+x197V10),0]-If[x198V10>0,50+5 (-1+x198V10),0]-If[x199V10>0,80+10 (-1+x199V10),0]-If[x19V10>0,100+25 (-1+x19V10),0]-If[x200V10>0,250+25 (-1+x200V10),0]-If[x20V10>0,20+5 (-1+x20V10),0]-If[x22V10>0,50+5 (-1+x22V10),0]-If[x24V10>0,500+50 (-1+x24V10),0]-If[x2V10>0,400+50 (-1+x2V10),0]-If[x31V10>0,600+100 (-1+x31V10),0]-If[x32V10>0,20+5 (-1+x32V10),0]-If[x33V10>0,40+5 (-1+x33V10),0]-If[x34V10>0,40+5 (-1+x34V10),0]-If[x41V10>0,20+5 (-1+x41V10),0]-If[x42V10>0,100+25 (-1+x42V10),0]-If[x43V10>0,300+50 (-1+x43V10),0]-If[x44V10>0,500+50 (-1+x44V10),0]-If[x45V10>0,400+50 (-1+x45V10),0]-If[x47V10>0,200+50 (-1+x47V10),0]-If[x4V10>0,50+5 (-1+x4V10),0]-If[x53V10>0,40+10 (-1+x53V10),0]-If[x54V10>0,80+10 (-1+x54V10),0]-If[x55V10>0,400+50 (-1+x55V10),0]-If[x57V10>0,800+100 (-1+x57V10),0]-If[x58V10>0,1000+100 (-1+x58V10),0]-If[x60V10>0,40+5 (-1+x60V10),0]-If[x65V10>0,800+100 (-1+x65V10),0]-If[x6V10>0,300+50 (-1+x6V10),0]-If[x70V10>0,50+5 (-1+x70V10),0]-If[x75V10>0,20+5 (-1+x75V10),0]-If[x76V10>0,250+25 (-1+x76V10),0]-If[x77V10>0,30+5 (-1+x77V10),0]-If[x7V10>0,40+10 (-1+x7V10),0]-If[x82V10>0,80+10 (-1+x82V10),0]-If[x84V10>0,400+50 (-1+x84V10),0]-If[x86V10>0,100+25 (-1+x86V10),0]-If[x88V10>0,600+100 (-1+x88V10),0]-If[x8V10>0,50+5 (-1+x8V10),0]-If[x92V10>0,200+25 (-1+x92V10),0]-If[x94V10>0,30+5 (-1+x94V10),0]-If[x95V10>0,30+5 (-1+x95V10),0]-If[x96V10>0,200+25 (-1+x96V10),0]-If[x98V10>0,250+25 (-1+x98V10),0]-If[x9V10>0,600+100 (-1+x9V10),0]>=0
generateContractCost
generateContractCost[1] = (-193+If[x1V5>0,200+50 (-1+x1V5),0]+If[x1V6>0,20+5 (-1+x1V6),0]+If[x1V7>0,80+10 (-1+x1V7),0]) If[193-If[x1V5>0,200+50 (-1+x1V5),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x1V7>0,80+10 (-1+x1V7),0]>0,148,0]
generatePositionChange
{0,0,0,0,-If[x1V5>0,200+50 (-1+x1V5),0],-If[x1V6>0,20+5 (-1+x1V6),0],-If[x1V7>0,80+10 (-1+x1V7),0],0,0,0,0,0,0,0,0,0,0,0,0,0}
generateMaxFunc
40474889+37 (55197-If[x101V5>0,600+100 (-1+x101V5),0]-If[x107V5>0,30+5 (-1+x107V5),0]-If[x108V5>0,60+10 (-1+x108V5),0]-If[x10V5>0,50+5 (-1+x10V5),0]-If[x112V5>0,100+25 (-1+x112V5),0]-If[x113V5>0,40+5 (-1+x113V5),0]-If[x116V5>0,40+10 (-1+x116V5),0]-If[x121V5>0,80+10 (-1+x121V5),0]-If[x122V5>0,40+10 (-1+x122V5),0]-If[x124V5>0,1000+100 (-1+x124V5),0]-If[x125V5>0,600+100 (-1+x125V5),0]-If[x127V5>0,100+10 (-1+x127V5),0]-If[x129V5>0,600+100 (-1+x129V5),0]-If[x12V5>0,40+10 (-1+x12V5),0]-If[x131V5>0,200+25 (-1+x131V5),0]-If[x132V5>0,1000+100 (-1+x132V5),0]-If[x133V5>0,250+25 (-1+x133V5),0]-If[x134V5>0,20+5 (-1+x134V5),0]-If[x136V5>0,30+5 (-1+x136V5),0]-If[x138V5>0,100+25 (-1+x138V5),0]-If[x139V5>0,200+50 (-1+x139V5),0]-If[x140V5>0,800+100 (-1+x140V5),0]-If[x141V5>0,100+25 (-1+x141V5),0]-If[x145V5>0,40+5 (-1+x145V5),0]-If[x146V5>0,150+25 (-1+x146V5),0]-If[x147V5>0,20+5 (-1+x147V5),0]-If[x148V5>0,60+10 (-1+x148V5),0]-If[x14V5>0,400+50 (-1+x14V5),0]-If[x150V5>0,400+100 (-1+x150V5),0]-If[x151V5>0,80+10 (-1+x151V5),0]-If[x152V5>0,800+100 (-1+x152V5),0]-If[x154V5>0,250+25 (-1+x154V5),0]-If[x155V5>0,20+5 (-1+x155V5),0]-If[x157V5>0,200+25 (-1+x157V5),0]-If[x158V5>0,250+25 (-1+x158V5),0]-If[x15V5>0,250+25 (-1+x15V5),0]-If[x161V5>0,1000+100 (-1+x161V5),0]-If[x164V5>0,250+25 (-1+x164V5),0]-If[x168V5>0,500+50 (-1+x168V5),0]-If[x171V5>0,40+10 (-1+x171V5),0]-If[x175V5>0,200+25 (-1+x175V5),0]-If[x178V5>0,300+50 (-1+x178V5),0]-If[x17V5>0,250+25 (-1+x17V5),0]-If[x181V5>0,250+25 (-1+x181V5),0]-If[x184V5>0,200+50 (-1+x184V5),0]-If[x188V5>0,80+10 (-1+x188V5),0]-If[x18V5>0,400+100 (-1+x18V5),0]-If[x192V5>0,50+5 (-1+x192V5),0]-If[x193V5>0,100+10 (-1+x193V5),0]-If[x195V5>0,300+50 (-1+x195V5),0]-If[x196V5>0,150+25 (-1+x196V5),0]-If[x198V5>0,30+5 (-1+x198V5),0]-If[x199V5>0,400+50 (-1+x199V5),0]-If[x19V5>0,800+100 (-1+x19V5),0]-If[x1V5>0,200+50 (-1+x1V5),0]-If[x200V5>0,400+100 (-1+x200V5),0]-If[x21V5>0,100+25 (-1+x21V5),0]-If[x23V5>0,300+50 (-1+x23V5),0]-If[x24V5>0,200+50 (-1+x24V5),0]-If[x27V5>0,250+25 (-1+x27V5),0]-If[x29V5>0,600+100 (-1+x29V5),0]-If[x2V5>0,60+10 (-1+x2V5),0]-If[x31V5>0,500+50 (-1+x31V5),0]-If[x41V5>0,100+10 (-1+x41V5),0]-If[x43V5>0,40+10 (-1+x43V5),0]-If[x46V5>0,200+50 (-1+x46V5),0]-If[x47V5>0,500+50 (-1+x47V5),0]-If[x49V5>0,300+50 (-1+x49V5),0]-If[x57V5>0,150+25 (-1+x57V5),0]-If[x58V5>0,400+50 (-1+x58V5),0]-If[x59V5>0,20+5 (-1+x59V5),0]-If[x61V5>0,800+100 (-1+x61V5),0]-If[x63V5>0,150+25 (-1+x63V5),0]-If[x66V5>0,150+25 (-1+x66V5),0]-If[x68V5>0,30+5 (-1+x68V5),0]-If[x69V5>0,250+25 (-1+x69V5),0]-If[x75V5>0,400+50 (-1+x75V5),0]-If[x78V5>0,40+10 (-1+x78V5),0]-If[x80V5>0,800+100 (-1+x80V5),0]-If[x84V5>0,200+50 (-1+x84V5),0]-If[x88V5>0,20+5 (-1+x88V5),0]-If[x90V5>0,150+25 (-1+x90V5),0]-If[x93V5>0,1000+100 (-1+x93V5),0]-If[x94V5>0,200+25 (-1+x94V5),0]-If[x95V5>0,400+100 (-1+x95V5),0]-If[x96V5>0,40+10 (-1+x96V5),0]-If[x98V5>0,40+10 (-1+x98V5),0])+21 (135474-If[x101V7>0,60+10 (-1+x101V7),0]-If[x102V7>0,30+5 (-1+x102V7),0]-If[x105V7>0,600+100 (-1+x105V7),0]-If[x107V7>0,500+50 (-1+x107V7),0]-If[x109V7>0,200+25 (-1+x109V7),0]-If[x10V7>0,20+5 (-1+x10V7),0]-If[x110V7>0,250+25 (-1+x110V7),0]-If[x111V7>0,500+50 (-1+x111V7),0]-If[x112V7>0,20+5 (-1+x112V7),0]-If[x113V7>0,400+50 (-1+x113V7),0]-If[x114V7>0,250+25 (-1+x114V7),0]-If[x115V7>0,1000+100 (-1+x115V7),0]-If[x117V7>0,400+50 (-1+x117V7),0]-If[x118V7>0,30+5 (-1+x118V7),0]-If[x120V7>0,800+100 (-1+x120V7),0]-If[x122V7>0,40+5 (-1+x122V7),0]-If[x124V7>0,800+100 (-1+x124V7),0]-If[x125V7>0,40+10 (-1+x125V7),0]-If[x127V7>0,250+25 (-1+x127V7),0]-If[x129V7>0,300+50 (-1+x129V7),0]-If[x131V7>0,500+50 (-1+x131V7),0]-If[x132V7>0,30+5 (-1+x132V7),0]-If[x134V7>0,50+5 (-1+x134V7),0]-If[x135V7>0,100+25 (-1+x135V7),0]-If[x136V7>0,30+5 (-1+x136V7),0]-If[x138V7>0,30+5 (-1+x138V7),0]-If[x13V7>0,60+10 (-1+x13V7),0]-If[x140V7>0,100+25 (-1+x140V7),0]-If[x141V7>0,100+25 (-1+x141V7),0]-If[x144V7>0,400+50 (-1+x144V7),0]-If[x145V7>0,200+25 (-1+x145V7),0]-If[x148V7>0,100+10 (-1+x148V7),0]-If[x14V7>0,250+25 (-1+x14V7),0]-If[x150V7>0,50+5 (-1+x150V7),0]-If[x151V7>0,30+5 (-1+x151V7),0]-If[x152V7>0,300+50 (-1+x152V7),0]-If[x153V7>0,40+10 (-1+x153V7),0]-If[x154V7>0,800+100 (-1+x154V7),0]-If[x155V7>0,250+25 (-1+x155V7),0]-If[x156V7>0,150+25 (-1+x156V7),0]-If[x159V7>0,200+25 (-1+x159V7),0]-If[x162V7>0,20+5 (-1+x162V7),0]-If[x163V7>0,80+10 (-1+x163V7),0]-If[x165V7>0,150+25 (-1+x165V7),0]-If[x169V7>0,800+100 (-1+x169V7),0]-If[x171V7>0,40+10 (-1+x171V7),0]-If[x174V7>0,100+25 (-1+x174V7),0]-If[x177V7>0,50+5 (-1+x177V7),0]-If[x178V7>0,50+5 (-1+x178V7),0]-If[x179V7>0,100+10 (-1+x179V7),0]-If[x180V7>0,250+25 (-1+x180V7),0]-If[x187V7>0,30+5 (-1+x187V7),0]-If[x188V7>0,40+10 (-1+x188V7),0]-If[x18V7>0,200+50 (-1+x18V7),0]-If[x192V7>0,200+25 (-1+x192V7),0]-If[x193V7>0,60+10 (-1+x193V7),0]-If[x194V7>0,500+50 (-1+x194V7),0]-If[x196V7>0,1000+100 (-1+x196V7),0]-If[x197V7>0,1000+100 (-1+x197V7),0]-If[x199V7>0,100+10 (-1+x199V7),0]-If[x1V7>0,80+10 (-1+x1V7),0]-If[x21V7>0,300+50 (-1+x21V7),0]-If[x24V7>0,300+50 (-1+x24V7),0]-If[x25V7>0,50+5 (-1+x25V7),0]-If[x26V7>0,400+50 (-1+x26V7),0]-If[x28V7>0,50+5 (-1+x28V7),0]-If[x2V7>0,500+50 (-1+x2V7),0]-If[x31V7>0,150+25 (-1+x31V7),0]-If[x33V7>0,40+5 (-1+x33V7),0]-If[x34V7>0,80+10 (-1+x34V7),0]-If[x41V7>0,600+100 (-1+x41V7),0]-If[x42V7>0,80+10 (-1+x42V7),0]-If[x44V7>0,300+50 (-1+x44V7),0]-If[x48V7>0,200+25 (-1+x48V7),0]-If[x53V7>0,400+100 (-1+x53V7),0]-If[x55V7>0,500+50 (-1+x55V7),0]-If[x58V7>0,100+10 (-1+x58V7),0]-If[x59V7>0,100+25 (-1+x59V7),0]-If[x5V7>0,150+25 (-1+x5V7),0]-If[x60V7>0,20+5 (-1+x60V7),0]-If[x61V7>0,100+10 (-1+x61V7),0]-If[x62V7>0,200+25 (-1+x62V7),0]-If[x63V7>0,40+10 (-1+x63V7),0]-If[x68V7>0,500+50 (-1+x68V7),0]-If[x69V7>0,300+50 (-1+x69V7),0]-If[x70V7>0,50+5 (-1+x70V7),0]-If[x71V7>0,1000+100 (-1+x71V7),0]-If[x72V7>0,400+100 (-1+x72V7),0]-If[x75V7>0,400+50 (-1+x75V7),0]-If[x76V7>0,40+5 (-1+x76V7),0]-If[x79V7>0,150+25 (-1+x79V7),0]-If[x82V7>0,40+10 (-1+x82V7),0]-If[x84V7>0,60+10 (-1+x84V7),0]-If[x85V7>0,30+5 (-1+x85V7),0]-If[x86V7>0,100+10 (-1+x86V7),0]-If[x87V7>0,200+25 (-1+x87V7),0]-If[x88V7>0,200+25 (-1+x88V7),0]-If[x8V7>0,150+25 (-1+x8V7),0]-If[x90V7>0,500+50 (-1+x90V7),0]-If[x92V7>0,250+25 (-1+x92V7),0]-If[x93V7>0,40+10 (-1+x93V7),0]-If[x94V7>0,500+50 (-1+x94V7),0]-If[x96V7>0,250+25 (-1+x96V7),0]-If[x98V7>0,150+25 (-1+x98V7),0])+30 (85057-If[x100V3>0,600+100 (-1+x100V3),0]-If[x101V3>0,400+100 (-1+x101V3),0]-If[x102V3>0,150+25 (-1+x102V3),0]-If[x103V3>0,600+100 (-1+x103V3),0]-If[x106V3>0,40+5 (-1+x106V3),0]-If[x111V3>0,80+10 (-1+x111V3),0]-If[x112V3>0,60+10 (-1+x112V3),0]-If[x114V3>0,600+100 (-1+x114V3),0]-If[x117V3>0,200+25 (-1+x117V3),0]-If[x11V3>0,300+50 (-1+x11V3),0]-If[x12V3>0,40+10 (-1+x12V3),0]-If[x131V3>0,80+10 (-1+x131V3),0]-If[x132V3>0,20+5 (-1+x132V3),0]-If[x134V3>0,200+25 (-1+x134V3),0]-If[x135V3>0,40+10 (-1+x135V3),0]-If[x136V3>0,150+25 (-1+x136V3),0]-If[x137V3>0,100+25 (-1+x137V3),0]-If[x138V3>0,1000+100 (-1+x138V3),0]-If[x139V3>0,20+5 (-1+x139V3),0]-If[x13V3>0,200+50 (-1+x13V3),0]-If[x140V3>0,40+5 (-1+x140V3),0]-If[x142V3>0,100+10 (-1+x142V3),0]-If[x146V3>0,400+100 (-1+x146V3),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x155V3>0,400+50 (-1+x155V3),0]-If[x156V3>0,40+10 (-1+x156V3),0]-If[x157V3>0,300+50 (-1+x157V3),0]-If[x160V3>0,200+25 (-1+x160V3),0]-If[x161V3>0,200+25 (-1+x161V3),0]-If[x162V3>0,500+50 (-1+x162V3),0]-If[x164V3>0,30+5 (-1+x164V3),0]-If[x165V3>0,100+10 (-1+x165V3),0]-If[x166V3>0,600+100 (-1+x166V3),0]-If[x168V3>0,800+100 (-1+x168V3),0]-If[x16V3>0,200+25 (-1+x16V3),0]-If[x171V3>0,150+25 (-1+x171V3),0]-If[x173V3>0,400+50 (-1+x173V3),0]-If[x174V3>0,40+10 (-1+x174V3),0]-If[x175V3>0,150+25 (-1+x175V3),0]-If[x176V3>0,80+10 (-1+x176V3),0]-If[x178V3>0,80+10 (-1+x178V3),0]-If[x17V3>0,30+5 (-1+x17V3),0]-If[x186V3>0,400+100 (-1+x186V3),0]-If[x187V3>0,1000+100 (-1+x187V3),0]-If[x18V3>0,60+10 (-1+x18V3),0]-If[x190V3>0,40+5 (-1+x190V3),0]-If[x192V3>0,1000+100 (-1+x192V3),0]-If[x194V3>0,80+10 (-1+x194V3),0]-If[x196V3>0,400+100 (-1+x196V3),0]-If[x19V3>0,600+100 (-1+x19V3),0]-If[x200V3>0,100+10 (-1+x200V3),0]-If[x20V3>0,100+25 (-1+x20V3),0]-If[x21V3>0,600+100 (-1+x21V3),0]-If[x22V3>0,150+25 (-1+x22V3),0]-If[x24V3>0,150+25 (-1+x24V3),0]-If[x29V3>0,500+50 (-1+x29V3),0]-If[x31V3>0,40+5 (-1+x31V3),0]-If[x32V3>0,1000+100 (-1+x32V3),0]-If[x35V3>0,200+50 (-1+x35V3),0]-If[x36V3>0,200+50 (-1+x36V3),0]-If[x37V3>0,30+5 (-1+x37V3),0]-If[x3V3>0,400+100 (-1+x3V3),0]-If[x41V3>0,300+50 (-1+x41V3),0]-If[x42V3>0,500+50 (-1+x42V3),0]-If[x43V3>0,150+25 (-1+x43V3),0]-If[x45V3>0,100+10 (-1+x45V3),0]-If[x46V3>0,60+10 (-1+x46V3),0]-If[x47V3>0,50+5 (-1+x47V3),0]-If[x48V3>0,200+50 (-1+x48V3),0]-If[x49V3>0,800+100 (-1+x49V3),0]-If[x50V3>0,20+5 (-1+x50V3),0]-If[x52V3>0,50+5 (-1+x52V3),0]-If[x56V3>0,250+25 (-1+x56V3),0]-If[x58V3>0,500+50 (-1+x58V3),0]-If[x63V3>0,100+25 (-1+x63V3),0]-If[x65V3>0,300+50 (-1+x65V3),0]-If[x66V3>0,250+25 (-1+x66V3),0]-If[x70V3>0,60+10 (-1+x70V3),0]-If[x71V3>0,250+25 (-1+x71V3),0]-If[x74V3>0,500+50 (-1+x74V3),0]-If[x76V3>0,100+25 (-1+x76V3),0]-If[x77V3>0,1000+100 (-1+x77V3),0]-If[x78V3>0,80+10 (-1+x78V3),0]-If[x81V3>0,100+25 (-1+x81V3),0]-If[x85V3>0,200+50 (-1+x85V3),0]-If[x8V3>0,50+5 (-1+x8V3),0]-If[x90V3>0,150+25 (-1+x90V3),0]-If[x94V3>0,250+25 (-1+x94V3),0]-If[x95V3>0,300+50 (-1+x95V3),0]-If[x96V3>0,300+50 (-1+x96V3),0]-If[x97V3>0,200+25 (-1+x97V3),0]-If[x98V3>0,60+10 (-1+x98V3),0]-If[x99V3>0,40+10 (-1+x99V3),0])+39 (31699-If[x100V1>0,30+5 (-1+x100V1),0]-If[x102V1>0,40+5 (-1+x102V1),0]-If[x103V1>0,40+5 (-1+x103V1),0]-If[x107V1>0,400+100 (-1+x107V1),0]-If[x109V1>0,100+10 (-1+x109V1),0]-If[x111V1>0,100+10 (-1+x111V1),0]-If[x113V1>0,100+25 (-1+x113V1),0]-If[x114V1>0,100+10 (-1+x114V1),0]-If[x115V1>0,300+50 (-1+x115V1),0]-If[x116V1>0,40+5 (-1+x116V1),0]-If[x117V1>0,1000+100 (-1+x117V1),0]-If[x118V1>0,100+25 (-1+x118V1),0]-If[x119V1>0,600+100 (-1+x119V1),0]-If[x11V1>0,100+10 (-1+x11V1),0]-If[x123V1>0,50+5 (-1+x123V1),0]-If[x124V1>0,1000+100 (-1+x124V1),0]-If[x126V1>0,400+50 (-1+x126V1),0]-If[x127V1>0,1000+100 (-1+x127V1),0]-If[x128V1>0,150+25 (-1+x128V1),0]-If[x12V1>0,600+100 (-1+x12V1),0]-If[x130V1>0,20+5 (-1+x130V1),0]-If[x131V1>0,30+5 (-1+x131V1),0]-If[x132V1>0,800+100 (-1+x132V1),0]-If[x133V1>0,200+25 (-1+x133V1),0]-If[x134V1>0,50+5 (-1+x134V1),0]-If[x135V1>0,1000+100 (-1+x135V1),0]-If[x139V1>0,50+5 (-1+x139V1),0]-If[x13V1>0,100+10 (-1+x13V1),0]-If[x140V1>0,40+5 (-1+x140V1),0]-If[x142V1>0,800+100 (-1+x142V1),0]-If[x144V1>0,250+25 (-1+x144V1),0]-If[x147V1>0,40+5 (-1+x147V1),0]-If[x14V1>0,40+5 (-1+x14V1),0]-If[x151V1>0,800+100 (-1+x151V1),0]-If[x156V1>0,100+10 (-1+x156V1),0]-If[x157V1>0,600+100 (-1+x157V1),0]-If[x158V1>0,60+10 (-1+x158V1),0]-If[x15V1>0,50+5 (-1+x15V1),0]-If[x162V1>0,1000+100 (-1+x162V1),0]-If[x163V1>0,300+50 (-1+x163V1),0]-If[x164V1>0,150+25 (-1+x164V1),0]-If[x166V1>0,200+25 (-1+x166V1),0]-If[x16V1>0,150+25 (-1+x16V1),0]-If[x172V1>0,40+10 (-1+x172V1),0]-If[x174V1>0,250+25 (-1+x174V1),0]-If[x178V1>0,30+5 (-1+x178V1),0]-If[x181V1>0,1000+100 (-1+x181V1),0]-If[x182V1>0,200+50 (-1+x182V1),0]-If[x183V1>0,600+100 (-1+x183V1),0]-If[x184V1>0,100+25 (-1+x184V1),0]-If[x185V1>0,400+100 (-1+x185V1),0]-If[x186V1>0,150+25 (-1+x186V1),0]-If[x189V1>0,400+50 (-1+x189V1),0]-If[x18V1>0,400+100 (-1+x18V1),0]-If[x190V1>0,40+10 (-1+x190V1),0]-If[x191V1>0,400+50 (-1+x191V1),0]-If[x192V1>0,100+10 (-1+x192V1),0]-If[x193V1>0,200+25 (-1+x193V1),0]-If[x194V1>0,250+25 (-1+x194V1),0]-If[x198V1>0,40+10 (-1+x198V1),0]-If[x199V1>0,1000+100 (-1+x199V1),0]-If[x20V1>0,600+100 (-1+x20V1),0]-If[x22V1>0,50+5 (-1+x22V1),0]-If[x24V1>0,500+50 (-1+x24V1),0]-If[x26V1>0,50+5 (-1+x26V1),0]-If[x28V1>0,500+50 (-1+x28V1),0]-If[x29V1>0,200+50 (-1+x29V1),0]-If[x30V1>0,1000+100 (-1+x30V1),0]-If[x31V1>0,1000+100 (-1+x31V1),0]-If[x32V1>0,400+100 (-1+x32V1),0]-If[x36V1>0,500+50 (-1+x36V1),0]-If[x3V1>0,20+5 (-1+x3V1),0]-If[x40V1>0,30+5 (-1+x40V1),0]-If[x41V1>0,200+25 (-1+x41V1),0]-If[x42V1>0,1000+100 (-1+x42V1),0]-If[x43V1>0,100+25 (-1+x43V1),0]-If[x45V1>0,600+100 (-1+x45V1),0]-If[x46V1>0,250+25 (-1+x46V1),0]-If[x47V1>0,1000+100 (-1+x47V1),0]-If[x4V1>0,500+50 (-1+x4V1),0]-If[x50V1>0,150+25 (-1+x50V1),0]-If[x51V1>0,100+10 (-1+x51V1),0]-If[x53V1>0,500+50 (-1+x53V1),0]-If[x55V1>0,40+10 (-1+x55V1),0]-If[x57V1>0,500+50 (-1+x57V1),0]-If[x58V1>0,250+25 (-1+x58V1),0]-If[x5V1>0,600+100 (-1+x5V1),0]-If[x65V1>0,400+50 (-1+x65V1),0]-If[x68V1>0,150+25 (-1+x68V1),0]-If[x69V1>0,50+5 (-1+x69V1),0]-If[x6V1>0,50+5 (-1+x6V1),0]-If[x71V1>0,30+5 (-1+x71V1),0]-If[x72V1>0,800+100 (-1+x72V1),0]-If[x73V1>0,400+50 (-1+x73V1),0]-If[x75V1>0,500+50 (-1+x75V1),0]-If[x78V1>0,20+5 (-1+x78V1),0]-If[x80V1>0,60+10 (-1+x80V1),0]-If[x81V1>0,20+5 (-1+x81V1),0]-If[x82V1>0,200+50 (-1+x82V1),0]-If[x83V1>0,200+50 (-1+x83V1),0]-If[x85V1>0,200+50 (-1+x85V1),0]-If[x86V1>0,50+5 (-1+x86V1),0]-If[x87V1>0,300+50 (-1+x87V1),0]-If[x88V1>0,100+10 (-1+x88V1),0]-If[x90V1>0,60+10 (-1+x90V1),0]-If[x93V1>0,200+25 (-1+x93V1),0]-If[x95V1>0,200+25 (-1+x95V1),0]-If[x97V1>0,30+5 (-1+x97V1),0]-If[x98V1>0,400+100 (-1+x98V1),0]-If[x9V1>0,400+100 (-1+x9V1),0])+18 (97419-If[x100V10>0,1000+100 (-1+x100V10),0]-If[x103V10>0,40+5 (-1+x103V10),0]-If[x106V10>0,100+25 (-1+x106V10),0]-If[x10V10>0,30+5 (-1+x10V10),0]-If[x110V10>0,600+100 (-1+x110V10),0]-If[x113V10>0,1000+100 (-1+x113V10),0]-If[x114V10>0,500+50 (-1+x114V10),0]-If[x116V10>0,200+50 (-1+x116V10),0]-If[x117V10>0,100+25 (-1+x117V10),0]-If[x118V10>0,60+10 (-1+x118V10),0]-If[x11V10>0,250+25 (-1+x11V10),0]-If[x122V10>0,400+50 (-1+x122V10),0]-If[x123V10>0,800+100 (-1+x123V10),0]-If[x125V10>0,60+10 (-1+x125V10),0]-If[x127V10>0,200+25 (-1+x127V10),0]-If[x129V10>0,200+25 (-1+x129V10),0]-If[x12V10>0,20+5 (-1+x12V10),0]-If[x130V10>0,800+100 (-1+x130V10),0]-If[x133V10>0,30+5 (-1+x133V10),0]-If[x134V10>0,500+50 (-1+x134V10),0]-If[x13V10>0,600+100 (-1+x13V10),0]-If[x141V10>0,100+10 (-1+x141V10),0]-If[x145V10>0,30+5 (-1+x145V10),0]-If[x147V10>0,200+50 (-1+x147V10),0]-If[x14V10>0,50+5 (-1+x14V10),0]-If[x151V10>0,100+25 (-1+x151V10),0]-If[x152V10>0,150+25 (-1+x152V10),0]-If[x154V10>0,40+5 (-1+x154V10),0]-If[x155V10>0,300+50 (-1+x155V10),0]-If[x156V10>0,30+5 (-1+x156V10),0]-If[x158V10>0,30+5 (-1+x158V10),0]-If[x159V10>0,400+50 (-1+x159V10),0]-If[x162V10>0,600+100 (-1+x162V10),0]-If[x163V10>0,1000+100 (-1+x163V10),0]-If[x164V10>0,30+5 (-1+x164V10),0]-If[x168V10>0,400+50 (-1+x168V10),0]-If[x173V10>0,200+50 (-1+x173V10),0]-If[x174V10>0,300+50 (-1+x174V10),0]-If[x175V10>0,40+5 (-1+x175V10),0]-If[x178V10>0,1000+100 (-1+x178V10),0]-If[x17V10>0,400+100 (-1+x17V10),0]-If[x181V10>0,800+100 (-1+x181V10),0]-If[x182V10>0,30+5 (-1+x182V10),0]-If[x184V10>0,50+5 (-1+x184V10),0]-If[x186V10>0,400+50 (-1+x186V10),0]-If[x187V10>0,100+25 (-1+x187V10),0]-If[x188V10>0,250+25 (-1+x188V10),0]-If[x189V10>0,300+50 (-1+x189V10),0]-If[x18V10>0,40+5 (-1+x18V10),0]-If[x193V10>0,400+100 (-1+x193V10),0]-If[x194V10>0,1000+100 (-1+x194V10),0]-If[x195V10>0,60+10 (-1+x195V10),0]-If[x196V10>0,100+10 (-1+x196V10),0]-If[x197V10>0,400+100 (-1+x197V10),0]-If[x198V10>0,50+5 (-1+x198V10),0]-If[x199V10>0,80+10 (-1+x199V10),0]-If[x19V10>0,100+25 (-1+x19V10),0]-If[x200V10>0,250+25 (-1+x200V10),0]-If[x20V10>0,20+5 (-1+x20V10),0]-If[x22V10>0,50+5 (-1+x22V10),0]-If[x24V10>0,500+50 (-1+x24V10),0]-If[x2V10>0,400+50 (-1+x2V10),0]-If[x31V10>0,600+100 (-1+x31V10),0]-If[x32V10>0,20+5 (-1+x32V10),0]-If[x33V10>0,40+5 (-1+x33V10),0]-If[x34V10>0,40+5 (-1+x34V10),0]-If[x41V10>0,20+5 (-1+x41V10),0]-If[x42V10>0,100+25 (-1+x42V10),0]-If[x43V10>0,300+50 (-1+x43V10),0]-If[x44V10>0,500+50 (-1+x44V10),0]-If[x45V10>0,400+50 (-1+x45V10),0]-If[x47V10>0,200+50 (-1+x47V10),0]-If[x4V10>0,50+5 (-1+x4V10),0]-If[x53V10>0,40+10 (-1+x53V10),0]-If[x54V10>0,80+10 (-1+x54V10),0]-If[x55V10>0,400+50 (-1+x55V10),0]-If[x57V10>0,800+100 (-1+x57V10),0]-If[x58V10>0,1000+100 (-1+x58V10),0]-If[x60V10>0,40+5 (-1+x60V10),0]-If[x65V10>0,800+100 (-1+x65V10),0]-If[x6V10>0,300+50 (-1+x6V10),0]-If[x70V10>0,50+5 (-1+x70V10),0]-If[x75V10>0,20+5 (-1+x75V10),0]-If[x76V10>0,250+25 (-1+x76V10),0]-If[x77V10>0,30+5 (-1+x77V10),0]-If[x7V10>0,40+10 (-1+x7V10),0]-If[x82V10>0,80+10 (-1+x82V10),0]-If[x84V10>0,400+50 (-1+x84V10),0]-If[x86V10>0,100+25 (-1+x86V10),0]-If[x88V10>0,600+100 (-1+x88V10),0]-If[x8V10>0,50+5 (-1+x8V10),0]-If[x92V10>0,200+25 (-1+x92V10),0]-If[x94V10>0,30+5 (-1+x94V10),0]-If[x95V10>0,30+5 (-1+x95V10),0]-If[x96V10>0,200+25 (-1+x96V10),0]-If[x98V10>0,250+25 (-1+x98V10),0]-If[x9V10>0,600+100 (-1+x9V10),0])+38 (54687-If[x101V2>0,40+10 (-1+x101V2),0]-If[x103V2>0,60+10 (-1+x103V2),0]-If[x107V2>0,60+10 (-1+x107V2),0]-If[x108V2>0,800+100 (-1+x108V2),0]-If[x111V2>0,20+5 (-1+x111V2),0]-If[x114V2>0,800+100 (-1+x114V2),0]-If[x116V2>0,40+5 (-1+x116V2),0]-If[x11V2>0,20+5 (-1+x11V2),0]-If[x123V2>0,20+5 (-1+x123V2),0]-If[x125V2>0,30+5 (-1+x125V2),0]-If[x126V2>0,600+100 (-1+x126V2),0]-If[x127V2>0,40+5 (-1+x127V2),0]-If[x129V2>0,40+5 (-1+x129V2),0]-If[x131V2>0,40+5 (-1+x131V2),0]-If[x133V2>0,30+5 (-1+x133V2),0]-If[x134V2>0,50+5 (-1+x134V2),0]-If[x136V2>0,150+25 (-1+x136V2),0]-If[x137V2>0,600+100 (-1+x137V2),0]-If[x138V2>0,400+50 (-1+x138V2),0]-If[x141V2>0,80+10 (-1+x141V2),0]-If[x142V2>0,400+50 (-1+x142V2),0]-If[x143V2>0,20+5 (-1+x143V2),0]-If[x145V2>0,50+5 (-1+x145V2),0]-If[x148V2>0,150+25 (-1+x148V2),0]-If[x14V2>0,100+10 (-1+x14V2),0]-If[x151V2>0,300+50 (-1+x151V2),0]-If[x152V2>0,30+5 (-1+x152V2),0]-If[x154V2>0,800+100 (-1+x154V2),0]-If[x15V2>0,150+25 (-1+x15V2),0]-If[x160V2>0,50+5 (-1+x160V2),0]-If[x162V2>0,80+10 (-1+x162V2),0]-If[x165V2>0,100+10 (-1+x165V2),0]-If[x168V2>0,300+50 (-1+x168V2),0]-If[x169V2>0,800+100 (-1+x169V2),0]-If[x170V2>0,60+10 (-1+x170V2),0]-If[x172V2>0,100+25 (-1+x172V2),0]-If[x173V2>0,200+50 (-1+x173V2),0]-If[x174V2>0,30+5 (-1+x174V2),0]-If[x175V2>0,20+5 (-1+x175V2),0]-If[x177V2>0,800+100 (-1+x177V2),0]-If[x178V2>0,50+5 (-1+x178V2),0]-If[x17V2>0,400+100 (-1+x17V2),0]-If[x182V2>0,30+5 (-1+x182V2),0]-If[x183V2>0,400+100 (-1+x183V2),0]-If[x184V2>0,600+100 (-1+x184V2),0]-If[x185V2>0,30+5 (-1+x185V2),0]-If[x186V2>0,100+25 (-1+x186V2),0]-If[x189V2>0,60+10 (-1+x189V2),0]-If[x18V2>0,20+5 (-1+x18V2),0]-If[x190V2>0,50+5 (-1+x190V2),0]-If[x191V2>0,300+50 (-1+x191V2),0]-If[x195V2>0,50+5 (-1+x195V2),0]-If[x196V2>0,500+50 (-1+x196V2),0]-If[x198V2>0,50+5 (-1+x198V2),0]-If[x200V2>0,200+50 (-1+x200V2),0]-If[x20V2>0,30+5 (-1+x20V2),0]-If[x25V2>0,1000+100 (-1+x25V2),0]-If[x27V2>0,200+25 (-1+x27V2),0]-If[x29V2>0,150+25 (-1+x29V2),0]-If[x2V2>0,1000+100 (-1+x2V2),0]-If[x36V2>0,400+50 (-1+x36V2),0]-If[x37V2>0,20+5 (-1+x37V2),0]-If[x39V2>0,100+25 (-1+x39V2),0]-If[x44V2>0,400+50 (-1+x44V2),0]-If[x46V2>0,500+50 (-1+x46V2),0]-If[x48V2>0,300+50 (-1+x48V2),0]-If[x49V2>0,40+10 (-1+x49V2),0]-If[x4V2>0,400+50 (-1+x4V2),0]-If[x53V2>0,600+100 (-1+x53V2),0]-If[x55V2>0,40+10 (-1+x55V2),0]-If[x56V2>0,20+5 (-1+x56V2),0]-If[x58V2>0,800+100 (-1+x58V2),0]-If[x5V2>0,800+100 (-1+x5V2),0]-If[x60V2>0,250+25 (-1+x60V2),0]-If[x61V2>0,250+25 (-1+x61V2),0]-If[x62V2>0,300+50 (-1+x62V2),0]-If[x64V2>0,200+25 (-1+x64V2),0]-If[x66V2>0,600+100 (-1+x66V2),0]-If[x69V2>0,60+10 (-1+x69V2),0]-If[x70V2>0,40+5 (-1+x70V2),0]-If[x73V2>0,50+5 (-1+x73V2),0]-If[x74V2>0,40+5 (-1+x74V2),0]-If[x76V2>0,600+100 (-1+x76V2),0]-If[x78V2>0,80+10 (-1+x78V2),0]-If[x79V2>0,100+10 (-1+x79V2),0]-If[x7V2>0,100+10 (-1+x7V2),0]-If[x82V2>0,400+100 (-1+x82V2),0]-If[x83V2>0,50+5 (-1+x83V2),0]-If[x84V2>0,60+10 (-1+x84V2),0]-If[x86V2>0,300+50 (-1+x86V2),0]-If[x88V2>0,250+25 (-1+x88V2),0]-If[x89V2>0,800+100 (-1+x89V2),0]-If[x92V2>0,800+100 (-1+x92V2),0]-If[x93V2>0,300+50 (-1+x93V2),0]-If[x94V2>0,60+10 (-1+x94V2),0]-If[x97V2>0,50+5 (-1+x97V2),0]-If[x98V2>0,1000+100 (-1+x98V2),0]-If[x9V2>0,1000+100 (-1+x9V2),0])+3 (149097-If[x100V4>0,80+10 (-1+x100V4),0]-If[x102V4>0,100+10 (-1+x102V4),0]-If[x103V4>0,600+100 (-1+x103V4),0]-If[x106V4>0,100+10 (-1+x106V4),0]-If[x109V4>0,100+10 (-1+x109V4),0]-If[x111V4>0,600+100 (-1+x111V4),0]-If[x112V4>0,50+5 (-1+x112V4),0]-If[x113V4>0,20+5 (-1+x113V4),0]-If[x114V4>0,60+10 (-1+x114V4),0]-If[x115V4>0,1000+100 (-1+x115V4),0]-If[x116V4>0,60+10 (-1+x116V4),0]-If[x117V4>0,600+100 (-1+x117V4),0]-If[x11V4>0,300+50 (-1+x11V4),0]-If[x120V4>0,600+100 (-1+x120V4),0]-If[x123V4>0,30+5 (-1+x123V4),0]-If[x125V4>0,50+5 (-1+x125V4),0]-If[x126V4>0,300+50 (-1+x126V4),0]-If[x127V4>0,60+10 (-1+x127V4),0]-If[x128V4>0,400+50 (-1+x128V4),0]-If[x12V4>0,100+10 (-1+x12V4),0]-If[x131V4>0,40+10 (-1+x131V4),0]-If[x132V4>0,30+5 (-1+x132V4),0]-If[x133V4>0,100+25 (-1+x133V4),0]-If[x137V4>0,400+50 (-1+x137V4),0]-If[x138V4>0,200+50 (-1+x138V4),0]-If[x139V4>0,1000+100 (-1+x139V4),0]-If[x13V4>0,600+100 (-1+x13V4),0]-If[x140V4>0,80+10 (-1+x140V4),0]-If[x141V4>0,60+10 (-1+x141V4),0]-If[x142V4>0,100+10 (-1+x142V4),0]-If[x147V4>0,30+5 (-1+x147V4),0]-If[x148V4>0,250+25 (-1+x148V4),0]-If[x149V4>0,800+100 (-1+x149V4),0]-If[x14V4>0,50+5 (-1+x14V4),0]-If[x150V4>0,800+100 (-1+x150V4),0]-If[x154V4>0,800+100 (-1+x154V4),0]-If[x156V4>0,250+25 (-1+x156V4),0]-If[x157V4>0,200+50 (-1+x157V4),0]-If[x160V4>0,800+100 (-1+x160V4),0]-If[x162V4>0,100+10 (-1+x162V4),0]-If[x163V4>0,150+25 (-1+x163V4),0]-If[x164V4>0,200+25 (-1+x164V4),0]-If[x166V4>0,800+100 (-1+x166V4),0]-If[x169V4>0,600+100 (-1+x169V4),0]-If[x170V4>0,100+10 (-1+x170V4),0]-If[x172V4>0,80+10 (-1+x172V4),0]-If[x173V4>0,40+5 (-1+x173V4),0]-If[x174V4>0,150+25 (-1+x174V4),0]-If[x175V4>0,30+5 (-1+x175V4),0]-If[x178V4>0,20+5 (-1+x178V4),0]-If[x17V4>0,200+50 (-1+x17V4),0]-If[x180V4>0,100+10 (-1+x180V4),0]-If[x182V4>0,400+50 (-1+x182V4),0]-If[x183V4>0,1000+100 (-1+x183V4),0]-If[x184V4>0,250+25 (-1+x184V4),0]-If[x186V4>0,500+50 (-1+x186V4),0]-If[x188V4>0,800+100 (-1+x188V4),0]-If[x189V4>0,800+100 (-1+x189V4),0]-If[x18V4>0,150+25 (-1+x18V4),0]-If[x190V4>0,20+5 (-1+x190V4),0]-If[x191V4>0,250+25 (-1+x191V4),0]-If[x192V4>0,40+5 (-1+x192V4),0]-If[x195V4>0,200+50 (-1+x195V4),0]-If[x199V4>0,600+100 (-1+x199V4),0]-If[x200V4>0,150+25 (-1+x200V4),0]-If[x22V4>0,800+100 (-1+x22V4),0]-If[x23V4>0,250+25 (-1+x23V4),0]-If[x24V4>0,500+50 (-1+x24V4),0]-If[x25V4>0,80+10 (-1+x25V4),0]-If[x27V4>0,50+5 (-1+x27V4),0]-If[x29V4>0,200+25 (-1+x29V4),0]-If[x30V4>0,100+25 (-1+x30V4),0]-If[x34V4>0,40+5 (-1+x34V4),0]-If[x35V4>0,100+10 (-1+x35V4),0]-If[x36V4>0,250+25 (-1+x36V4),0]-If[x39V4>0,200+25 (-1+x39V4),0]-If[x42V4>0,1000+100 (-1+x42V4),0]-If[x43V4>0,800+100 (-1+x43V4),0]-If[x44V4>0,40+10 (-1+x44V4),0]-If[x46V4>0,250+25 (-1+x46V4),0]-If[x48V4>0,30+5 (-1+x48V4),0]-If[x49V4>0,20+5 (-1+x49V4),0]-If[x4V4>0,40+5 (-1+x4V4),0]-If[x51V4>0,800+100 (-1+x51V4),0]-If[x54V4>0,500+50 (-1+x54V4),0]-If[x55V4>0,20+5 (-1+x55V4),0]-If[x57V4>0,1000+100 (-1+x57V4),0]-If[x58V4>0,100+25 (-1+x58V4),0]-If[x5V4>0,60+10 (-1+x5V4),0]-If[x60V4>0,1000+100 (-1+x60V4),0]-If[x63V4>0,200+50 (-1+x63V4),0]-If[x64V4>0,80+10 (-1+x64V4),0]-If[x65V4>0,100+25 (-1+x65V4),0]-If[x66V4>0,30+5 (-1+x66V4),0]-If[x67V4>0,60+10 (-1+x67V4),0]-If[x68V4>0,400+100 (-1+x68V4),0]-If[x69V4>0,100+10 (-1+x69V4),0]-If[x6V4>0,250+25 (-1+x6V4),0]-If[x70V4>0,60+10 (-1+x70V4),0]-If[x74V4>0,80+10 (-1+x74V4),0]-If[x75V4>0,30+5 (-1+x75V4),0]-If[x76V4>0,500+50 (-1+x76V4),0]-If[x77V4>0,400+100 (-1+x77V4),0]-If[x78V4>0,20+5 (-1+x78V4),0]-If[x7V4>0,150+25 (-1+x7V4),0]-If[x82V4>0,150+25 (-1+x82V4),0]-If[x84V4>0,300+50 (-1+x84V4),0]-If[x85V4>0,40+10 (-1+x85V4),0]-If[x86V4>0,100+10 (-1+x86V4),0]-If[x88V4>0,100+25 (-1+x88V4),0]-If[x90V4>0,100+10 (-1+x90V4),0]-If[x92V4>0,20+5 (-1+x92V4),0]-If[x93V4>0,30+5 (-1+x93V4),0]-If[x96V4>0,200+50 (-1+x96V4),0]-If[x99V4>0,40+5 (-1+x99V4),0]-If[x9V4>0,150+25 (-1+x9V4),0])+97 (35370-If[x100V6>0,40+5 (-1+x100V6),0]-If[x101V6>0,40+5 (-1+x101V6),0]-If[x103V6>0,200+50 (-1+x103V6),0]-If[x104V6>0,80+10 (-1+x104V6),0]-If[x105V6>0,30+5 (-1+x105V6),0]-If[x106V6>0,800+100 (-1+x106V6),0]-If[x109V6>0,600+100 (-1+x109V6),0]-If[x112V6>0,800+100 (-1+x112V6),0]-If[x113V6>0,600+100 (-1+x113V6),0]-If[x114V6>0,50+5 (-1+x114V6),0]-If[x116V6>0,80+10 (-1+x116V6),0]-If[x117V6>0,400+100 (-1+x117V6),0]-If[x121V6>0,60+10 (-1+x121V6),0]-If[x123V6>0,500+50 (-1+x123V6),0]-If[x125V6>0,40+5 (-1+x125V6),0]-If[x126V6>0,50+5 (-1+x126V6),0]-If[x127V6>0,20+5 (-1+x127V6),0]-If[x132V6>0,100+10 (-1+x132V6),0]-If[x135V6>0,1000+100 (-1+x135V6),0]-If[x136V6>0,50+5 (-1+x136V6),0]-If[x138V6>0,400+50 (-1+x138V6),0]-If[x139V6>0,250+25 (-1+x139V6),0]-If[x141V6>0,500+50 (-1+x141V6),0]-If[x142V6>0,400+50 (-1+x142V6),0]-If[x143V6>0,250+25 (-1+x143V6),0]-If[x145V6>0,200+50 (-1+x145V6),0]-If[x147V6>0,50+5 (-1+x147V6),0]-If[x149V6>0,400+100 (-1+x149V6),0]-If[x14V6>0,250+25 (-1+x14V6),0]-If[x150V6>0,600+100 (-1+x150V6),0]-If[x151V6>0,200+25 (-1+x151V6),0]-If[x153V6>0,400+50 (-1+x153V6),0]-If[x154V6>0,800+100 (-1+x154V6),0]-If[x156V6>0,20+5 (-1+x156V6),0]-If[x157V6>0,1000+100 (-1+x157V6),0]-If[x158V6>0,400+50 (-1+x158V6),0]-If[x162V6>0,80+10 (-1+x162V6),0]-If[x163V6>0,100+25 (-1+x163V6),0]-If[x164V6>0,100+10 (-1+x164V6),0]-If[x165V6>0,20+5 (-1+x165V6),0]-If[x166V6>0,50+5 (-1+x166V6),0]-If[x167V6>0,20+5 (-1+x167V6),0]-If[x168V6>0,40+5 (-1+x168V6),0]-If[x171V6>0,200+50 (-1+x171V6),0]-If[x173V6>0,20+5 (-1+x173V6),0]-If[x174V6>0,100+25 (-1+x174V6),0]-If[x175V6>0,400+100 (-1+x175V6),0]-If[x176V6>0,200+25 (-1+x176V6),0]-If[x179V6>0,20+5 (-1+x179V6),0]-If[x17V6>0,300+50 (-1+x17V6),0]-If[x180V6>0,150+25 (-1+x180V6),0]-If[x184V6>0,30+5 (-1+x184V6),0]-If[x187V6>0,40+10 (-1+x187V6),0]-If[x188V6>0,100+25 (-1+x188V6),0]-If[x189V6>0,50+5 (-1+x189V6),0]-If[x18V6>0,200+25 (-1+x18V6),0]-If[x190V6>0,500+50 (-1+x190V6),0]-If[x192V6>0,300+50 (-1+x192V6),0]-If[x193V6>0,600+100 (-1+x193V6),0]-If[x198V6>0,400+100 (-1+x198V6),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x200V6>0,20+5 (-1+x200V6),0]-If[x20V6>0,250+25 (-1+x20V6),0]-If[x21V6>0,50+5 (-1+x21V6),0]-If[x22V6>0,20+5 (-1+x22V6),0]-If[x26V6>0,30+5 (-1+x26V6),0]-If[x28V6>0,40+10 (-1+x28V6),0]-If[x2V6>0,40+10 (-1+x2V6),0]-If[x31V6>0,500+50 (-1+x31V6),0]-If[x34V6>0,60+10 (-1+x34V6),0]-If[x36V6>0,50+5 (-1+x36V6),0]-If[x41V6>0,800+100 (-1+x41V6),0]-If[x42V6>0,800+100 (-1+x42V6),0]-If[x44V6>0,80+10 (-1+x44V6),0]-If[x46V6>0,600+100 (-1+x46V6),0]-If[x47V6>0,30+5 (-1+x47V6),0]-If[x48V6>0,100+10 (-1+x48V6),0]-If[x50V6>0,500+50 (-1+x50V6),0]-If[x51V6>0,200+50 (-1+x51V6),0]-If[x55V6>0,1000+100 (-1+x55V6),0]-If[x57V6>0,60+10 (-1+x57V6),0]-If[x58V6>0,20+5 (-1+x58V6),0]-If[x5V6>0,150+25 (-1+x5V6),0]-If[x63V6>0,400+50 (-1+x63V6),0]-If[x66V6>0,50+5 (-1+x66V6),0]-If[x70V6>0,150+25 (-1+x70V6),0]-If[x73V6>0,20+5 (-1+x73V6),0]-If[x74V6>0,20+5 (-1+x74V6),0]-If[x75V6>0,100+10 (-1+x75V6),0]-If[x77V6>0,400+100 (-1+x77V6),0]-If[x78V6>0,40+10 (-1+x78V6),0]-If[x79V6>0,800+100 (-1+x79V6),0]-If[x7V6>0,100+10 (-1+x7V6),0]-If[x82V6>0,400+50 (-1+x82V6),0]-If[x84V6>0,60+10 (-1+x84V6),0]-If[x85V6>0,800+100 (-1+x85V6),0]-If[x86V6>0,400+100 (-1+x86V6),0]-If[x87V6>0,30+5 (-1+x87V6),0]-If[x88V6>0,200+25 (-1+x88V6),0]-If[x89V6>0,400+100 (-1+x89V6),0]-If[x91V6>0,800+100 (-1+x91V6),0]-If[x96V6>0,50+5 (-1+x96V6),0]-If[x97V6>0,800+100 (-1+x97V6),0]-If[x98V6>0,30+5 (-1+x98V6),0]-If[x9V6>0,100+25 (-1+x9V6),0])+52 (80689-If[x100V8>0,1000+100 (-1+x100V8),0]-If[x106V8>0,100+10 (-1+x106V8),0]-If[x109V8>0,40+5 (-1+x109V8),0]-If[x10V8>0,100+25 (-1+x10V8),0]-If[x111V8>0,200+25 (-1+x111V8),0]-If[x114V8>0,400+100 (-1+x114V8),0]-If[x116V8>0,60+10 (-1+x116V8),0]-If[x118V8>0,500+50 (-1+x118V8),0]-If[x122V8>0,400+100 (-1+x122V8),0]-If[x126V8>0,800+100 (-1+x126V8),0]-If[x12V8>0,80+10 (-1+x12V8),0]-If[x133V8>0,150+25 (-1+x133V8),0]-If[x134V8>0,150+25 (-1+x134V8),0]-If[x137V8>0,100+10 (-1+x137V8),0]-If[x138V8>0,1000+100 (-1+x138V8),0]-If[x139V8>0,800+100 (-1+x139V8),0]-If[x140V8>0,600+100 (-1+x140V8),0]-If[x141V8>0,800+100 (-1+x141V8),0]-If[x142V8>0,150+25 (-1+x142V8),0]-If[x143V8>0,20+5 (-1+x143V8),0]-If[x145V8>0,400+50 (-1+x145V8),0]-If[x147V8>0,100+10 (-1+x147V8),0]-If[x148V8>0,30+5 (-1+x148V8),0]-If[x150V8>0,500+50 (-1+x150V8),0]-If[x155V8>0,100+10 (-1+x155V8),0]-If[x160V8>0,1000+100 (-1+x160V8),0]-If[x162V8>0,500+50 (-1+x162V8),0]-If[x163V8>0,300+50 (-1+x163V8),0]-If[x165V8>0,400+50 (-1+x165V8),0]-If[x169V8>0,400+100 (-1+x169V8),0]-If[x172V8>0,30+5 (-1+x172V8),0]-If[x174V8>0,60+10 (-1+x174V8),0]-If[x17V8>0,30+5 (-1+x17V8),0]-If[x180V8>0,40+5 (-1+x180V8),0]-If[x181V8>0,1000+100 (-1+x181V8),0]-If[x182V8>0,1000+100 (-1+x182V8),0]-If[x183V8>0,40+10 (-1+x183V8),0]-If[x186V8>0,500+50 (-1+x186V8),0]-If[x188V8>0,500+50 (-1+x188V8),0]-If[x190V8>0,80+10 (-1+x190V8),0]-If[x191V8>0,40+10 (-1+x191V8),0]-If[x195V8>0,400+100 (-1+x195V8),0]-If[x199V8>0,500+50 (-1+x199V8),0]-If[x19V8>0,50+5 (-1+x19V8),0]-If[x20V8>0,400+100 (-1+x20V8),0]-If[x28V8>0,400+50 (-1+x28V8),0]-If[x29V8>0,100+25 (-1+x29V8),0]-If[x32V8>0,600+100 (-1+x32V8),0]-If[x34V8>0,30+5 (-1+x34V8),0]-If[x35V8>0,200+25 (-1+x35V8),0]-If[x38V8>0,40+5 (-1+x38V8),0]-If[x39V8>0,200+25 (-1+x39V8),0]-If[x41V8>0,60+10 (-1+x41V8),0]-If[x43V8>0,200+50 (-1+x43V8),0]-If[x45V8>0,40+10 (-1+x45V8),0]-If[x47V8>0,200+50 (-1+x47V8),0]-If[x48V8>0,500+50 (-1+x48V8),0]-If[x4V8>0,20+5 (-1+x4V8),0]-If[x50V8>0,500+50 (-1+x50V8),0]-If[x51V8>0,300+50 (-1+x51V8),0]-If[x52V8>0,300+50 (-1+x52V8),0]-If[x53V8>0,60+10 (-1+x53V8),0]-If[x55V8>0,100+10 (-1+x55V8),0]-If[x56V8>0,1000+100 (-1+x56V8),0]-If[x57V8>0,30+5 (-1+x57V8),0]-If[x58V8>0,60+10 (-1+x58V8),0]-If[x5V8>0,600+100 (-1+x5V8),0]-If[x63V8>0,20+5 (-1+x63V8),0]-If[x64V8>0,50+5 (-1+x64V8),0]-If[x66V8>0,100+10 (-1+x66V8),0]-If[x67V8>0,400+100 (-1+x67V8),0]-If[x68V8>0,50+5 (-1+x68V8),0]-If[x69V8>0,150+25 (-1+x69V8),0]-If[x6V8>0,50+5 (-1+x6V8),0]-If[x71V8>0,40+10 (-1+x71V8),0]-If[x73V8>0,80+10 (-1+x73V8),0]-If[x76V8>0,80+10 (-1+x76V8),0]-If[x81V8>0,150+25 (-1+x81V8),0]-If[x84V8>0,60+10 (-1+x84V8),0]-If[x85V8>0,100+25 (-1+x85V8),0]-If[x86V8>0,40+10 (-1+x86V8),0]-If[x87V8>0,100+10 (-1+x87V8),0]-If[x88V8>0,400+100 (-1+x88V8),0]-If[x89V8>0,400+50 (-1+x89V8),0]-If[x8V8>0,20+5 (-1+x8V8),0]-If[x90V8>0,250+25 (-1+x90V8),0]-If[x91V8>0,100+10 (-1+x91V8),0]-If[x93V8>0,20+5 (-1+x93V8),0]-If[x94V8>0,600+100 (-1+x94V8),0]-If[x95V8>0,300+50 (-1+x95V8),0]-If[x9V8>0,600+100 (-1+x9V8),0])+68 (11090-If[x100V9>0,20+5 (-1+x100V9),0]-If[x101V9>0,100+10 (-1+x101V9),0]-If[x102V9>0,100+10 (-1+x102V9),0]-If[x103V9>0,400+50 (-1+x103V9),0]-If[x104V9>0,40+10 (-1+x104V9),0]-If[x106V9>0,60+10 (-1+x106V9),0]-If[x107V9>0,50+5 (-1+x107V9),0]-If[x111V9>0,200+25 (-1+x111V9),0]-If[x112V9>0,250+25 (-1+x112V9),0]-If[x113V9>0,800+100 (-1+x113V9),0]-If[x114V9>0,80+10 (-1+x114V9),0]-If[x115V9>0,80+10 (-1+x115V9),0]-If[x116V9>0,40+10 (-1+x116V9),0]-If[x117V9>0,600+100 (-1+x117V9),0]-If[x118V9>0,40+5 (-1+x118V9),0]-If[x119V9>0,60+10 (-1+x119V9),0]-If[x11V9>0,100+25 (-1+x11V9),0]-If[x120V9>0,40+10 (-1+x120V9),0]-If[x122V9>0,100+10 (-1+x122V9),0]-If[x123V9>0,30+5 (-1+x123V9),0]-If[x125V9>0,200+25 (-1+x125V9),0]-If[x126V9>0,500+50 (-1+x126V9),0]-If[x127V9>0,100+10 (-1+x127V9),0]-If[x129V9>0,60+10 (-1+x129V9),0]-If[x12V9>0,800+100 (-1+x12V9),0]-If[x132V9>0,250+25 (-1+x132V9),0]-If[x134V9>0,800+100 (-1+x134V9),0]-If[x139V9>0,400+100 (-1+x139V9),0]-If[x140V9>0,800+100 (-1+x140V9),0]-If[x142V9>0,40+5 (-1+x142V9),0]-If[x143V9>0,200+50 (-1+x143V9),0]-If[x151V9>0,200+50 (-1+x151V9),0]-If[x153V9>0,30+5 (-1+x153V9),0]-If[x154V9>0,100+10 (-1+x154V9),0]-If[x156V9>0,60+10 (-1+x156V9),0]-If[x157V9>0,100+10 (-1+x157V9),0]-If[x160V9>0,400+50 (-1+x160V9),0]-If[x161V9>0,50+5 (-1+x161V9),0]-If[x162V9>0,250+25 (-1+x162V9),0]-If[x164V9>0,60+10 (-1+x164V9),0]-If[x167V9>0,200+50 (-1+x167V9),0]-If[x168V9>0,200+25 (-1+x168V9),0]-If[x170V9>0,30+5 (-1+x170V9),0]-If[x172V9>0,1000+100 (-1+x172V9),0]-If[x179V9>0,40+5 (-1+x179V9),0]-If[x17V9>0,150+25 (-1+x17V9),0]-If[x180V9>0,20+5 (-1+x180V9),0]-If[x182V9>0,800+100 (-1+x182V9),0]-If[x183V9>0,200+50 (-1+x183V9),0]-If[x184V9>0,300+50 (-1+x184V9),0]-If[x186V9>0,80+10 (-1+x186V9),0]-If[x187V9>0,400+100 (-1+x187V9),0]-If[x188V9>0,80+10 (-1+x188V9),0]-If[x191V9>0,50+5 (-1+x191V9),0]-If[x192V9>0,40+10 (-1+x192V9),0]-If[x193V9>0,50+5 (-1+x193V9),0]-If[x195V9>0,500+50 (-1+x195V9),0]-If[x198V9>0,800+100 (-1+x198V9),0]-If[x199V9>0,100+10 (-1+x199V9),0]-If[x19V9>0,1000+100 (-1+x19V9),0]-If[x24V9>0,200+50 (-1+x24V9),0]-If[x25V9>0,600+100 (-1+x25V9),0]-If[x26V9>0,20+5 (-1+x26V9),0]-If[x28V9>0,50+5 (-1+x28V9),0]-If[x29V9>0,600+100 (-1+x29V9),0]-If[x2V9>0,30+5 (-1+x2V9),0]-If[x32V9>0,400+50 (-1+x32V9),0]-If[x36V9>0,40+5 (-1+x36V9),0]-If[x37V9>0,60+10 (-1+x37V9),0]-If[x38V9>0,200+25 (-1+x38V9),0]-If[x39V9>0,80+10 (-1+x39V9),0]-If[x40V9>0,1000+100 (-1+x40V9),0]-If[x41V9>0,40+10 (-1+x41V9),0]-If[x42V9>0,200+25 (-1+x42V9),0]-If[x49V9>0,100+25 (-1+x49V9),0]-If[x4V9>0,30+5 (-1+x4V9),0]-If[x50V9>0,80+10 (-1+x50V9),0]-If[x51V9>0,400+100 (-1+x51V9),0]-If[x53V9>0,150+25 (-1+x53V9),0]-If[x55V9>0,80+10 (-1+x55V9),0]-If[x57V9>0,800+100 (-1+x57V9),0]-If[x58V9>0,100+10 (-1+x58V9),0]-If[x5V9>0,80+10 (-1+x5V9),0]-If[x60V9>0,40+10 (-1+x60V9),0]-If[x61V9>0,250+25 (-1+x61V9),0]-If[x63V9>0,30+5 (-1+x63V9),0]-If[x65V9>0,500+50 (-1+x65V9),0]-If[x66V9>0,500+50 (-1+x66V9),0]-If[x68V9>0,150+25 (-1+x68V9),0]-If[x69V9>0,40+5 (-1+x69V9),0]-If[x71V9>0,40+5 (-1+x71V9),0]-If[x73V9>0,300+50 (-1+x73V9),0]-If[x75V9>0,20+5 (-1+x75V9),0]-If[x76V9>0,1000+100 (-1+x76V9),0]-If[x80V9>0,100+10 (-1+x80V9),0]-If[x81V9>0,150+25 (-1+x81V9),0]-If[x82V9>0,250+25 (-1+x82V9),0]-If[x83V9>0,300+50 (-1+x83V9),0]-If[x85V9>0,400+100 (-1+x85V9),0]-If[x86V9>0,30+5 (-1+x86V9),0]-If[x87V9>0,200+25 (-1+x87V9),0]-If[x88V9>0,100+25 (-1+x88V9),0]-If[x90V9>0,400+100 (-1+x90V9),0]-If[x94V9>0,400+50 (-1+x94V9),0]-If[x95V9>0,200+25 (-1+x95V9),0]-If[x96V9>0,20+5 (-1+x96V9),0]-If[x97V9>0,100+25 (-1+x97V9),0]-If[x98V9>0,50+5 (-1+x98V9),0]-If[x99V9>0,600+100 (-1+x99V9),0]-If[x9V9>0,200+50 (-1+x9V9),0])+(-613+If[x100V1>0,30+5 (-1+x100V1),0]+If[x100V10>0,1000+100 (-1+x100V10),0]+If[x100V3>0,600+100 (-1+x100V3),0]+If[x100V4>0,80+10 (-1+x100V4),0]+If[x100V6>0,40+5 (-1+x100V6),0]+If[x100V8>0,1000+100 (-1+x100V8),0]+If[x100V9>0,20+5 (-1+x100V9),0]) If[613-If[x100V1>0,30+5 (-1+x100V1),0]-If[x100V10>0,1000+100 (-1+x100V10),0]-If[x100V3>0,600+100 (-1+x100V3),0]-If[x100V4>0,80+10 (-1+x100V4),0]-If[x100V6>0,40+5 (-1+x100V6),0]-If[x100V8>0,1000+100 (-1+x100V8),0]-If[x100V9>0,20+5 (-1+x100V9),0]>0,164,0]+(-674+If[x101V2>0,40+10 (-1+x101V2),0]+If[x101V3>0,400+100 (-1+x101V3),0]+If[x101V5>0,600+100 (-1+x101V5),0]+If[x101V6>0,40+5 (-1+x101V6),0]+If[x101V7>0,60+10 (-1+x101V7),0]+If[x101V9>0,100+10 (-1+x101V9),0]) If[674-If[x101V2>0,40+10 (-1+x101V2),0]-If[x101V3>0,400+100 (-1+x101V3),0]-If[x101V5>0,600+100 (-1+x101V5),0]-If[x101V6>0,40+5 (-1+x101V6),0]-If[x101V7>0,60+10 (-1+x101V7),0]-If[x101V9>0,100+10 (-1+x101V9),0]>0,107,0]+(-1578+If[x102V1>0,40+5 (-1+x102V1),0]+If[x102V3>0,150+25 (-1+x102V3),0]+If[x102V4>0,100+10 (-1+x102V4),0]+If[x102V7>0,30+5 (-1+x102V7),0]+If[x102V9>0,100+10 (-1+x102V9),0]) If[1578-If[x102V1>0,40+5 (-1+x102V1),0]-If[x102V3>0,150+25 (-1+x102V3),0]-If[x102V4>0,100+10 (-1+x102V4),0]-If[x102V7>0,30+5 (-1+x102V7),0]-If[x102V9>0,100+10 (-1+x102V9),0]>0,105,0]+(-2063+If[x103V1>0,40+5 (-1+x103V1),0]+If[x103V10>0,40+5 (-1+x103V10),0]+If[x103V2>0,60+10 (-1+x103V2),0]+If[x103V3>0,600+100 (-1+x103V3),0]+If[x103V4>0,600+100 (-1+x103V4),0]+If[x103V6>0,200+50 (-1+x103V6),0]+If[x103V9>0,400+50 (-1+x103V9),0]) If[2063-If[x103V1>0,40+5 (-1+x103V1),0]-If[x103V10>0,40+5 (-1+x103V10),0]-If[x103V2>0,60+10 (-1+x103V2),0]-If[x103V3>0,600+100 (-1+x103V3),0]-If[x103V4>0,600+100 (-1+x103V4),0]-If[x103V6>0,200+50 (-1+x103V6),0]-If[x103V9>0,400+50 (-1+x103V9),0]>0,190,0]+(-479+If[x104V6>0,80+10 (-1+x104V6),0]+If[x104V9>0,40+10 (-1+x104V9),0]) If[479-If[x104V6>0,80+10 (-1+x104V6),0]-If[x104V9>0,40+10 (-1+x104V9),0]>0,133,0]+(-1941+If[x105V6>0,30+5 (-1+x105V6),0]+If[x105V7>0,600+100 (-1+x105V7),0]) If[1941-If[x105V6>0,30+5 (-1+x105V6),0]-If[x105V7>0,600+100 (-1+x105V7),0]>0,147,0]+(-2959+If[x106V10>0,100+25 (-1+x106V10),0]+If[x106V3>0,40+5 (-1+x106V3),0]+If[x106V4>0,100+10 (-1+x106V4),0]+If[x106V6>0,800+100 (-1+x106V6),0]+If[x106V8>0,100+10 (-1+x106V8),0]+If[x106V9>0,60+10 (-1+x106V9),0]) If[2959-If[x106V10>0,100+25 (-1+x106V10),0]-If[x106V3>0,40+5 (-1+x106V3),0]-If[x106V4>0,100+10 (-1+x106V4),0]-If[x106V6>0,800+100 (-1+x106V6),0]-If[x106V8>0,100+10 (-1+x106V8),0]-If[x106V9>0,60+10 (-1+x106V9),0]>0,163,0]+(-645+If[x107V1>0,400+100 (-1+x107V1),0]+If[x107V2>0,60+10 (-1+x107V2),0]+If[x107V5>0,30+5 (-1+x107V5),0]+If[x107V7>0,500+50 (-1+x107V7),0]+If[x107V9>0,50+5 (-1+x107V9),0]) If[645-If[x107V1>0,400+100 (-1+x107V1),0]-If[x107V2>0,60+10 (-1+x107V2),0]-If[x107V5>0,30+5 (-1+x107V5),0]-If[x107V7>0,500+50 (-1+x107V7),0]-If[x107V9>0,50+5 (-1+x107V9),0]>0,137,0]+(-1988+If[x108V2>0,800+100 (-1+x108V2),0]+If[x108V5>0,60+10 (-1+x108V5),0]) If[1988-If[x108V2>0,800+100 (-1+x108V2),0]-If[x108V5>0,60+10 (-1+x108V5),0]>0,117,0]+(-1200+If[x109V1>0,100+10 (-1+x109V1),0]+If[x109V4>0,100+10 (-1+x109V4),0]+If[x109V6>0,600+100 (-1+x109V6),0]+If[x109V7>0,200+25 (-1+x109V7),0]+If[x109V8>0,40+5 (-1+x109V8),0]) If[1200-If[x109V1>0,100+10 (-1+x109V1),0]-If[x109V4>0,100+10 (-1+x109V4),0]-If[x109V6>0,600+100 (-1+x109V6),0]-If[x109V7>0,200+25 (-1+x109V7),0]-If[x109V8>0,40+5 (-1+x109V8),0]>0,145,0]+(-1641+If[x10V10>0,30+5 (-1+x10V10),0]+If[x10V5>0,50+5 (-1+x10V5),0]+If[x10V7>0,20+5 (-1+x10V7),0]+If[x10V8>0,100+25 (-1+x10V8),0]) If[1641-If[x10V10>0,30+5 (-1+x10V10),0]-If[x10V5>0,50+5 (-1+x10V5),0]-If[x10V7>0,20+5 (-1+x10V7),0]-If[x10V8>0,100+25 (-1+x10V8),0]>0,132,0]+(-2668+If[x110V10>0,600+100 (-1+x110V10),0]+If[x110V7>0,250+25 (-1+x110V7),0]) If[2668-If[x110V10>0,600+100 (-1+x110V10),0]-If[x110V7>0,250+25 (-1+x110V7),0]>0,149,0]+(-2836+If[x111V1>0,100+10 (-1+x111V1),0]+If[x111V2>0,20+5 (-1+x111V2),0]+If[x111V3>0,80+10 (-1+x111V3),0]+If[x111V4>0,600+100 (-1+x111V4),0]+If[x111V7>0,500+50 (-1+x111V7),0]+If[x111V8>0,200+25 (-1+x111V8),0]+If[x111V9>0,200+25 (-1+x111V9),0]) If[2836-If[x111V1>0,100+10 (-1+x111V1),0]-If[x111V2>0,20+5 (-1+x111V2),0]-If[x111V3>0,80+10 (-1+x111V3),0]-If[x111V4>0,600+100 (-1+x111V4),0]-If[x111V7>0,500+50 (-1+x111V7),0]-If[x111V8>0,200+25 (-1+x111V8),0]-If[x111V9>0,200+25 (-1+x111V9),0]>0,179,0]+(-1704+If[x112V3>0,60+10 (-1+x112V3),0]+If[x112V4>0,50+5 (-1+x112V4),0]+If[x112V5>0,100+25 (-1+x112V5),0]+If[x112V6>0,800+100 (-1+x112V6),0]+If[x112V7>0,20+5 (-1+x112V7),0]+If[x112V9>0,250+25 (-1+x112V9),0]) If[1704-If[x112V3>0,60+10 (-1+x112V3),0]-If[x112V4>0,50+5 (-1+x112V4),0]-If[x112V5>0,100+25 (-1+x112V5),0]-If[x112V6>0,800+100 (-1+x112V6),0]-If[x112V7>0,20+5 (-1+x112V7),0]-If[x112V9>0,250+25 (-1+x112V9),0]>0,126,0]+(-1449+If[x113V1>0,100+25 (-1+x113V1),0]+If[x113V10>0,1000+100 (-1+x113V10),0]+If[x113V4>0,20+5 (-1+x113V4),0]+If[x113V5>0,40+5 (-1+x113V5),0]+If[x113V6>0,600+100 (-1+x113V6),0]+If[x113V7>0,400+50 (-1+x113V7),0]+If[x113V9>0,800+100 (-1+x113V9),0]) If[1449-If[x113V1>0,100+25 (-1+x113V1),0]-If[x113V10>0,1000+100 (-1+x113V10),0]-If[x113V4>0,20+5 (-1+x113V4),0]-If[x113V5>0,40+5 (-1+x113V5),0]-If[x113V6>0,600+100 (-1+x113V6),0]-If[x113V7>0,400+50 (-1+x113V7),0]-If[x113V9>0,800+100 (-1+x113V9),0]>0,126,0]+(-2395+If[x114V1>0,100+10 (-1+x114V1),0]+If[x114V10>0,500+50 (-1+x114V10),0]+If[x114V2>0,800+100 (-1+x114V2),0]+If[x114V3>0,600+100 (-1+x114V3),0]+If[x114V4>0,60+10 (-1+x114V4),0]+If[x114V6>0,50+5 (-1+x114V6),0]+If[x114V7>0,250+25 (-1+x114V7),0]+If[x114V8>0,400+100 (-1+x114V8),0]+If[x114V9>0,80+10 (-1+x114V9),0]) If[2395-If[x114V1>0,100+10 (-1+x114V1),0]-If[x114V10>0,500+50 (-1+x114V10),0]-If[x114V2>0,800+100 (-1+x114V2),0]-If[x114V3>0,600+100 (-1+x114V3),0]-If[x114V4>0,60+10 (-1+x114V4),0]-If[x114V6>0,50+5 (-1+x114V6),0]-If[x114V7>0,250+25 (-1+x114V7),0]-If[x114V8>0,400+100 (-1+x114V8),0]-If[x114V9>0,80+10 (-1+x114V9),0]>0,177,0]+(-1987+If[x115V1>0,300+50 (-1+x115V1),0]+If[x115V4>0,1000+100 (-1+x115V4),0]+If[x115V7>0,1000+100 (-1+x115V7),0]+If[x115V9>0,80+10 (-1+x115V9),0]) If[1987-If[x115V1>0,300+50 (-1+x115V1),0]-If[x115V4>0,1000+100 (-1+x115V4),0]-If[x115V7>0,1000+100 (-1+x115V7),0]-If[x115V9>0,80+10 (-1+x115V9),0]>0,170,0]+(-2326+If[x116V1>0,40+5 (-1+x116V1),0]+If[x116V10>0,200+50 (-1+x116V10),0]+If[x116V2>0,40+5 (-1+x116V2),0]+If[x116V4>0,60+10 (-1+x116V4),0]+If[x116V5>0,40+10 (-1+x116V5),0]+If[x116V6>0,80+10 (-1+x116V6),0]+If[x116V8>0,60+10 (-1+x116V8),0]+If[x116V9>0,40+10 (-1+x116V9),0]) If[2326-If[x116V1>0,40+5 (-1+x116V1),0]-If[x116V10>0,200+50 (-1+x116V10),0]-If[x116V2>0,40+5 (-1+x116V2),0]-If[x116V4>0,60+10 (-1+x116V4),0]-If[x116V5>0,40+10 (-1+x116V5),0]-If[x116V6>0,80+10 (-1+x116V6),0]-If[x116V8>0,60+10 (-1+x116V8),0]-If[x116V9>0,40+10 (-1+x116V9),0]>0,137,0]+(-1880+If[x117V1>0,1000+100 (-1+x117V1),0]+If[x117V10>0,100+25 (-1+x117V10),0]+If[x117V3>0,200+25 (-1+x117V3),0]+If[x117V4>0,600+100 (-1+x117V4),0]+If[x117V6>0,400+100 (-1+x117V6),0]+If[x117V7>0,400+50 (-1+x117V7),0]+If[x117V9>0,600+100 (-1+x117V9),0]) If[1880-If[x117V1>0,1000+100 (-1+x117V1),0]-If[x117V10>0,100+25 (-1+x117V10),0]-If[x117V3>0,200+25 (-1+x117V3),0]-If[x117V4>0,600+100 (-1+x117V4),0]-If[x117V6>0,400+100 (-1+x117V6),0]-If[x117V7>0,400+50 (-1+x117V7),0]-If[x117V9>0,600+100 (-1+x117V9),0]>0,150,0]+(-2497+If[x118V1>0,100+25 (-1+x118V1),0]+If[x118V10>0,60+10 (-1+x118V10),0]+If[x118V7>0,30+5 (-1+x118V7),0]+If[x118V8>0,500+50 (-1+x118V8),0]+If[x118V9>0,40+5 (-1+x118V9),0]) If[2497-If[x118V1>0,100+25 (-1+x118V1),0]-If[x118V10>0,60+10 (-1+x118V10),0]-If[x118V7>0,30+5 (-1+x118V7),0]-If[x118V8>0,500+50 (-1+x118V8),0]-If[x118V9>0,40+5 (-1+x118V9),0]>0,165,0]+(-206+If[x119V1>0,600+100 (-1+x119V1),0]+If[x119V9>0,60+10 (-1+x119V9),0]) If[206-If[x119V1>0,600+100 (-1+x119V1),0]-If[x119V9>0,60+10 (-1+x119V9),0]>0,184,0]+(-1207+If[x11V1>0,100+10 (-1+x11V1),0]+If[x11V10>0,250+25 (-1+x11V10),0]+If[x11V2>0,20+5 (-1+x11V2),0]+If[x11V3>0,300+50 (-1+x11V3),0]+If[x11V4>0,300+50 (-1+x11V4),0]+If[x11V9>0,100+25 (-1+x11V9),0]) If[1207-If[x11V1>0,100+10 (-1+x11V1),0]-If[x11V10>0,250+25 (-1+x11V10),0]-If[x11V2>0,20+5 (-1+x11V2),0]-If[x11V3>0,300+50 (-1+x11V3),0]-If[x11V4>0,300+50 (-1+x11V4),0]-If[x11V9>0,100+25 (-1+x11V9),0]>0,120,0]+(-2763+If[x120V4>0,600+100 (-1+x120V4),0]+If[x120V7>0,800+100 (-1+x120V7),0]+If[x120V9>0,40+10 (-1+x120V9),0]) If[2763-If[x120V4>0,600+100 (-1+x120V4),0]-If[x120V7>0,800+100 (-1+x120V7),0]-If[x120V9>0,40+10 (-1+x120V9),0]>0,158,0]+(-1149+If[x121V5>0,80+10 (-1+x121V5),0]+If[x121V6>0,60+10 (-1+x121V6),0]) If[1149-If[x121V5>0,80+10 (-1+x121V5),0]-If[x121V6>0,60+10 (-1+x121V6),0]>0,132,0]+(-1259+If[x122V10>0,400+50 (-1+x122V10),0]+If[x122V5>0,40+10 (-1+x122V5),0]+If[x122V7>0,40+5 (-1+x122V7),0]+If[x122V8>0,400+100 (-1+x122V8),0]+If[x122V9>0,100+10 (-1+x122V9),0]) If[1259-If[x122V10>0,400+50 (-1+x122V10),0]-If[x122V5>0,40+10 (-1+x122V5),0]-If[x122V7>0,40+5 (-1+x122V7),0]-If[x122V8>0,400+100 (-1+x122V8),0]-If[x122V9>0,100+10 (-1+x122V9),0]>0,180,0]+(-195+If[x123V1>0,50+5 (-1+x123V1),0]+If[x123V10>0,800+100 (-1+x123V10),0]+If[x123V2>0,20+5 (-1+x123V2),0]+If[x123V4>0,30+5 (-1+x123V4),0]+If[x123V6>0,500+50 (-1+x123V6),0]+If[x123V9>0,30+5 (-1+x123V9),0]) If[195-If[x123V1>0,50+5 (-1+x123V1),0]-If[x123V10>0,800+100 (-1+x123V10),0]-If[x123V2>0,20+5 (-1+x123V2),0]-If[x123V4>0,30+5 (-1+x123V4),0]-If[x123V6>0,500+50 (-1+x123V6),0]-If[x123V9>0,30+5 (-1+x123V9),0]>0,160,0]+(-1803+If[x124V1>0,1000+100 (-1+x124V1),0]+If[x124V5>0,1000+100 (-1+x124V5),0]+If[x124V7>0,800+100 (-1+x124V7),0]) If[1803-If[x124V1>0,1000+100 (-1+x124V1),0]-If[x124V5>0,1000+100 (-1+x124V5),0]-If[x124V7>0,800+100 (-1+x124V7),0]>0,148,0]+(-2456+If[x125V10>0,60+10 (-1+x125V10),0]+If[x125V2>0,30+5 (-1+x125V2),0]+If[x125V4>0,50+5 (-1+x125V4),0]+If[x125V5>0,600+100 (-1+x125V5),0]+If[x125V6>0,40+5 (-1+x125V6),0]+If[x125V7>0,40+10 (-1+x125V7),0]+If[x125V9>0,200+25 (-1+x125V9),0]) If[2456-If[x125V10>0,60+10 (-1+x125V10),0]-If[x125V2>0,30+5 (-1+x125V2),0]-If[x125V4>0,50+5 (-1+x125V4),0]-If[x125V5>0,600+100 (-1+x125V5),0]-If[x125V6>0,40+5 (-1+x125V6),0]-If[x125V7>0,40+10 (-1+x125V7),0]-If[x125V9>0,200+25 (-1+x125V9),0]>0,147,0]+(-320+If[x126V1>0,400+50 (-1+x126V1),0]+If[x126V2>0,600+100 (-1+x126V2),0]+If[x126V4>0,300+50 (-1+x126V4),0]+If[x126V6>0,50+5 (-1+x126V6),0]+If[x126V8>0,800+100 (-1+x126V8),0]+If[x126V9>0,500+50 (-1+x126V9),0]) If[320-If[x126V1>0,400+50 (-1+x126V1),0]-If[x126V2>0,600+100 (-1+x126V2),0]-If[x126V4>0,300+50 (-1+x126V4),0]-If[x126V6>0,50+5 (-1+x126V6),0]-If[x126V8>0,800+100 (-1+x126V8),0]-If[x126V9>0,500+50 (-1+x126V9),0]>0,181,0]+(-2958+If[x127V1>0,1000+100 (-1+x127V1),0]+If[x127V10>0,200+25 (-1+x127V10),0]+If[x127V2>0,40+5 (-1+x127V2),0]+If[x127V4>0,60+10 (-1+x127V4),0]+If[x127V5>0,100+10 (-1+x127V5),0]+If[x127V6>0,20+5 (-1+x127V6),0]+If[x127V7>0,250+25 (-1+x127V7),0]+If[x127V9>0,100+10 (-1+x127V9),0]) If[2958-If[x127V1>0,1000+100 (-1+x127V1),0]-If[x127V10>0,200+25 (-1+x127V10),0]-If[x127V2>0,40+5 (-1+x127V2),0]-If[x127V4>0,60+10 (-1+x127V4),0]-If[x127V5>0,100+10 (-1+x127V5),0]-If[x127V6>0,20+5 (-1+x127V6),0]-If[x127V7>0,250+25 (-1+x127V7),0]-If[x127V9>0,100+10 (-1+x127V9),0]>0,159,0]+(-581+If[x128V1>0,150+25 (-1+x128V1),0]+If[x128V4>0,400+50 (-1+x128V4),0]) If[581-If[x128V1>0,150+25 (-1+x128V1),0]-If[x128V4>0,400+50 (-1+x128V4),0]>0,145,0]+(-1958+If[x129V10>0,200+25 (-1+x129V10),0]+If[x129V2>0,40+5 (-1+x129V2),0]+If[x129V5>0,600+100 (-1+x129V5),0]+If[x129V7>0,300+50 (-1+x129V7),0]+If[x129V9>0,60+10 (-1+x129V9),0]) If[1958-If[x129V10>0,200+25 (-1+x129V10),0]-If[x129V2>0,40+5 (-1+x129V2),0]-If[x129V5>0,600+100 (-1+x129V5),0]-If[x129V7>0,300+50 (-1+x129V7),0]-If[x129V9>0,60+10 (-1+x129V9),0]>0,179,0]+(-2373+If[x12V1>0,600+100 (-1+x12V1),0]+If[x12V10>0,20+5 (-1+x12V10),0]+If[x12V3>0,40+10 (-1+x12V3),0]+If[x12V4>0,100+10 (-1+x12V4),0]+If[x12V5>0,40+10 (-1+x12V5),0]+If[x12V8>0,80+10 (-1+x12V8),0]+If[x12V9>0,800+100 (-1+x12V9),0]) If[2373-If[x12V1>0,600+100 (-1+x12V1),0]-If[x12V10>0,20+5 (-1+x12V10),0]-If[x12V3>0,40+10 (-1+x12V3),0]-If[x12V4>0,100+10 (-1+x12V4),0]-If[x12V5>0,40+10 (-1+x12V5),0]-If[x12V8>0,80+10 (-1+x12V8),0]-If[x12V9>0,800+100 (-1+x12V9),0]>0,161,0]+(-1009+If[x130V1>0,20+5 (-1+x130V1),0]+If[x130V10>0,800+100 (-1+x130V10),0]) If[1009-If[x130V1>0,20+5 (-1+x130V1),0]-If[x130V10>0,800+100 (-1+x130V10),0]>0,113,0]+(-1389+If[x131V1>0,30+5 (-1+x131V1),0]+If[x131V2>0,40+5 (-1+x131V2),0]+If[x131V3>0,80+10 (-1+x131V3),0]+If[x131V4>0,40+10 (-1+x131V4),0]+If[x131V5>0,200+25 (-1+x131V5),0]+If[x131V7>0,500+50 (-1+x131V7),0]) If[1389-If[x131V1>0,30+5 (-1+x131V1),0]-If[x131V2>0,40+5 (-1+x131V2),0]-If[x131V3>0,80+10 (-1+x131V3),0]-If[x131V4>0,40+10 (-1+x131V4),0]-If[x131V5>0,200+25 (-1+x131V5),0]-If[x131V7>0,500+50 (-1+x131V7),0]>0,112,0]+(-1709+If[x132V1>0,800+100 (-1+x132V1),0]+If[x132V3>0,20+5 (-1+x132V3),0]+If[x132V4>0,30+5 (-1+x132V4),0]+If[x132V5>0,1000+100 (-1+x132V5),0]+If[x132V6>0,100+10 (-1+x132V6),0]+If[x132V7>0,30+5 (-1+x132V7),0]+If[x132V9>0,250+25 (-1+x132V9),0]) If[1709-If[x132V1>0,800+100 (-1+x132V1),0]-If[x132V3>0,20+5 (-1+x132V3),0]-If[x132V4>0,30+5 (-1+x132V4),0]-If[x132V5>0,1000+100 (-1+x132V5),0]-If[x132V6>0,100+10 (-1+x132V6),0]-If[x132V7>0,30+5 (-1+x132V7),0]-If[x132V9>0,250+25 (-1+x132V9),0]>0,122,0]+(-2349+If[x133V1>0,200+25 (-1+x133V1),0]+If[x133V10>0,30+5 (-1+x133V10),0]+If[x133V2>0,30+5 (-1+x133V2),0]+If[x133V4>0,100+25 (-1+x133V4),0]+If[x133V5>0,250+25 (-1+x133V5),0]+If[x133V8>0,150+25 (-1+x133V8),0]) If[2349-If[x133V1>0,200+25 (-1+x133V1),0]-If[x133V10>0,30+5 (-1+x133V10),0]-If[x133V2>0,30+5 (-1+x133V2),0]-If[x133V4>0,100+25 (-1+x133V4),0]-If[x133V5>0,250+25 (-1+x133V5),0]-If[x133V8>0,150+25 (-1+x133V8),0]>0,141,0]+(-444+If[x134V1>0,50+5 (-1+x134V1),0]+If[x134V10>0,500+50 (-1+x134V10),0]+If[x134V2>0,50+5 (-1+x134V2),0]+If[x134V3>0,200+25 (-1+x134V3),0]+If[x134V5>0,20+5 (-1+x134V5),0]+If[x134V7>0,50+5 (-1+x134V7),0]+If[x134V8>0,150+25 (-1+x134V8),0]+If[x134V9>0,800+100 (-1+x134V9),0]) If[444-If[x134V1>0,50+5 (-1+x134V1),0]-If[x134V10>0,500+50 (-1+x134V10),0]-If[x134V2>0,50+5 (-1+x134V2),0]-If[x134V3>0,200+25 (-1+x134V3),0]-If[x134V5>0,20+5 (-1+x134V5),0]-If[x134V7>0,50+5 (-1+x134V7),0]-If[x134V8>0,150+25 (-1+x134V8),0]-If[x134V9>0,800+100 (-1+x134V9),0]>0,102,0]+(-376+If[x135V1>0,1000+100 (-1+x135V1),0]+If[x135V3>0,40+10 (-1+x135V3),0]+If[x135V6>0,1000+100 (-1+x135V6),0]+If[x135V7>0,100+25 (-1+x135V7),0]) If[376-If[x135V1>0,1000+100 (-1+x135V1),0]-If[x135V3>0,40+10 (-1+x135V3),0]-If[x135V6>0,1000+100 (-1+x135V6),0]-If[x135V7>0,100+25 (-1+x135V7),0]>0,128,0]+(-981+If[x136V2>0,150+25 (-1+x136V2),0]+If[x136V3>0,150+25 (-1+x136V3),0]+If[x136V5>0,30+5 (-1+x136V5),0]+If[x136V6>0,50+5 (-1+x136V6),0]+If[x136V7>0,30+5 (-1+x136V7),0]) If[981-If[x136V2>0,150+25 (-1+x136V2),0]-If[x136V3>0,150+25 (-1+x136V3),0]-If[x136V5>0,30+5 (-1+x136V5),0]-If[x136V6>0,50+5 (-1+x136V6),0]-If[x136V7>0,30+5 (-1+x136V7),0]>0,195,0]+(-2392+If[x137V2>0,600+100 (-1+x137V2),0]+If[x137V3>0,100+25 (-1+x137V3),0]+If[x137V4>0,400+50 (-1+x137V4),0]+If[x137V8>0,100+10 (-1+x137V8),0]) If[2392-If[x137V2>0,600+100 (-1+x137V2),0]-If[x137V3>0,100+25 (-1+x137V3),0]-If[x137V4>0,400+50 (-1+x137V4),0]-If[x137V8>0,100+10 (-1+x137V8),0]>0,174,0]+(-1481+If[x138V2>0,400+50 (-1+x138V2),0]+If[x138V3>0,1000+100 (-1+x138V3),0]+If[x138V4>0,200+50 (-1+x138V4),0]+If[x138V5>0,100+25 (-1+x138V5),0]+If[x138V6>0,400+50 (-1+x138V6),0]+If[x138V7>0,30+5 (-1+x138V7),0]+If[x138V8>0,1000+100 (-1+x138V8),0]) If[1481-If[x138V2>0,400+50 (-1+x138V2),0]-If[x138V3>0,1000+100 (-1+x138V3),0]-If[x138V4>0,200+50 (-1+x138V4),0]-If[x138V5>0,100+25 (-1+x138V5),0]-If[x138V6>0,400+50 (-1+x138V6),0]-If[x138V7>0,30+5 (-1+x138V7),0]-If[x138V8>0,1000+100 (-1+x138V8),0]>0,139,0]+(-1603+If[x139V1>0,50+5 (-1+x139V1),0]+If[x139V3>0,20+5 (-1+x139V3),0]+If[x139V4>0,1000+100 (-1+x139V4),0]+If[x139V5>0,200+50 (-1+x139V5),0]+If[x139V6>0,250+25 (-1+x139V6),0]+If[x139V8>0,800+100 (-1+x139V8),0]+If[x139V9>0,400+100 (-1+x139V9),0]) If[1603-If[x139V1>0,50+5 (-1+x139V1),0]-If[x139V3>0,20+5 (-1+x139V3),0]-If[x139V4>0,1000+100 (-1+x139V4),0]-If[x139V5>0,200+50 (-1+x139V5),0]-If[x139V6>0,250+25 (-1+x139V6),0]-If[x139V8>0,800+100 (-1+x139V8),0]-If[x139V9>0,400+100 (-1+x139V9),0]>0,167,0]+(-991+If[x13V1>0,100+10 (-1+x13V1),0]+If[x13V10>0,600+100 (-1+x13V10),0]+If[x13V3>0,200+50 (-1+x13V3),0]+If[x13V4>0,600+100 (-1+x13V4),0]+If[x13V7>0,60+10 (-1+x13V7),0]) If[991-If[x13V1>0,100+10 (-1+x13V1),0]-If[x13V10>0,600+100 (-1+x13V10),0]-If[x13V3>0,200+50 (-1+x13V3),0]-If[x13V4>0,600+100 (-1+x13V4),0]-If[x13V7>0,60+10 (-1+x13V7),0]>0,175,0]+(-7+If[x140V1>0,40+5 (-1+x140V1),0]+If[x140V3>0,40+5 (-1+x140V3),0]+If[x140V4>0,80+10 (-1+x140V4),0]+If[x140V5>0,800+100 (-1+x140V5),0]+If[x140V7>0,100+25 (-1+x140V7),0]+If[x140V8>0,600+100 (-1+x140V8),0]+If[x140V9>0,800+100 (-1+x140V9),0]) If[7-If[x140V1>0,40+5 (-1+x140V1),0]-If[x140V3>0,40+5 (-1+x140V3),0]-If[x140V4>0,80+10 (-1+x140V4),0]-If[x140V5>0,800+100 (-1+x140V5),0]-If[x140V7>0,100+25 (-1+x140V7),0]-If[x140V8>0,600+100 (-1+x140V8),0]-If[x140V9>0,800+100 (-1+x140V9),0]>0,200,0]+(-1598+If[x141V10>0,100+10 (-1+x141V10),0]+If[x141V2>0,80+10 (-1+x141V2),0]+If[x141V4>0,60+10 (-1+x141V4),0]+If[x141V5>0,100+25 (-1+x141V5),0]+If[x141V6>0,500+50 (-1+x141V6),0]+If[x141V7>0,100+25 (-1+x141V7),0]+If[x141V8>0,800+100 (-1+x141V8),0]) If[1598-If[x141V10>0,100+10 (-1+x141V10),0]-If[x141V2>0,80+10 (-1+x141V2),0]-If[x141V4>0,60+10 (-1+x141V4),0]-If[x141V5>0,100+25 (-1+x141V5),0]-If[x141V6>0,500+50 (-1+x141V6),0]-If[x141V7>0,100+25 (-1+x141V7),0]-If[x141V8>0,800+100 (-1+x141V8),0]>0,168,0]+(-2724+If[x142V1>0,800+100 (-1+x142V1),0]+If[x142V2>0,400+50 (-1+x142V2),0]+If[x142V3>0,100+10 (-1+x142V3),0]+If[x142V4>0,100+10 (-1+x142V4),0]+If[x142V6>0,400+50 (-1+x142V6),0]+If[x142V8>0,150+25 (-1+x142V8),0]+If[x142V9>0,40+5 (-1+x142V9),0]) If[2724-If[x142V1>0,800+100 (-1+x142V1),0]-If[x142V2>0,400+50 (-1+x142V2),0]-If[x142V3>0,100+10 (-1+x142V3),0]-If[x142V4>0,100+10 (-1+x142V4),0]-If[x142V6>0,400+50 (-1+x142V6),0]-If[x142V8>0,150+25 (-1+x142V8),0]-If[x142V9>0,40+5 (-1+x142V9),0]>0,184,0]+(-2148+If[x143V2>0,20+5 (-1+x143V2),0]+If[x143V6>0,250+25 (-1+x143V6),0]+If[x143V8>0,20+5 (-1+x143V8),0]+If[x143V9>0,200+50 (-1+x143V9),0]) If[2148-If[x143V2>0,20+5 (-1+x143V2),0]-If[x143V6>0,250+25 (-1+x143V6),0]-If[x143V8>0,20+5 (-1+x143V8),0]-If[x143V9>0,200+50 (-1+x143V9),0]>0,167,0]+(-1005+If[x144V1>0,250+25 (-1+x144V1),0]+If[x144V7>0,400+50 (-1+x144V7),0]) If[1005-If[x144V1>0,250+25 (-1+x144V1),0]-If[x144V7>0,400+50 (-1+x144V7),0]>0,185,0]+(-2525+If[x145V10>0,30+5 (-1+x145V10),0]+If[x145V2>0,50+5 (-1+x145V2),0]+If[x145V5>0,40+5 (-1+x145V5),0]+If[x145V6>0,200+50 (-1+x145V6),0]+If[x145V7>0,200+25 (-1+x145V7),0]+If[x145V8>0,400+50 (-1+x145V8),0]) If[2525-If[x145V10>0,30+5 (-1+x145V10),0]-If[x145V2>0,50+5 (-1+x145V2),0]-If[x145V5>0,40+5 (-1+x145V5),0]-If[x145V6>0,200+50 (-1+x145V6),0]-If[x145V7>0,200+25 (-1+x145V7),0]-If[x145V8>0,400+50 (-1+x145V8),0]>0,190,0]+(-1558+If[x146V3>0,400+100 (-1+x146V3),0]+If[x146V5>0,150+25 (-1+x146V5),0]) If[1558-If[x146V3>0,400+100 (-1+x146V3),0]-If[x146V5>0,150+25 (-1+x146V5),0]>0,112,0]+(-1179+If[x147V1>0,40+5 (-1+x147V1),0]+If[x147V10>0,200+50 (-1+x147V10),0]+If[x147V4>0,30+5 (-1+x147V4),0]+If[x147V5>0,20+5 (-1+x147V5),0]+If[x147V6>0,50+5 (-1+x147V6),0]+If[x147V8>0,100+10 (-1+x147V8),0]) If[1179-If[x147V1>0,40+5 (-1+x147V1),0]-If[x147V10>0,200+50 (-1+x147V10),0]-If[x147V4>0,30+5 (-1+x147V4),0]-If[x147V5>0,20+5 (-1+x147V5),0]-If[x147V6>0,50+5 (-1+x147V6),0]-If[x147V8>0,100+10 (-1+x147V8),0]>0,181,0]+(-1283+If[x148V2>0,150+25 (-1+x148V2),0]+If[x148V4>0,250+25 (-1+x148V4),0]+If[x148V5>0,60+10 (-1+x148V5),0]+If[x148V7>0,100+10 (-1+x148V7),0]+If[x148V8>0,30+5 (-1+x148V8),0]) If[1283-If[x148V2>0,150+25 (-1+x148V2),0]-If[x148V4>0,250+25 (-1+x148V4),0]-If[x148V5>0,60+10 (-1+x148V5),0]-If[x148V7>0,100+10 (-1+x148V7),0]-If[x148V8>0,30+5 (-1+x148V8),0]>0,176,0]+(-1897+If[x149V4>0,800+100 (-1+x149V4),0]+If[x149V6>0,400+100 (-1+x149V6),0]) If[1897-If[x149V4>0,800+100 (-1+x149V4),0]-If[x149V6>0,400+100 (-1+x149V6),0]>0,115,0]+(-1042+If[x14V1>0,40+5 (-1+x14V1),0]+If[x14V10>0,50+5 (-1+x14V10),0]+If[x14V2>0,100+10 (-1+x14V2),0]+If[x14V3>0,30+5 (-1+x14V3),0]+If[x14V4>0,50+5 (-1+x14V4),0]+If[x14V5>0,400+50 (-1+x14V5),0]+If[x14V6>0,250+25 (-1+x14V6),0]+If[x14V7>0,250+25 (-1+x14V7),0]) If[1042-If[x14V1>0,40+5 (-1+x14V1),0]-If[x14V10>0,50+5 (-1+x14V10),0]-If[x14V2>0,100+10 (-1+x14V2),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x14V4>0,50+5 (-1+x14V4),0]-If[x14V5>0,400+50 (-1+x14V5),0]-If[x14V6>0,250+25 (-1+x14V6),0]-If[x14V7>0,250+25 (-1+x14V7),0]>0,127,0]+(-1114+If[x150V4>0,800+100 (-1+x150V4),0]+If[x150V5>0,400+100 (-1+x150V5),0]+If[x150V6>0,600+100 (-1+x150V6),0]+If[x150V7>0,50+5 (-1+x150V7),0]+If[x150V8>0,500+50 (-1+x150V8),0]) If[1114-If[x150V4>0,800+100 (-1+x150V4),0]-If[x150V5>0,400+100 (-1+x150V5),0]-If[x150V6>0,600+100 (-1+x150V6),0]-If[x150V7>0,50+5 (-1+x150V7),0]-If[x150V8>0,500+50 (-1+x150V8),0]>0,164,0]+(-1314+If[x151V1>0,800+100 (-1+x151V1),0]+If[x151V10>0,100+25 (-1+x151V10),0]+If[x151V2>0,300+50 (-1+x151V2),0]+If[x151V5>0,80+10 (-1+x151V5),0]+If[x151V6>0,200+25 (-1+x151V6),0]+If[x151V7>0,30+5 (-1+x151V7),0]+If[x151V9>0,200+50 (-1+x151V9),0]) If[1314-If[x151V1>0,800+100 (-1+x151V1),0]-If[x151V10>0,100+25 (-1+x151V10),0]-If[x151V2>0,300+50 (-1+x151V2),0]-If[x151V5>0,80+10 (-1+x151V5),0]-If[x151V6>0,200+25 (-1+x151V6),0]-If[x151V7>0,30+5 (-1+x151V7),0]-If[x151V9>0,200+50 (-1+x151V9),0]>0,192,0]+(-2455+If[x152V10>0,150+25 (-1+x152V10),0]+If[x152V2>0,30+5 (-1+x152V2),0]+If[x152V5>0,800+100 (-1+x152V5),0]+If[x152V7>0,300+50 (-1+x152V7),0]) If[2455-If[x152V10>0,150+25 (-1+x152V10),0]-If[x152V2>0,30+5 (-1+x152V2),0]-If[x152V5>0,800+100 (-1+x152V5),0]-If[x152V7>0,300+50 (-1+x152V7),0]>0,129,0]+(-242+If[x153V6>0,400+50 (-1+x153V6),0]+If[x153V7>0,40+10 (-1+x153V7),0]+If[x153V9>0,30+5 (-1+x153V9),0]) If[242-If[x153V6>0,400+50 (-1+x153V6),0]-If[x153V7>0,40+10 (-1+x153V7),0]-If[x153V9>0,30+5 (-1+x153V9),0]>0,171,0]+(-2638+If[x154V10>0,40+5 (-1+x154V10),0]+If[x154V2>0,800+100 (-1+x154V2),0]+If[x154V4>0,800+100 (-1+x154V4),0]+If[x154V5>0,250+25 (-1+x154V5),0]+If[x154V6>0,800+100 (-1+x154V6),0]+If[x154V7>0,800+100 (-1+x154V7),0]+If[x154V9>0,100+10 (-1+x154V9),0]) If[2638-If[x154V10>0,40+5 (-1+x154V10),0]-If[x154V2>0,800+100 (-1+x154V2),0]-If[x154V4>0,800+100 (-1+x154V4),0]-If[x154V5>0,250+25 (-1+x154V5),0]-If[x154V6>0,800+100 (-1+x154V6),0]-If[x154V7>0,800+100 (-1+x154V7),0]-If[x154V9>0,100+10 (-1+x154V9),0]>0,148,0]+(-1172+If[x155V10>0,300+50 (-1+x155V10),0]+If[x155V3>0,400+50 (-1+x155V3),0]+If[x155V5>0,20+5 (-1+x155V5),0]+If[x155V7>0,250+25 (-1+x155V7),0]+If[x155V8>0,100+10 (-1+x155V8),0]) If[1172-If[x155V10>0,300+50 (-1+x155V10),0]-If[x155V3>0,400+50 (-1+x155V3),0]-If[x155V5>0,20+5 (-1+x155V5),0]-If[x155V7>0,250+25 (-1+x155V7),0]-If[x155V8>0,100+10 (-1+x155V8),0]>0,137,0]+(-998+If[x156V1>0,100+10 (-1+x156V1),0]+If[x156V10>0,30+5 (-1+x156V10),0]+If[x156V3>0,40+10 (-1+x156V3),0]+If[x156V4>0,250+25 (-1+x156V4),0]+If[x156V6>0,20+5 (-1+x156V6),0]+If[x156V7>0,150+25 (-1+x156V7),0]+If[x156V9>0,60+10 (-1+x156V9),0]) If[998-If[x156V1>0,100+10 (-1+x156V1),0]-If[x156V10>0,30+5 (-1+x156V10),0]-If[x156V3>0,40+10 (-1+x156V3),0]-If[x156V4>0,250+25 (-1+x156V4),0]-If[x156V6>0,20+5 (-1+x156V6),0]-If[x156V7>0,150+25 (-1+x156V7),0]-If[x156V9>0,60+10 (-1+x156V9),0]>0,142,0]+(-511+If[x157V1>0,600+100 (-1+x157V1),0]+If[x157V3>0,300+50 (-1+x157V3),0]+If[x157V4>0,200+50 (-1+x157V4),0]+If[x157V5>0,200+25 (-1+x157V5),0]+If[x157V6>0,1000+100 (-1+x157V6),0]+If[x157V9>0,100+10 (-1+x157V9),0]) If[511-If[x157V1>0,600+100 (-1+x157V1),0]-If[x157V3>0,300+50 (-1+x157V3),0]-If[x157V4>0,200+50 (-1+x157V4),0]-If[x157V5>0,200+25 (-1+x157V5),0]-If[x157V6>0,1000+100 (-1+x157V6),0]-If[x157V9>0,100+10 (-1+x157V9),0]>0,179,0]+(-1176+If[x158V1>0,60+10 (-1+x158V1),0]+If[x158V10>0,30+5 (-1+x158V10),0]+If[x158V5>0,250+25 (-1+x158V5),0]+If[x158V6>0,400+50 (-1+x158V6),0]) If[1176-If[x158V1>0,60+10 (-1+x158V1),0]-If[x158V10>0,30+5 (-1+x158V10),0]-If[x158V5>0,250+25 (-1+x158V5),0]-If[x158V6>0,400+50 (-1+x158V6),0]>0,123,0]+(-2530+If[x159V10>0,400+50 (-1+x159V10),0]+If[x159V7>0,200+25 (-1+x159V7),0]) If[2530-If[x159V10>0,400+50 (-1+x159V10),0]-If[x159V7>0,200+25 (-1+x159V7),0]>0,129,0]+(-2805+If[x15V1>0,50+5 (-1+x15V1),0]+If[x15V2>0,150+25 (-1+x15V2),0]+If[x15V5>0,250+25 (-1+x15V5),0]) If[2805-If[x15V1>0,50+5 (-1+x15V1),0]-If[x15V2>0,150+25 (-1+x15V2),0]-If[x15V5>0,250+25 (-1+x15V5),0]>0,179,0]+(-1523+If[x160V2>0,50+5 (-1+x160V2),0]+If[x160V3>0,200+25 (-1+x160V3),0]+If[x160V4>0,800+100 (-1+x160V4),0]+If[x160V8>0,1000+100 (-1+x160V8),0]+If[x160V9>0,400+50 (-1+x160V9),0]) If[1523-If[x160V2>0,50+5 (-1+x160V2),0]-If[x160V3>0,200+25 (-1+x160V3),0]-If[x160V4>0,800+100 (-1+x160V4),0]-If[x160V8>0,1000+100 (-1+x160V8),0]-If[x160V9>0,400+50 (-1+x160V9),0]>0,113,0]+(-1893+If[x161V3>0,200+25 (-1+x161V3),0]+If[x161V5>0,1000+100 (-1+x161V5),0]+If[x161V9>0,50+5 (-1+x161V9),0]) If[1893-If[x161V3>0,200+25 (-1+x161V3),0]-If[x161V5>0,1000+100 (-1+x161V5),0]-If[x161V9>0,50+5 (-1+x161V9),0]>0,188,0]+(-2334+If[x162V1>0,1000+100 (-1+x162V1),0]+If[x162V10>0,600+100 (-1+x162V10),0]+If[x162V2>0,80+10 (-1+x162V2),0]+If[x162V3>0,500+50 (-1+x162V3),0]+If[x162V4>0,100+10 (-1+x162V4),0]+If[x162V6>0,80+10 (-1+x162V6),0]+If[x162V7>0,20+5 (-1+x162V7),0]+If[x162V8>0,500+50 (-1+x162V8),0]+If[x162V9>0,250+25 (-1+x162V9),0]) If[2334-If[x162V1>0,1000+100 (-1+x162V1),0]-If[x162V10>0,600+100 (-1+x162V10),0]-If[x162V2>0,80+10 (-1+x162V2),0]-If[x162V3>0,500+50 (-1+x162V3),0]-If[x162V4>0,100+10 (-1+x162V4),0]-If[x162V6>0,80+10 (-1+x162V6),0]-If[x162V7>0,20+5 (-1+x162V7),0]-If[x162V8>0,500+50 (-1+x162V8),0]-If[x162V9>0,250+25 (-1+x162V9),0]>0,115,0]+(-377+If[x163V1>0,300+50 (-1+x163V1),0]+If[x163V10>0,1000+100 (-1+x163V10),0]+If[x163V4>0,150+25 (-1+x163V4),0]+If[x163V6>0,100+25 (-1+x163V6),0]+If[x163V7>0,80+10 (-1+x163V7),0]+If[x163V8>0,300+50 (-1+x163V8),0]) If[377-If[x163V1>0,300+50 (-1+x163V1),0]-If[x163V10>0,1000+100 (-1+x163V10),0]-If[x163V4>0,150+25 (-1+x163V4),0]-If[x163V6>0,100+25 (-1+x163V6),0]-If[x163V7>0,80+10 (-1+x163V7),0]-If[x163V8>0,300+50 (-1+x163V8),0]>0,102,0]+(-1682+If[x164V1>0,150+25 (-1+x164V1),0]+If[x164V10>0,30+5 (-1+x164V10),0]+If[x164V3>0,30+5 (-1+x164V3),0]+If[x164V4>0,200+25 (-1+x164V4),0]+If[x164V5>0,250+25 (-1+x164V5),0]+If[x164V6>0,100+10 (-1+x164V6),0]+If[x164V9>0,60+10 (-1+x164V9),0]) If[1682-If[x164V1>0,150+25 (-1+x164V1),0]-If[x164V10>0,30+5 (-1+x164V10),0]-If[x164V3>0,30+5 (-1+x164V3),0]-If[x164V4>0,200+25 (-1+x164V4),0]-If[x164V5>0,250+25 (-1+x164V5),0]-If[x164V6>0,100+10 (-1+x164V6),0]-If[x164V9>0,60+10 (-1+x164V9),0]>0,194,0]+(-1543+If[x165V2>0,100+10 (-1+x165V2),0]+If[x165V3>0,100+10 (-1+x165V3),0]+If[x165V6>0,20+5 (-1+x165V6),0]+If[x165V7>0,150+25 (-1+x165V7),0]+If[x165V8>0,400+50 (-1+x165V8),0]) If[1543-If[x165V2>0,100+10 (-1+x165V2),0]-If[x165V3>0,100+10 (-1+x165V3),0]-If[x165V6>0,20+5 (-1+x165V6),0]-If[x165V7>0,150+25 (-1+x165V7),0]-If[x165V8>0,400+50 (-1+x165V8),0]>0,118,0]+(-2759+If[x166V1>0,200+25 (-1+x166V1),0]+If[x166V3>0,600+100 (-1+x166V3),0]+If[x166V4>0,800+100 (-1+x166V4),0]+If[x166V6>0,50+5 (-1+x166V6),0]) If[2759-If[x166V1>0,200+25 (-1+x166V1),0]-If[x166V3>0,600+100 (-1+x166V3),0]-If[x166V4>0,800+100 (-1+x166V4),0]-If[x166V6>0,50+5 (-1+x166V6),0]>0,165,0]+(-377+If[x167V6>0,20+5 (-1+x167V6),0]+If[x167V9>0,200+50 (-1+x167V9),0]) If[377-If[x167V6>0,20+5 (-1+x167V6),0]-If[x167V9>0,200+50 (-1+x167V9),0]>0,104,0]+(-671+If[x168V10>0,400+50 (-1+x168V10),0]+If[x168V2>0,300+50 (-1+x168V2),0]+If[x168V3>0,800+100 (-1+x168V3),0]+If[x168V5>0,500+50 (-1+x168V5),0]+If[x168V6>0,40+5 (-1+x168V6),0]+If[x168V9>0,200+25 (-1+x168V9),0]) If[671-If[x168V10>0,400+50 (-1+x168V10),0]-If[x168V2>0,300+50 (-1+x168V2),0]-If[x168V3>0,800+100 (-1+x168V3),0]-If[x168V5>0,500+50 (-1+x168V5),0]-If[x168V6>0,40+5 (-1+x168V6),0]-If[x168V9>0,200+25 (-1+x168V9),0]>0,123,0]+(-868+If[x169V2>0,800+100 (-1+x169V2),0]+If[x169V4>0,600+100 (-1+x169V4),0]+If[x169V7>0,800+100 (-1+x169V7),0]+If[x169V8>0,400+100 (-1+x169V8),0]) If[868-If[x169V2>0,800+100 (-1+x169V2),0]-If[x169V4>0,600+100 (-1+x169V4),0]-If[x169V7>0,800+100 (-1+x169V7),0]-If[x169V8>0,400+100 (-1+x169V8),0]>0,174,0]+(-2438+If[x16V1>0,150+25 (-1+x16V1),0]+If[x16V3>0,200+25 (-1+x16V3),0]) If[2438-If[x16V1>0,150+25 (-1+x16V1),0]-If[x16V3>0,200+25 (-1+x16V3),0]>0,154,0]+(-629+If[x170V2>0,60+10 (-1+x170V2),0]+If[x170V4>0,100+10 (-1+x170V4),0]+If[x170V9>0,30+5 (-1+x170V9),0]) If[629-If[x170V2>0,60+10 (-1+x170V2),0]-If[x170V4>0,100+10 (-1+x170V4),0]-If[x170V9>0,30+5 (-1+x170V9),0]>0,166,0]+(-270+If[x171V3>0,150+25 (-1+x171V3),0]+If[x171V5>0,40+10 (-1+x171V5),0]+If[x171V6>0,200+50 (-1+x171V6),0]+If[x171V7>0,40+10 (-1+x171V7),0]) If[270-If[x171V3>0,150+25 (-1+x171V3),0]-If[x171V5>0,40+10 (-1+x171V5),0]-If[x171V6>0,200+50 (-1+x171V6),0]-If[x171V7>0,40+10 (-1+x171V7),0]>0,107,0]+(-544+If[x172V1>0,40+10 (-1+x172V1),0]+If[x172V2>0,100+25 (-1+x172V2),0]+If[x172V4>0,80+10 (-1+x172V4),0]+If[x172V8>0,30+5 (-1+x172V8),0]+If[x172V9>0,1000+100 (-1+x172V9),0]) If[544-If[x172V1>0,40+10 (-1+x172V1),0]-If[x172V2>0,100+25 (-1+x172V2),0]-If[x172V4>0,80+10 (-1+x172V4),0]-If[x172V8>0,30+5 (-1+x172V8),0]-If[x172V9>0,1000+100 (-1+x172V9),0]>0,112,0]+(-2308+If[x173V10>0,200+50 (-1+x173V10),0]+If[x173V2>0,200+50 (-1+x173V2),0]+If[x173V3>0,400+50 (-1+x173V3),0]+If[x173V4>0,40+5 (-1+x173V4),0]+If[x173V6>0,20+5 (-1+x173V6),0]) If[2308-If[x173V10>0,200+50 (-1+x173V10),0]-If[x173V2>0,200+50 (-1+x173V2),0]-If[x173V3>0,400+50 (-1+x173V3),0]-If[x173V4>0,40+5 (-1+x173V4),0]-If[x173V6>0,20+5 (-1+x173V6),0]>0,113,0]+(-153+If[x174V1>0,250+25 (-1+x174V1),0]+If[x174V10>0,300+50 (-1+x174V10),0]+If[x174V2>0,30+5 (-1+x174V2),0]+If[x174V3>0,40+10 (-1+x174V3),0]+If[x174V4>0,150+25 (-1+x174V4),0]+If[x174V6>0,100+25 (-1+x174V6),0]+If[x174V7>0,100+25 (-1+x174V7),0]+If[x174V8>0,60+10 (-1+x174V8),0]) If[153-If[x174V1>0,250+25 (-1+x174V1),0]-If[x174V10>0,300+50 (-1+x174V10),0]-If[x174V2>0,30+5 (-1+x174V2),0]-If[x174V3>0,40+10 (-1+x174V3),0]-If[x174V4>0,150+25 (-1+x174V4),0]-If[x174V6>0,100+25 (-1+x174V6),0]-If[x174V7>0,100+25 (-1+x174V7),0]-If[x174V8>0,60+10 (-1+x174V8),0]>0,105,0]+(-1394+If[x175V10>0,40+5 (-1+x175V10),0]+If[x175V2>0,20+5 (-1+x175V2),0]+If[x175V3>0,150+25 (-1+x175V3),0]+If[x175V4>0,30+5 (-1+x175V4),0]+If[x175V5>0,200+25 (-1+x175V5),0]+If[x175V6>0,400+100 (-1+x175V6),0]) If[1394-If[x175V10>0,40+5 (-1+x175V10),0]-If[x175V2>0,20+5 (-1+x175V2),0]-If[x175V3>0,150+25 (-1+x175V3),0]-If[x175V4>0,30+5 (-1+x175V4),0]-If[x175V5>0,200+25 (-1+x175V5),0]-If[x175V6>0,400+100 (-1+x175V6),0]>0,198,0]+(-957+If[x176V3>0,80+10 (-1+x176V3),0]+If[x176V6>0,200+25 (-1+x176V6),0]) If[957-If[x176V3>0,80+10 (-1+x176V3),0]-If[x176V6>0,200+25 (-1+x176V6),0]>0,121,0]+(-192+If[x177V2>0,800+100 (-1+x177V2),0]+If[x177V7>0,50+5 (-1+x177V7),0]) If[192-If[x177V2>0,800+100 (-1+x177V2),0]-If[x177V7>0,50+5 (-1+x177V7),0]>0,193,0]+(-1284+If[x178V1>0,30+5 (-1+x178V1),0]+If[x178V10>0,1000+100 (-1+x178V10),0]+If[x178V2>0,50+5 (-1+x178V2),0]+If[x178V3>0,80+10 (-1+x178V3),0]+If[x178V4>0,20+5 (-1+x178V4),0]+If[x178V5>0,300+50 (-1+x178V5),0]+If[x178V7>0,50+5 (-1+x178V7),0]) If[1284-If[x178V1>0,30+5 (-1+x178V1),0]-If[x178V10>0,1000+100 (-1+x178V10),0]-If[x178V2>0,50+5 (-1+x178V2),0]-If[x178V3>0,80+10 (-1+x178V3),0]-If[x178V4>0,20+5 (-1+x178V4),0]-If[x178V5>0,300+50 (-1+x178V5),0]-If[x178V7>0,50+5 (-1+x178V7),0]>0,139,0]+(-1353+If[x179V6>0,20+5 (-1+x179V6),0]+If[x179V7>0,100+10 (-1+x179V7),0]+If[x179V9>0,40+5 (-1+x179V9),0]) If[1353-If[x179V6>0,20+5 (-1+x179V6),0]-If[x179V7>0,100+10 (-1+x179V7),0]-If[x179V9>0,40+5 (-1+x179V9),0]>0,190,0]+(-2209+If[x17V10>0,400+100 (-1+x17V10),0]+If[x17V2>0,400+100 (-1+x17V2),0]+If[x17V3>0,30+5 (-1+x17V3),0]+If[x17V4>0,200+50 (-1+x17V4),0]+If[x17V5>0,250+25 (-1+x17V5),0]+If[x17V6>0,300+50 (-1+x17V6),0]+If[x17V8>0,30+5 (-1+x17V8),0]+If[x17V9>0,150+25 (-1+x17V9),0]) If[2209-If[x17V10>0,400+100 (-1+x17V10),0]-If[x17V2>0,400+100 (-1+x17V2),0]-If[x17V3>0,30+5 (-1+x17V3),0]-If[x17V4>0,200+50 (-1+x17V4),0]-If[x17V5>0,250+25 (-1+x17V5),0]-If[x17V6>0,300+50 (-1+x17V6),0]-If[x17V8>0,30+5 (-1+x17V8),0]-If[x17V9>0,150+25 (-1+x17V9),0]>0,149,0]+(-910+If[x180V4>0,100+10 (-1+x180V4),0]+If[x180V6>0,150+25 (-1+x180V6),0]+If[x180V7>0,250+25 (-1+x180V7),0]+If[x180V8>0,40+5 (-1+x180V8),0]+If[x180V9>0,20+5 (-1+x180V9),0]) If[910-If[x180V4>0,100+10 (-1+x180V4),0]-If[x180V6>0,150+25 (-1+x180V6),0]-If[x180V7>0,250+25 (-1+x180V7),0]-If[x180V8>0,40+5 (-1+x180V8),0]-If[x180V9>0,20+5 (-1+x180V9),0]>0,147,0]+(-897+If[x181V1>0,1000+100 (-1+x181V1),0]+If[x181V10>0,800+100 (-1+x181V10),0]+If[x181V5>0,250+25 (-1+x181V5),0]+If[x181V8>0,1000+100 (-1+x181V8),0]) If[897-If[x181V1>0,1000+100 (-1+x181V1),0]-If[x181V10>0,800+100 (-1+x181V10),0]-If[x181V5>0,250+25 (-1+x181V5),0]-If[x181V8>0,1000+100 (-1+x181V8),0]>0,117,0]+(-2919+If[x182V1>0,200+50 (-1+x182V1),0]+If[x182V10>0,30+5 (-1+x182V10),0]+If[x182V2>0,30+5 (-1+x182V2),0]+If[x182V4>0,400+50 (-1+x182V4),0]+If[x182V8>0,1000+100 (-1+x182V8),0]+If[x182V9>0,800+100 (-1+x182V9),0]) If[2919-If[x182V1>0,200+50 (-1+x182V1),0]-If[x182V10>0,30+5 (-1+x182V10),0]-If[x182V2>0,30+5 (-1+x182V2),0]-If[x182V4>0,400+50 (-1+x182V4),0]-If[x182V8>0,1000+100 (-1+x182V8),0]-If[x182V9>0,800+100 (-1+x182V9),0]>0,144,0]+(-1350+If[x183V1>0,600+100 (-1+x183V1),0]+If[x183V2>0,400+100 (-1+x183V2),0]+If[x183V4>0,1000+100 (-1+x183V4),0]+If[x183V8>0,40+10 (-1+x183V8),0]+If[x183V9>0,200+50 (-1+x183V9),0]) If[1350-If[x183V1>0,600+100 (-1+x183V1),0]-If[x183V2>0,400+100 (-1+x183V2),0]-If[x183V4>0,1000+100 (-1+x183V4),0]-If[x183V8>0,40+10 (-1+x183V8),0]-If[x183V9>0,200+50 (-1+x183V9),0]>0,195,0]+(-2914+If[x184V1>0,100+25 (-1+x184V1),0]+If[x184V10>0,50+5 (-1+x184V10),0]+If[x184V2>0,600+100 (-1+x184V2),0]+If[x184V4>0,250+25 (-1+x184V4),0]+If[x184V5>0,200+50 (-1+x184V5),0]+If[x184V6>0,30+5 (-1+x184V6),0]+If[x184V9>0,300+50 (-1+x184V9),0]) If[2914-If[x184V1>0,100+25 (-1+x184V1),0]-If[x184V10>0,50+5 (-1+x184V10),0]-If[x184V2>0,600+100 (-1+x184V2),0]-If[x184V4>0,250+25 (-1+x184V4),0]-If[x184V5>0,200+50 (-1+x184V5),0]-If[x184V6>0,30+5 (-1+x184V6),0]-If[x184V9>0,300+50 (-1+x184V9),0]>0,131,0]+(-1953+If[x185V1>0,400+100 (-1+x185V1),0]+If[x185V2>0,30+5 (-1+x185V2),0]) If[1953-If[x185V1>0,400+100 (-1+x185V1),0]-If[x185V2>0,30+5 (-1+x185V2),0]>0,128,0]+(-26+If[x186V1>0,150+25 (-1+x186V1),0]+If[x186V10>0,400+50 (-1+x186V10),0]+If[x186V2>0,100+25 (-1+x186V2),0]+If[x186V3>0,400+100 (-1+x186V3),0]+If[x186V4>0,500+50 (-1+x186V4),0]+If[x186V8>0,500+50 (-1+x186V8),0]+If[x186V9>0,80+10 (-1+x186V9),0]) If[26-If[x186V1>0,150+25 (-1+x186V1),0]-If[x186V10>0,400+50 (-1+x186V10),0]-If[x186V2>0,100+25 (-1+x186V2),0]-If[x186V3>0,400+100 (-1+x186V3),0]-If[x186V4>0,500+50 (-1+x186V4),0]-If[x186V8>0,500+50 (-1+x186V8),0]-If[x186V9>0,80+10 (-1+x186V9),0]>0,199,0]+(-2814+If[x187V10>0,100+25 (-1+x187V10),0]+If[x187V3>0,1000+100 (-1+x187V3),0]+If[x187V6>0,40+10 (-1+x187V6),0]+If[x187V7>0,30+5 (-1+x187V7),0]+If[x187V9>0,400+100 (-1+x187V9),0]) If[2814-If[x187V10>0,100+25 (-1+x187V10),0]-If[x187V3>0,1000+100 (-1+x187V3),0]-If[x187V6>0,40+10 (-1+x187V6),0]-If[x187V7>0,30+5 (-1+x187V7),0]-If[x187V9>0,400+100 (-1+x187V9),0]>0,174,0]+(-1494+If[x188V10>0,250+25 (-1+x188V10),0]+If[x188V4>0,800+100 (-1+x188V4),0]+If[x188V5>0,80+10 (-1+x188V5),0]+If[x188V6>0,100+25 (-1+x188V6),0]+If[x188V7>0,40+10 (-1+x188V7),0]+If[x188V8>0,500+50 (-1+x188V8),0]+If[x188V9>0,80+10 (-1+x188V9),0]) If[1494-If[x188V10>0,250+25 (-1+x188V10),0]-If[x188V4>0,800+100 (-1+x188V4),0]-If[x188V5>0,80+10 (-1+x188V5),0]-If[x188V6>0,100+25 (-1+x188V6),0]-If[x188V7>0,40+10 (-1+x188V7),0]-If[x188V8>0,500+50 (-1+x188V8),0]-If[x188V9>0,80+10 (-1+x188V9),0]>0,145,0]+(-2348+If[x189V1>0,400+50 (-1+x189V1),0]+If[x189V10>0,300+50 (-1+x189V10),0]+If[x189V2>0,60+10 (-1+x189V2),0]+If[x189V4>0,800+100 (-1+x189V4),0]+If[x189V6>0,50+5 (-1+x189V6),0]) If[2348-If[x189V1>0,400+50 (-1+x189V1),0]-If[x189V10>0,300+50 (-1+x189V10),0]-If[x189V2>0,60+10 (-1+x189V2),0]-If[x189V4>0,800+100 (-1+x189V4),0]-If[x189V6>0,50+5 (-1+x189V6),0]>0,163,0]+(-2438+If[x18V1>0,400+100 (-1+x18V1),0]+If[x18V10>0,40+5 (-1+x18V10),0]+If[x18V2>0,20+5 (-1+x18V2),0]+If[x18V3>0,60+10 (-1+x18V3),0]+If[x18V4>0,150+25 (-1+x18V4),0]+If[x18V5>0,400+100 (-1+x18V5),0]+If[x18V6>0,200+25 (-1+x18V6),0]+If[x18V7>0,200+50 (-1+x18V7),0]) If[2438-If[x18V1>0,400+100 (-1+x18V1),0]-If[x18V10>0,40+5 (-1+x18V10),0]-If[x18V2>0,20+5 (-1+x18V2),0]-If[x18V3>0,60+10 (-1+x18V3),0]-If[x18V4>0,150+25 (-1+x18V4),0]-If[x18V5>0,400+100 (-1+x18V5),0]-If[x18V6>0,200+25 (-1+x18V6),0]-If[x18V7>0,200+50 (-1+x18V7),0]>0,119,0]+(-1861+If[x190V1>0,40+10 (-1+x190V1),0]+If[x190V2>0,50+5 (-1+x190V2),0]+If[x190V3>0,40+5 (-1+x190V3),0]+If[x190V4>0,20+5 (-1+x190V4),0]+If[x190V6>0,500+50 (-1+x190V6),0]+If[x190V8>0,80+10 (-1+x190V8),0]) If[1861-If[x190V1>0,40+10 (-1+x190V1),0]-If[x190V2>0,50+5 (-1+x190V2),0]-If[x190V3>0,40+5 (-1+x190V3),0]-If[x190V4>0,20+5 (-1+x190V4),0]-If[x190V6>0,500+50 (-1+x190V6),0]-If[x190V8>0,80+10 (-1+x190V8),0]>0,108,0]+(-2611+If[x191V1>0,400+50 (-1+x191V1),0]+If[x191V2>0,300+50 (-1+x191V2),0]+If[x191V4>0,250+25 (-1+x191V4),0]+If[x191V8>0,40+10 (-1+x191V8),0]+If[x191V9>0,50+5 (-1+x191V9),0]) If[2611-If[x191V1>0,400+50 (-1+x191V1),0]-If[x191V2>0,300+50 (-1+x191V2),0]-If[x191V4>0,250+25 (-1+x191V4),0]-If[x191V8>0,40+10 (-1+x191V8),0]-If[x191V9>0,50+5 (-1+x191V9),0]>0,169,0]+(-927+If[x192V1>0,100+10 (-1+x192V1),0]+If[x192V3>0,1000+100 (-1+x192V3),0]+If[x192V4>0,40+5 (-1+x192V4),0]+If[x192V5>0,50+5 (-1+x192V5),0]+If[x192V6>0,300+50 (-1+x192V6),0]+If[x192V7>0,200+25 (-1+x192V7),0]+If[x192V9>0,40+10 (-1+x192V9),0]) If[927-If[x192V1>0,100+10 (-1+x192V1),0]-If[x192V3>0,1000+100 (-1+x192V3),0]-If[x192V4>0,40+5 (-1+x192V4),0]-If[x192V5>0,50+5 (-1+x192V5),0]-If[x192V6>0,300+50 (-1+x192V6),0]-If[x192V7>0,200+25 (-1+x192V7),0]-If[x192V9>0,40+10 (-1+x192V9),0]>0,172,0]+(-2943+If[x193V1>0,200+25 (-1+x193V1),0]+If[x193V10>0,400+100 (-1+x193V10),0]+If[x193V5>0,100+10 (-1+x193V5),0]+If[x193V6>0,600+100 (-1+x193V6),0]+If[x193V7>0,60+10 (-1+x193V7),0]+If[x193V9>0,50+5 (-1+x193V9),0]) If[2943-If[x193V1>0,200+25 (-1+x193V1),0]-If[x193V10>0,400+100 (-1+x193V10),0]-If[x193V5>0,100+10 (-1+x193V5),0]-If[x193V6>0,600+100 (-1+x193V6),0]-If[x193V7>0,60+10 (-1+x193V7),0]-If[x193V9>0,50+5 (-1+x193V9),0]>0,142,0]+(-2818+If[x194V1>0,250+25 (-1+x194V1),0]+If[x194V10>0,1000+100 (-1+x194V10),0]+If[x194V3>0,80+10 (-1+x194V3),0]+If[x194V7>0,500+50 (-1+x194V7),0]) If[2818-If[x194V1>0,250+25 (-1+x194V1),0]-If[x194V10>0,1000+100 (-1+x194V10),0]-If[x194V3>0,80+10 (-1+x194V3),0]-If[x194V7>0,500+50 (-1+x194V7),0]>0,118,0]+(-1320+If[x195V10>0,60+10 (-1+x195V10),0]+If[x195V2>0,50+5 (-1+x195V2),0]+If[x195V4>0,200+50 (-1+x195V4),0]+If[x195V5>0,300+50 (-1+x195V5),0]+If[x195V8>0,400+100 (-1+x195V8),0]+If[x195V9>0,500+50 (-1+x195V9),0]) If[1320-If[x195V10>0,60+10 (-1+x195V10),0]-If[x195V2>0,50+5 (-1+x195V2),0]-If[x195V4>0,200+50 (-1+x195V4),0]-If[x195V5>0,300+50 (-1+x195V5),0]-If[x195V8>0,400+100 (-1+x195V8),0]-If[x195V9>0,500+50 (-1+x195V9),0]>0,105,0]+(-712+If[x196V10>0,100+10 (-1+x196V10),0]+If[x196V2>0,500+50 (-1+x196V2),0]+If[x196V3>0,400+100 (-1+x196V3),0]+If[x196V5>0,150+25 (-1+x196V5),0]+If[x196V7>0,1000+100 (-1+x196V7),0]) If[712-If[x196V10>0,100+10 (-1+x196V10),0]-If[x196V2>0,500+50 (-1+x196V2),0]-If[x196V3>0,400+100 (-1+x196V3),0]-If[x196V5>0,150+25 (-1+x196V5),0]-If[x196V7>0,1000+100 (-1+x196V7),0]>0,199,0]+(-410+If[x197V10>0,400+100 (-1+x197V10),0]+If[x197V7>0,1000+100 (-1+x197V7),0]) If[410-If[x197V10>0,400+100 (-1+x197V10),0]-If[x197V7>0,1000+100 (-1+x197V7),0]>0,167,0]+(-949+If[x198V1>0,40+10 (-1+x198V1),0]+If[x198V10>0,50+5 (-1+x198V10),0]+If[x198V2>0,50+5 (-1+x198V2),0]+If[x198V5>0,30+5 (-1+x198V5),0]+If[x198V6>0,400+100 (-1+x198V6),0]+If[x198V9>0,800+100 (-1+x198V9),0]) If[949-If[x198V1>0,40+10 (-1+x198V1),0]-If[x198V10>0,50+5 (-1+x198V10),0]-If[x198V2>0,50+5 (-1+x198V2),0]-If[x198V5>0,30+5 (-1+x198V5),0]-If[x198V6>0,400+100 (-1+x198V6),0]-If[x198V9>0,800+100 (-1+x198V9),0]>0,200,0]+(-1925+If[x199V1>0,1000+100 (-1+x199V1),0]+If[x199V10>0,80+10 (-1+x199V10),0]+If[x199V4>0,600+100 (-1+x199V4),0]+If[x199V5>0,400+50 (-1+x199V5),0]+If[x199V7>0,100+10 (-1+x199V7),0]+If[x199V8>0,500+50 (-1+x199V8),0]+If[x199V9>0,100+10 (-1+x199V9),0]) If[1925-If[x199V1>0,1000+100 (-1+x199V1),0]-If[x199V10>0,80+10 (-1+x199V10),0]-If[x199V4>0,600+100 (-1+x199V4),0]-If[x199V5>0,400+50 (-1+x199V5),0]-If[x199V7>0,100+10 (-1+x199V7),0]-If[x199V8>0,500+50 (-1+x199V8),0]-If[x199V9>0,100+10 (-1+x199V9),0]>0,133,0]+(-2015+If[x19V10>0,100+25 (-1+x19V10),0]+If[x19V3>0,600+100 (-1+x19V3),0]+If[x19V5>0,800+100 (-1+x19V5),0]+If[x19V8>0,50+5 (-1+x19V8),0]+If[x19V9>0,1000+100 (-1+x19V9),0]) If[2015-If[x19V10>0,100+25 (-1+x19V10),0]-If[x19V3>0,600+100 (-1+x19V3),0]-If[x19V5>0,800+100 (-1+x19V5),0]-If[x19V8>0,50+5 (-1+x19V8),0]-If[x19V9>0,1000+100 (-1+x19V9),0]>0,143,0]+(-193+If[x1V5>0,200+50 (-1+x1V5),0]+If[x1V6>0,20+5 (-1+x1V6),0]+If[x1V7>0,80+10 (-1+x1V7),0]) If[193-If[x1V5>0,200+50 (-1+x1V5),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x1V7>0,80+10 (-1+x1V7),0]>0,148,0]+(-829+If[x200V10>0,250+25 (-1+x200V10),0]+If[x200V2>0,200+50 (-1+x200V2),0]+If[x200V3>0,100+10 (-1+x200V3),0]+If[x200V4>0,150+25 (-1+x200V4),0]+If[x200V5>0,400+100 (-1+x200V5),0]+If[x200V6>0,20+5 (-1+x200V6),0]) If[829-If[x200V10>0,250+25 (-1+x200V10),0]-If[x200V2>0,200+50 (-1+x200V2),0]-If[x200V3>0,100+10 (-1+x200V3),0]-If[x200V4>0,150+25 (-1+x200V4),0]-If[x200V5>0,400+100 (-1+x200V5),0]-If[x200V6>0,20+5 (-1+x200V6),0]>0,165,0]+(-1391+If[x20V1>0,600+100 (-1+x20V1),0]+If[x20V10>0,20+5 (-1+x20V10),0]+If[x20V2>0,30+5 (-1+x20V2),0]+If[x20V3>0,100+25 (-1+x20V3),0]+If[x20V6>0,250+25 (-1+x20V6),0]+If[x20V8>0,400+100 (-1+x20V8),0]) If[1391-If[x20V1>0,600+100 (-1+x20V1),0]-If[x20V10>0,20+5 (-1+x20V10),0]-If[x20V2>0,30+5 (-1+x20V2),0]-If[x20V3>0,100+25 (-1+x20V3),0]-If[x20V6>0,250+25 (-1+x20V6),0]-If[x20V8>0,400+100 (-1+x20V8),0]>0,137,0]+(-357+If[x21V3>0,600+100 (-1+x21V3),0]+If[x21V5>0,100+25 (-1+x21V5),0]+If[x21V6>0,50+5 (-1+x21V6),0]+If[x21V7>0,300+50 (-1+x21V7),0]) If[357-If[x21V3>0,600+100 (-1+x21V3),0]-If[x21V5>0,100+25 (-1+x21V5),0]-If[x21V6>0,50+5 (-1+x21V6),0]-If[x21V7>0,300+50 (-1+x21V7),0]>0,196,0]+(-573+If[x22V1>0,50+5 (-1+x22V1),0]+If[x22V10>0,50+5 (-1+x22V10),0]+If[x22V3>0,150+25 (-1+x22V3),0]+If[x22V4>0,800+100 (-1+x22V4),0]+If[x22V6>0,20+5 (-1+x22V6),0]) If[573-If[x22V1>0,50+5 (-1+x22V1),0]-If[x22V10>0,50+5 (-1+x22V10),0]-If[x22V3>0,150+25 (-1+x22V3),0]-If[x22V4>0,800+100 (-1+x22V4),0]-If[x22V6>0,20+5 (-1+x22V6),0]>0,167,0]+(-1992+If[x23V4>0,250+25 (-1+x23V4),0]+If[x23V5>0,300+50 (-1+x23V5),0]) If[1992-If[x23V4>0,250+25 (-1+x23V4),0]-If[x23V5>0,300+50 (-1+x23V5),0]>0,179,0]+(-428+If[x24V1>0,500+50 (-1+x24V1),0]+If[x24V10>0,500+50 (-1+x24V10),0]+If[x24V3>0,150+25 (-1+x24V3),0]+If[x24V4>0,500+50 (-1+x24V4),0]+If[x24V5>0,200+50 (-1+x24V5),0]+If[x24V7>0,300+50 (-1+x24V7),0]+If[x24V9>0,200+50 (-1+x24V9),0]) If[428-If[x24V1>0,500+50 (-1+x24V1),0]-If[x24V10>0,500+50 (-1+x24V10),0]-If[x24V3>0,150+25 (-1+x24V3),0]-If[x24V4>0,500+50 (-1+x24V4),0]-If[x24V5>0,200+50 (-1+x24V5),0]-If[x24V7>0,300+50 (-1+x24V7),0]-If[x24V9>0,200+50 (-1+x24V9),0]>0,167,0]+(-86+If[x25V2>0,1000+100 (-1+x25V2),0]+If[x25V4>0,80+10 (-1+x25V4),0]+If[x25V7>0,50+5 (-1+x25V7),0]+If[x25V9>0,600+100 (-1+x25V9),0]) If[86-If[x25V2>0,1000+100 (-1+x25V2),0]-If[x25V4>0,80+10 (-1+x25V4),0]-If[x25V7>0,50+5 (-1+x25V7),0]-If[x25V9>0,600+100 (-1+x25V9),0]>0,141,0]+(-1145+If[x26V1>0,50+5 (-1+x26V1),0]+If[x26V6>0,30+5 (-1+x26V6),0]+If[x26V7>0,400+50 (-1+x26V7),0]+If[x26V9>0,20+5 (-1+x26V9),0]) If[1145-If[x26V1>0,50+5 (-1+x26V1),0]-If[x26V6>0,30+5 (-1+x26V6),0]-If[x26V7>0,400+50 (-1+x26V7),0]-If[x26V9>0,20+5 (-1+x26V9),0]>0,180,0]+(-508+If[x27V2>0,200+25 (-1+x27V2),0]+If[x27V4>0,50+5 (-1+x27V4),0]+If[x27V5>0,250+25 (-1+x27V5),0]) If[508-If[x27V2>0,200+25 (-1+x27V2),0]-If[x27V4>0,50+5 (-1+x27V4),0]-If[x27V5>0,250+25 (-1+x27V5),0]>0,173,0]+(-2013+If[x28V1>0,500+50 (-1+x28V1),0]+If[x28V6>0,40+10 (-1+x28V6),0]+If[x28V7>0,50+5 (-1+x28V7),0]+If[x28V8>0,400+50 (-1+x28V8),0]+If[x28V9>0,50+5 (-1+x28V9),0]) If[2013-If[x28V1>0,500+50 (-1+x28V1),0]-If[x28V6>0,40+10 (-1+x28V6),0]-If[x28V7>0,50+5 (-1+x28V7),0]-If[x28V8>0,400+50 (-1+x28V8),0]-If[x28V9>0,50+5 (-1+x28V9),0]>0,156,0]+(-1858+If[x29V1>0,200+50 (-1+x29V1),0]+If[x29V2>0,150+25 (-1+x29V2),0]+If[x29V3>0,500+50 (-1+x29V3),0]+If[x29V4>0,200+25 (-1+x29V4),0]+If[x29V5>0,600+100 (-1+x29V5),0]+If[x29V8>0,100+25 (-1+x29V8),0]+If[x29V9>0,600+100 (-1+x29V9),0]) If[1858-If[x29V1>0,200+50 (-1+x29V1),0]-If[x29V2>0,150+25 (-1+x29V2),0]-If[x29V3>0,500+50 (-1+x29V3),0]-If[x29V4>0,200+25 (-1+x29V4),0]-If[x29V5>0,600+100 (-1+x29V5),0]-If[x29V8>0,100+25 (-1+x29V8),0]-If[x29V9>0,600+100 (-1+x29V9),0]>0,123,0]+(-1161+If[x2V10>0,400+50 (-1+x2V10),0]+If[x2V2>0,1000+100 (-1+x2V2),0]+If[x2V5>0,60+10 (-1+x2V5),0]+If[x2V6>0,40+10 (-1+x2V6),0]+If[x2V7>0,500+50 (-1+x2V7),0]+If[x2V9>0,30+5 (-1+x2V9),0]) If[1161-If[x2V10>0,400+50 (-1+x2V10),0]-If[x2V2>0,1000+100 (-1+x2V2),0]-If[x2V5>0,60+10 (-1+x2V5),0]-If[x2V6>0,40+10 (-1+x2V6),0]-If[x2V7>0,500+50 (-1+x2V7),0]-If[x2V9>0,30+5 (-1+x2V9),0]>0,105,0]+(-1095+If[x30V1>0,1000+100 (-1+x30V1),0]+If[x30V4>0,100+25 (-1+x30V4),0]) If[1095-If[x30V1>0,1000+100 (-1+x30V1),0]-If[x30V4>0,100+25 (-1+x30V4),0]>0,193,0]+(-718+If[x31V1>0,1000+100 (-1+x31V1),0]+If[x31V10>0,600+100 (-1+x31V10),0]+If[x31V3>0,40+5 (-1+x31V3),0]+If[x31V5>0,500+50 (-1+x31V5),0]+If[x31V6>0,500+50 (-1+x31V6),0]+If[x31V7>0,150+25 (-1+x31V7),0]) If[718-If[x31V1>0,1000+100 (-1+x31V1),0]-If[x31V10>0,600+100 (-1+x31V10),0]-If[x31V3>0,40+5 (-1+x31V3),0]-If[x31V5>0,500+50 (-1+x31V5),0]-If[x31V6>0,500+50 (-1+x31V6),0]-If[x31V7>0,150+25 (-1+x31V7),0]>0,133,0]+(-2522+If[x32V1>0,400+100 (-1+x32V1),0]+If[x32V10>0,20+5 (-1+x32V10),0]+If[x32V3>0,1000+100 (-1+x32V3),0]+If[x32V8>0,600+100 (-1+x32V8),0]+If[x32V9>0,400+50 (-1+x32V9),0]) If[2522-If[x32V1>0,400+100 (-1+x32V1),0]-If[x32V10>0,20+5 (-1+x32V10),0]-If[x32V3>0,1000+100 (-1+x32V3),0]-If[x32V8>0,600+100 (-1+x32V8),0]-If[x32V9>0,400+50 (-1+x32V9),0]>0,177,0]+(-1651+If[x33V10>0,40+5 (-1+x33V10),0]+If[x33V7>0,40+5 (-1+x33V7),0]) If[1651-If[x33V10>0,40+5 (-1+x33V10),0]-If[x33V7>0,40+5 (-1+x33V7),0]>0,187,0]+(-1424+If[x34V10>0,40+5 (-1+x34V10),0]+If[x34V4>0,40+5 (-1+x34V4),0]+If[x34V6>0,60+10 (-1+x34V6),0]+If[x34V7>0,80+10 (-1+x34V7),0]+If[x34V8>0,30+5 (-1+x34V8),0]) If[1424-If[x34V10>0,40+5 (-1+x34V10),0]-If[x34V4>0,40+5 (-1+x34V4),0]-If[x34V6>0,60+10 (-1+x34V6),0]-If[x34V7>0,80+10 (-1+x34V7),0]-If[x34V8>0,30+5 (-1+x34V8),0]>0,156,0]+(-2618+If[x35V3>0,200+50 (-1+x35V3),0]+If[x35V4>0,100+10 (-1+x35V4),0]+If[x35V8>0,200+25 (-1+x35V8),0]) If[2618-If[x35V3>0,200+50 (-1+x35V3),0]-If[x35V4>0,100+10 (-1+x35V4),0]-If[x35V8>0,200+25 (-1+x35V8),0]>0,169,0]+(-897+If[x36V1>0,500+50 (-1+x36V1),0]+If[x36V2>0,400+50 (-1+x36V2),0]+If[x36V3>0,200+50 (-1+x36V3),0]+If[x36V4>0,250+25 (-1+x36V4),0]+If[x36V6>0,50+5 (-1+x36V6),0]+If[x36V9>0,40+5 (-1+x36V9),0]) If[897-If[x36V1>0,500+50 (-1+x36V1),0]-If[x36V2>0,400+50 (-1+x36V2),0]-If[x36V3>0,200+50 (-1+x36V3),0]-If[x36V4>0,250+25 (-1+x36V4),0]-If[x36V6>0,50+5 (-1+x36V6),0]-If[x36V9>0,40+5 (-1+x36V9),0]>0,125,0]+(-2855+If[x37V2>0,20+5 (-1+x37V2),0]+If[x37V3>0,30+5 (-1+x37V3),0]+If[x37V9>0,60+10 (-1+x37V9),0]) If[2855-If[x37V2>0,20+5 (-1+x37V2),0]-If[x37V3>0,30+5 (-1+x37V3),0]-If[x37V9>0,60+10 (-1+x37V9),0]>0,150,0]+(-883+If[x38V8>0,40+5 (-1+x38V8),0]+If[x38V9>0,200+25 (-1+x38V9),0]) If[883-If[x38V8>0,40+5 (-1+x38V8),0]-If[x38V9>0,200+25 (-1+x38V9),0]>0,123,0]+(-1292+If[x39V2>0,100+25 (-1+x39V2),0]+If[x39V4>0,200+25 (-1+x39V4),0]+If[x39V8>0,200+25 (-1+x39V8),0]+If[x39V9>0,80+10 (-1+x39V9),0]) If[1292-If[x39V2>0,100+25 (-1+x39V2),0]-If[x39V4>0,200+25 (-1+x39V4),0]-If[x39V8>0,200+25 (-1+x39V8),0]-If[x39V9>0,80+10 (-1+x39V9),0]>0,101,0]+(-654+If[x3V1>0,20+5 (-1+x3V1),0]+If[x3V3>0,400+100 (-1+x3V3),0]) If[654-If[x3V1>0,20+5 (-1+x3V1),0]-If[x3V3>0,400+100 (-1+x3V3),0]>0,150,0]+(-1910+If[x40V1>0,30+5 (-1+x40V1),0]+If[x40V9>0,1000+100 (-1+x40V9),0]) If[1910-If[x40V1>0,30+5 (-1+x40V1),0]-If[x40V9>0,1000+100 (-1+x40V9),0]>0,114,0]+(-2920+If[x41V1>0,200+25 (-1+x41V1),0]+If[x41V10>0,20+5 (-1+x41V10),0]+If[x41V3>0,300+50 (-1+x41V3),0]+If[x41V5>0,100+10 (-1+x41V5),0]+If[x41V6>0,800+100 (-1+x41V6),0]+If[x41V7>0,600+100 (-1+x41V7),0]+If[x41V8>0,60+10 (-1+x41V8),0]+If[x41V9>0,40+10 (-1+x41V9),0]) If[2920-If[x41V1>0,200+25 (-1+x41V1),0]-If[x41V10>0,20+5 (-1+x41V10),0]-If[x41V3>0,300+50 (-1+x41V3),0]-If[x41V5>0,100+10 (-1+x41V5),0]-If[x41V6>0,800+100 (-1+x41V6),0]-If[x41V7>0,600+100 (-1+x41V7),0]-If[x41V8>0,60+10 (-1+x41V8),0]-If[x41V9>0,40+10 (-1+x41V9),0]>0,169,0]+(-2758+If[x42V1>0,1000+100 (-1+x42V1),0]+If[x42V10>0,100+25 (-1+x42V10),0]+If[x42V3>0,500+50 (-1+x42V3),0]+If[x42V4>0,1000+100 (-1+x42V4),0]+If[x42V6>0,800+100 (-1+x42V6),0]+If[x42V7>0,80+10 (-1+x42V7),0]+If[x42V9>0,200+25 (-1+x42V9),0]) If[2758-If[x42V1>0,1000+100 (-1+x42V1),0]-If[x42V10>0,100+25 (-1+x42V10),0]-If[x42V3>0,500+50 (-1+x42V3),0]-If[x42V4>0,1000+100 (-1+x42V4),0]-If[x42V6>0,800+100 (-1+x42V6),0]-If[x42V7>0,80+10 (-1+x42V7),0]-If[x42V9>0,200+25 (-1+x42V9),0]>0,181,0]+(-906+If[x43V1>0,100+25 (-1+x43V1),0]+If[x43V10>0,300+50 (-1+x43V10),0]+If[x43V3>0,150+25 (-1+x43V3),0]+If[x43V4>0,800+100 (-1+x43V4),0]+If[x43V5>0,40+10 (-1+x43V5),0]+If[x43V8>0,200+50 (-1+x43V8),0]) If[906-If[x43V1>0,100+25 (-1+x43V1),0]-If[x43V10>0,300+50 (-1+x43V10),0]-If[x43V3>0,150+25 (-1+x43V3),0]-If[x43V4>0,800+100 (-1+x43V4),0]-If[x43V5>0,40+10 (-1+x43V5),0]-If[x43V8>0,200+50 (-1+x43V8),0]>0,145,0]+(-2681+If[x44V10>0,500+50 (-1+x44V10),0]+If[x44V2>0,400+50 (-1+x44V2),0]+If[x44V4>0,40+10 (-1+x44V4),0]+If[x44V6>0,80+10 (-1+x44V6),0]+If[x44V7>0,300+50 (-1+x44V7),0]) If[2681-If[x44V10>0,500+50 (-1+x44V10),0]-If[x44V2>0,400+50 (-1+x44V2),0]-If[x44V4>0,40+10 (-1+x44V4),0]-If[x44V6>0,80+10 (-1+x44V6),0]-If[x44V7>0,300+50 (-1+x44V7),0]>0,128,0]+(-1270+If[x45V1>0,600+100 (-1+x45V1),0]+If[x45V10>0,400+50 (-1+x45V10),0]+If[x45V3>0,100+10 (-1+x45V3),0]+If[x45V8>0,40+10 (-1+x45V8),0]) If[1270-If[x45V1>0,600+100 (-1+x45V1),0]-If[x45V10>0,400+50 (-1+x45V10),0]-If[x45V3>0,100+10 (-1+x45V3),0]-If[x45V8>0,40+10 (-1+x45V8),0]>0,167,0]+(-1445+If[x46V1>0,250+25 (-1+x46V1),0]+If[x46V2>0,500+50 (-1+x46V2),0]+If[x46V3>0,60+10 (-1+x46V3),0]+If[x46V4>0,250+25 (-1+x46V4),0]+If[x46V5>0,200+50 (-1+x46V5),0]+If[x46V6>0,600+100 (-1+x46V6),0]) If[1445-If[x46V1>0,250+25 (-1+x46V1),0]-If[x46V2>0,500+50 (-1+x46V2),0]-If[x46V3>0,60+10 (-1+x46V3),0]-If[x46V4>0,250+25 (-1+x46V4),0]-If[x46V5>0,200+50 (-1+x46V5),0]-If[x46V6>0,600+100 (-1+x46V6),0]>0,146,0]+(-2150+If[x47V1>0,1000+100 (-1+x47V1),0]+If[x47V10>0,200+50 (-1+x47V10),0]+If[x47V3>0,50+5 (-1+x47V3),0]+If[x47V5>0,500+50 (-1+x47V5),0]+If[x47V6>0,30+5 (-1+x47V6),0]+If[x47V8>0,200+50 (-1+x47V8),0]) If[2150-If[x47V1>0,1000+100 (-1+x47V1),0]-If[x47V10>0,200+50 (-1+x47V10),0]-If[x47V3>0,50+5 (-1+x47V3),0]-If[x47V5>0,500+50 (-1+x47V5),0]-If[x47V6>0,30+5 (-1+x47V6),0]-If[x47V8>0,200+50 (-1+x47V8),0]>0,190,0]+(-88+If[x48V2>0,300+50 (-1+x48V2),0]+If[x48V3>0,200+50 (-1+x48V3),0]+If[x48V4>0,30+5 (-1+x48V4),0]+If[x48V6>0,100+10 (-1+x48V6),0]+If[x48V7>0,200+25 (-1+x48V7),0]+If[x48V8>0,500+50 (-1+x48V8),0]) If[88-If[x48V2>0,300+50 (-1+x48V2),0]-If[x48V3>0,200+50 (-1+x48V3),0]-If[x48V4>0,30+5 (-1+x48V4),0]-If[x48V6>0,100+10 (-1+x48V6),0]-If[x48V7>0,200+25 (-1+x48V7),0]-If[x48V8>0,500+50 (-1+x48V8),0]>0,177,0]+(-1356+If[x49V2>0,40+10 (-1+x49V2),0]+If[x49V3>0,800+100 (-1+x49V3),0]+If[x49V4>0,20+5 (-1+x49V4),0]+If[x49V5>0,300+50 (-1+x49V5),0]+If[x49V9>0,100+25 (-1+x49V9),0]) If[1356-If[x49V2>0,40+10 (-1+x49V2),0]-If[x49V3>0,800+100 (-1+x49V3),0]-If[x49V4>0,20+5 (-1+x49V4),0]-If[x49V5>0,300+50 (-1+x49V5),0]-If[x49V9>0,100+25 (-1+x49V9),0]>0,163,0]+(-1685+If[x4V1>0,500+50 (-1+x4V1),0]+If[x4V10>0,50+5 (-1+x4V10),0]+If[x4V2>0,400+50 (-1+x4V2),0]+If[x4V4>0,40+5 (-1+x4V4),0]+If[x4V8>0,20+5 (-1+x4V8),0]+If[x4V9>0,30+5 (-1+x4V9),0]) If[1685-If[x4V1>0,500+50 (-1+x4V1),0]-If[x4V10>0,50+5 (-1+x4V10),0]-If[x4V2>0,400+50 (-1+x4V2),0]-If[x4V4>0,40+5 (-1+x4V4),0]-If[x4V8>0,20+5 (-1+x4V8),0]-If[x4V9>0,30+5 (-1+x4V9),0]>0,157,0]+(-666+If[x50V1>0,150+25 (-1+x50V1),0]+If[x50V3>0,20+5 (-1+x50V3),0]+If[x50V6>0,500+50 (-1+x50V6),0]+If[x50V8>0,500+50 (-1+x50V8),0]+If[x50V9>0,80+10 (-1+x50V9),0]) If[666-If[x50V1>0,150+25 (-1+x50V1),0]-If[x50V3>0,20+5 (-1+x50V3),0]-If[x50V6>0,500+50 (-1+x50V6),0]-If[x50V8>0,500+50 (-1+x50V8),0]-If[x50V9>0,80+10 (-1+x50V9),0]>0,100,0]+(-794+If[x51V1>0,100+10 (-1+x51V1),0]+If[x51V4>0,800+100 (-1+x51V4),0]+If[x51V6>0,200+50 (-1+x51V6),0]+If[x51V8>0,300+50 (-1+x51V8),0]+If[x51V9>0,400+100 (-1+x51V9),0]) If[794-If[x51V1>0,100+10 (-1+x51V1),0]-If[x51V4>0,800+100 (-1+x51V4),0]-If[x51V6>0,200+50 (-1+x51V6),0]-If[x51V8>0,300+50 (-1+x51V8),0]-If[x51V9>0,400+100 (-1+x51V9),0]>0,194,0]+(-2629+If[x52V3>0,50+5 (-1+x52V3),0]+If[x52V8>0,300+50 (-1+x52V8),0]) If[2629-If[x52V3>0,50+5 (-1+x52V3),0]-If[x52V8>0,300+50 (-1+x52V8),0]>0,118,0]+(-2710+If[x53V1>0,500+50 (-1+x53V1),0]+If[x53V10>0,40+10 (-1+x53V10),0]+If[x53V2>0,600+100 (-1+x53V2),0]+If[x53V7>0,400+100 (-1+x53V7),0]+If[x53V8>0,60+10 (-1+x53V8),0]+If[x53V9>0,150+25 (-1+x53V9),0]) If[2710-If[x53V1>0,500+50 (-1+x53V1),0]-If[x53V10>0,40+10 (-1+x53V10),0]-If[x53V2>0,600+100 (-1+x53V2),0]-If[x53V7>0,400+100 (-1+x53V7),0]-If[x53V8>0,60+10 (-1+x53V8),0]-If[x53V9>0,150+25 (-1+x53V9),0]>0,131,0]+(-2957+If[x54V10>0,80+10 (-1+x54V10),0]+If[x54V4>0,500+50 (-1+x54V4),0]) If[2957-If[x54V10>0,80+10 (-1+x54V10),0]-If[x54V4>0,500+50 (-1+x54V4),0]>0,147,0]+(-353+If[x55V1>0,40+10 (-1+x55V1),0]+If[x55V10>0,400+50 (-1+x55V10),0]+If[x55V2>0,40+10 (-1+x55V2),0]+If[x55V4>0,20+5 (-1+x55V4),0]+If[x55V6>0,1000+100 (-1+x55V6),0]+If[x55V7>0,500+50 (-1+x55V7),0]+If[x55V8>0,100+10 (-1+x55V8),0]+If[x55V9>0,80+10 (-1+x55V9),0]) If[353-If[x55V1>0,40+10 (-1+x55V1),0]-If[x55V10>0,400+50 (-1+x55V10),0]-If[x55V2>0,40+10 (-1+x55V2),0]-If[x55V4>0,20+5 (-1+x55V4),0]-If[x55V6>0,1000+100 (-1+x55V6),0]-If[x55V7>0,500+50 (-1+x55V7),0]-If[x55V8>0,100+10 (-1+x55V8),0]-If[x55V9>0,80+10 (-1+x55V9),0]>0,117,0]+(-2623+If[x56V2>0,20+5 (-1+x56V2),0]+If[x56V3>0,250+25 (-1+x56V3),0]+If[x56V8>0,1000+100 (-1+x56V8),0]) If[2623-If[x56V2>0,20+5 (-1+x56V2),0]-If[x56V3>0,250+25 (-1+x56V3),0]-If[x56V8>0,1000+100 (-1+x56V8),0]>0,130,0]+(-470+If[x57V1>0,500+50 (-1+x57V1),0]+If[x57V10>0,800+100 (-1+x57V10),0]+If[x57V4>0,1000+100 (-1+x57V4),0]+If[x57V5>0,150+25 (-1+x57V5),0]+If[x57V6>0,60+10 (-1+x57V6),0]+If[x57V8>0,30+5 (-1+x57V8),0]+If[x57V9>0,800+100 (-1+x57V9),0]) If[470-If[x57V1>0,500+50 (-1+x57V1),0]-If[x57V10>0,800+100 (-1+x57V10),0]-If[x57V4>0,1000+100 (-1+x57V4),0]-If[x57V5>0,150+25 (-1+x57V5),0]-If[x57V6>0,60+10 (-1+x57V6),0]-If[x57V8>0,30+5 (-1+x57V8),0]-If[x57V9>0,800+100 (-1+x57V9),0]>0,165,0]+(-1980+If[x58V1>0,250+25 (-1+x58V1),0]+If[x58V10>0,1000+100 (-1+x58V10),0]+If[x58V2>0,800+100 (-1+x58V2),0]+If[x58V3>0,500+50 (-1+x58V3),0]+If[x58V4>0,100+25 (-1+x58V4),0]+If[x58V5>0,400+50 (-1+x58V5),0]+If[x58V6>0,20+5 (-1+x58V6),0]+If[x58V7>0,100+10 (-1+x58V7),0]+If[x58V8>0,60+10 (-1+x58V8),0]+If[x58V9>0,100+10 (-1+x58V9),0]) If[1980-If[x58V1>0,250+25 (-1+x58V1),0]-If[x58V10>0,1000+100 (-1+x58V10),0]-If[x58V2>0,800+100 (-1+x58V2),0]-If[x58V3>0,500+50 (-1+x58V3),0]-If[x58V4>0,100+25 (-1+x58V4),0]-If[x58V5>0,400+50 (-1+x58V5),0]-If[x58V6>0,20+5 (-1+x58V6),0]-If[x58V7>0,100+10 (-1+x58V7),0]-If[x58V8>0,60+10 (-1+x58V8),0]-If[x58V9>0,100+10 (-1+x58V9),0]>0,155,0]+(-2941+If[x59V5>0,20+5 (-1+x59V5),0]+If[x59V7>0,100+25 (-1+x59V7),0]) If[2941-If[x59V5>0,20+5 (-1+x59V5),0]-If[x59V7>0,100+25 (-1+x59V7),0]>0,174,0]+(-2639+If[x5V1>0,600+100 (-1+x5V1),0]+If[x5V2>0,800+100 (-1+x5V2),0]+If[x5V4>0,60+10 (-1+x5V4),0]+If[x5V6>0,150+25 (-1+x5V6),0]+If[x5V7>0,150+25 (-1+x5V7),0]+If[x5V8>0,600+100 (-1+x5V8),0]+If[x5V9>0,80+10 (-1+x5V9),0]) If[2639-If[x5V1>0,600+100 (-1+x5V1),0]-If[x5V2>0,800+100 (-1+x5V2),0]-If[x5V4>0,60+10 (-1+x5V4),0]-If[x5V6>0,150+25 (-1+x5V6),0]-If[x5V7>0,150+25 (-1+x5V7),0]-If[x5V8>0,600+100 (-1+x5V8),0]-If[x5V9>0,80+10 (-1+x5V9),0]>0,157,0]+(-2928+If[x60V10>0,40+5 (-1+x60V10),0]+If[x60V2>0,250+25 (-1+x60V2),0]+If[x60V4>0,1000+100 (-1+x60V4),0]+If[x60V7>0,20+5 (-1+x60V7),0]+If[x60V9>0,40+10 (-1+x60V9),0]) If[2928-If[x60V10>0,40+5 (-1+x60V10),0]-If[x60V2>0,250+25 (-1+x60V2),0]-If[x60V4>0,1000+100 (-1+x60V4),0]-If[x60V7>0,20+5 (-1+x60V7),0]-If[x60V9>0,40+10 (-1+x60V9),0]>0,150,0]+(-2274+If[x61V2>0,250+25 (-1+x61V2),0]+If[x61V5>0,800+100 (-1+x61V5),0]+If[x61V7>0,100+10 (-1+x61V7),0]+If[x61V9>0,250+25 (-1+x61V9),0]) If[2274-If[x61V2>0,250+25 (-1+x61V2),0]-If[x61V5>0,800+100 (-1+x61V5),0]-If[x61V7>0,100+10 (-1+x61V7),0]-If[x61V9>0,250+25 (-1+x61V9),0]>0,102,0]+(-2666+If[x62V2>0,300+50 (-1+x62V2),0]+If[x62V7>0,200+25 (-1+x62V7),0]) If[2666-If[x62V2>0,300+50 (-1+x62V2),0]-If[x62V7>0,200+25 (-1+x62V7),0]>0,176,0]+(-1708+If[x63V3>0,100+25 (-1+x63V3),0]+If[x63V4>0,200+50 (-1+x63V4),0]+If[x63V5>0,150+25 (-1+x63V5),0]+If[x63V6>0,400+50 (-1+x63V6),0]+If[x63V7>0,40+10 (-1+x63V7),0]+If[x63V8>0,20+5 (-1+x63V8),0]+If[x63V9>0,30+5 (-1+x63V9),0]) If[1708-If[x63V3>0,100+25 (-1+x63V3),0]-If[x63V4>0,200+50 (-1+x63V4),0]-If[x63V5>0,150+25 (-1+x63V5),0]-If[x63V6>0,400+50 (-1+x63V6),0]-If[x63V7>0,40+10 (-1+x63V7),0]-If[x63V8>0,20+5 (-1+x63V8),0]-If[x63V9>0,30+5 (-1+x63V9),0]>0,147,0]+(-1826+If[x64V2>0,200+25 (-1+x64V2),0]+If[x64V4>0,80+10 (-1+x64V4),0]+If[x64V8>0,50+5 (-1+x64V8),0]) If[1826-If[x64V2>0,200+25 (-1+x64V2),0]-If[x64V4>0,80+10 (-1+x64V4),0]-If[x64V8>0,50+5 (-1+x64V8),0]>0,143,0]+(-665+If[x65V1>0,400+50 (-1+x65V1),0]+If[x65V10>0,800+100 (-1+x65V10),0]+If[x65V3>0,300+50 (-1+x65V3),0]+If[x65V4>0,100+25 (-1+x65V4),0]+If[x65V9>0,500+50 (-1+x65V9),0]) If[665-If[x65V1>0,400+50 (-1+x65V1),0]-If[x65V10>0,800+100 (-1+x65V10),0]-If[x65V3>0,300+50 (-1+x65V3),0]-If[x65V4>0,100+25 (-1+x65V4),0]-If[x65V9>0,500+50 (-1+x65V9),0]>0,112,0]+(-938+If[x66V2>0,600+100 (-1+x66V2),0]+If[x66V3>0,250+25 (-1+x66V3),0]+If[x66V4>0,30+5 (-1+x66V4),0]+If[x66V5>0,150+25 (-1+x66V5),0]+If[x66V6>0,50+5 (-1+x66V6),0]+If[x66V8>0,100+10 (-1+x66V8),0]+If[x66V9>0,500+50 (-1+x66V9),0]) If[938-If[x66V2>0,600+100 (-1+x66V2),0]-If[x66V3>0,250+25 (-1+x66V3),0]-If[x66V4>0,30+5 (-1+x66V4),0]-If[x66V5>0,150+25 (-1+x66V5),0]-If[x66V6>0,50+5 (-1+x66V6),0]-If[x66V8>0,100+10 (-1+x66V8),0]-If[x66V9>0,500+50 (-1+x66V9),0]>0,159,0]+(-2447+If[x67V4>0,60+10 (-1+x67V4),0]+If[x67V8>0,400+100 (-1+x67V8),0]) If[2447-If[x67V4>0,60+10 (-1+x67V4),0]-If[x67V8>0,400+100 (-1+x67V8),0]>0,169,0]+(-79+If[x68V1>0,150+25 (-1+x68V1),0]+If[x68V4>0,400+100 (-1+x68V4),0]+If[x68V5>0,30+5 (-1+x68V5),0]+If[x68V7>0,500+50 (-1+x68V7),0]+If[x68V8>0,50+5 (-1+x68V8),0]+If[x68V9>0,150+25 (-1+x68V9),0]) If[79-If[x68V1>0,150+25 (-1+x68V1),0]-If[x68V4>0,400+100 (-1+x68V4),0]-If[x68V5>0,30+5 (-1+x68V5),0]-If[x68V7>0,500+50 (-1+x68V7),0]-If[x68V8>0,50+5 (-1+x68V8),0]-If[x68V9>0,150+25 (-1+x68V9),0]>0,181,0]+(-1253+If[x69V1>0,50+5 (-1+x69V1),0]+If[x69V2>0,60+10 (-1+x69V2),0]+If[x69V4>0,100+10 (-1+x69V4),0]+If[x69V5>0,250+25 (-1+x69V5),0]+If[x69V7>0,300+50 (-1+x69V7),0]+If[x69V8>0,150+25 (-1+x69V8),0]+If[x69V9>0,40+5 (-1+x69V9),0]) If[1253-If[x69V1>0,50+5 (-1+x69V1),0]-If[x69V2>0,60+10 (-1+x69V2),0]-If[x69V4>0,100+10 (-1+x69V4),0]-If[x69V5>0,250+25 (-1+x69V5),0]-If[x69V7>0,300+50 (-1+x69V7),0]-If[x69V8>0,150+25 (-1+x69V8),0]-If[x69V9>0,40+5 (-1+x69V9),0]>0,110,0]+(-1994+If[x6V1>0,50+5 (-1+x6V1),0]+If[x6V10>0,300+50 (-1+x6V10),0]+If[x6V4>0,250+25 (-1+x6V4),0]+If[x6V8>0,50+5 (-1+x6V8),0]) If[1994-If[x6V1>0,50+5 (-1+x6V1),0]-If[x6V10>0,300+50 (-1+x6V10),0]-If[x6V4>0,250+25 (-1+x6V4),0]-If[x6V8>0,50+5 (-1+x6V8),0]>0,121,0]+(-2762+If[x70V10>0,50+5 (-1+x70V10),0]+If[x70V2>0,40+5 (-1+x70V2),0]+If[x70V3>0,60+10 (-1+x70V3),0]+If[x70V4>0,60+10 (-1+x70V4),0]+If[x70V6>0,150+25 (-1+x70V6),0]+If[x70V7>0,50+5 (-1+x70V7),0]) If[2762-If[x70V10>0,50+5 (-1+x70V10),0]-If[x70V2>0,40+5 (-1+x70V2),0]-If[x70V3>0,60+10 (-1+x70V3),0]-If[x70V4>0,60+10 (-1+x70V4),0]-If[x70V6>0,150+25 (-1+x70V6),0]-If[x70V7>0,50+5 (-1+x70V7),0]>0,153,0]+(-2504+If[x71V1>0,30+5 (-1+x71V1),0]+If[x71V3>0,250+25 (-1+x71V3),0]+If[x71V7>0,1000+100 (-1+x71V7),0]+If[x71V8>0,40+10 (-1+x71V8),0]+If[x71V9>0,40+5 (-1+x71V9),0]) If[2504-If[x71V1>0,30+5 (-1+x71V1),0]-If[x71V3>0,250+25 (-1+x71V3),0]-If[x71V7>0,1000+100 (-1+x71V7),0]-If[x71V8>0,40+10 (-1+x71V8),0]-If[x71V9>0,40+5 (-1+x71V9),0]>0,119,0]+(-357+If[x72V1>0,800+100 (-1+x72V1),0]+If[x72V7>0,400+100 (-1+x72V7),0]) If[357-If[x72V1>0,800+100 (-1+x72V1),0]-If[x72V7>0,400+100 (-1+x72V7),0]>0,178,0]+(-1374+If[x73V1>0,400+50 (-1+x73V1),0]+If[x73V2>0,50+5 (-1+x73V2),0]+If[x73V6>0,20+5 (-1+x73V6),0]+If[x73V8>0,80+10 (-1+x73V8),0]+If[x73V9>0,300+50 (-1+x73V9),0]) If[1374-If[x73V1>0,400+50 (-1+x73V1),0]-If[x73V2>0,50+5 (-1+x73V2),0]-If[x73V6>0,20+5 (-1+x73V6),0]-If[x73V8>0,80+10 (-1+x73V8),0]-If[x73V9>0,300+50 (-1+x73V9),0]>0,122,0]+(-1906+If[x74V2>0,40+5 (-1+x74V2),0]+If[x74V3>0,500+50 (-1+x74V3),0]+If[x74V4>0,80+10 (-1+x74V4),0]+If[x74V6>0,20+5 (-1+x74V6),0]) If[1906-If[x74V2>0,40+5 (-1+x74V2),0]-If[x74V3>0,500+50 (-1+x74V3),0]-If[x74V4>0,80+10 (-1+x74V4),0]-If[x74V6>0,20+5 (-1+x74V6),0]>0,124,0]+(-484+If[x75V1>0,500+50 (-1+x75V1),0]+If[x75V10>0,20+5 (-1+x75V10),0]+If[x75V4>0,30+5 (-1+x75V4),0]+If[x75V5>0,400+50 (-1+x75V5),0]+If[x75V6>0,100+10 (-1+x75V6),0]+If[x75V7>0,400+50 (-1+x75V7),0]+If[x75V9>0,20+5 (-1+x75V9),0]) If[484-If[x75V1>0,500+50 (-1+x75V1),0]-If[x75V10>0,20+5 (-1+x75V10),0]-If[x75V4>0,30+5 (-1+x75V4),0]-If[x75V5>0,400+50 (-1+x75V5),0]-If[x75V6>0,100+10 (-1+x75V6),0]-If[x75V7>0,400+50 (-1+x75V7),0]-If[x75V9>0,20+5 (-1+x75V9),0]>0,196,0]+(-2003+If[x76V10>0,250+25 (-1+x76V10),0]+If[x76V2>0,600+100 (-1+x76V2),0]+If[x76V3>0,100+25 (-1+x76V3),0]+If[x76V4>0,500+50 (-1+x76V4),0]+If[x76V7>0,40+5 (-1+x76V7),0]+If[x76V8>0,80+10 (-1+x76V8),0]+If[x76V9>0,1000+100 (-1+x76V9),0]) If[2003-If[x76V10>0,250+25 (-1+x76V10),0]-If[x76V2>0,600+100 (-1+x76V2),0]-If[x76V3>0,100+25 (-1+x76V3),0]-If[x76V4>0,500+50 (-1+x76V4),0]-If[x76V7>0,40+5 (-1+x76V7),0]-If[x76V8>0,80+10 (-1+x76V8),0]-If[x76V9>0,1000+100 (-1+x76V9),0]>0,131,0]+(-103+If[x77V10>0,30+5 (-1+x77V10),0]+If[x77V3>0,1000+100 (-1+x77V3),0]+If[x77V4>0,400+100 (-1+x77V4),0]+If[x77V6>0,400+100 (-1+x77V6),0]) If[103-If[x77V10>0,30+5 (-1+x77V10),0]-If[x77V3>0,1000+100 (-1+x77V3),0]-If[x77V4>0,400+100 (-1+x77V4),0]-If[x77V6>0,400+100 (-1+x77V6),0]>0,132,0]+(-2059+If[x78V1>0,20+5 (-1+x78V1),0]+If[x78V2>0,80+10 (-1+x78V2),0]+If[x78V3>0,80+10 (-1+x78V3),0]+If[x78V4>0,20+5 (-1+x78V4),0]+If[x78V5>0,40+10 (-1+x78V5),0]+If[x78V6>0,40+10 (-1+x78V6),0]) If[2059-If[x78V1>0,20+5 (-1+x78V1),0]-If[x78V2>0,80+10 (-1+x78V2),0]-If[x78V3>0,80+10 (-1+x78V3),0]-If[x78V4>0,20+5 (-1+x78V4),0]-If[x78V5>0,40+10 (-1+x78V5),0]-If[x78V6>0,40+10 (-1+x78V6),0]>0,103,0]+(-2189+If[x79V2>0,100+10 (-1+x79V2),0]+If[x79V6>0,800+100 (-1+x79V6),0]+If[x79V7>0,150+25 (-1+x79V7),0]) If[2189-If[x79V2>0,100+10 (-1+x79V2),0]-If[x79V6>0,800+100 (-1+x79V6),0]-If[x79V7>0,150+25 (-1+x79V7),0]>0,137,0]+(-1523+If[x7V10>0,40+10 (-1+x7V10),0]+If[x7V2>0,100+10 (-1+x7V2),0]+If[x7V4>0,150+25 (-1+x7V4),0]+If[x7V6>0,100+10 (-1+x7V6),0]) If[1523-If[x7V10>0,40+10 (-1+x7V10),0]-If[x7V2>0,100+10 (-1+x7V2),0]-If[x7V4>0,150+25 (-1+x7V4),0]-If[x7V6>0,100+10 (-1+x7V6),0]>0,122,0]+(-2860+If[x80V1>0,60+10 (-1+x80V1),0]+If[x80V5>0,800+100 (-1+x80V5),0]+If[x80V9>0,100+10 (-1+x80V9),0]) If[2860-If[x80V1>0,60+10 (-1+x80V1),0]-If[x80V5>0,800+100 (-1+x80V5),0]-If[x80V9>0,100+10 (-1+x80V9),0]>0,175,0]+(-1010+If[x81V1>0,20+5 (-1+x81V1),0]+If[x81V3>0,100+25 (-1+x81V3),0]+If[x81V8>0,150+25 (-1+x81V8),0]+If[x81V9>0,150+25 (-1+x81V9),0]) If[1010-If[x81V1>0,20+5 (-1+x81V1),0]-If[x81V3>0,100+25 (-1+x81V3),0]-If[x81V8>0,150+25 (-1+x81V8),0]-If[x81V9>0,150+25 (-1+x81V9),0]>0,101,0]+(-1599+If[x82V1>0,200+50 (-1+x82V1),0]+If[x82V10>0,80+10 (-1+x82V10),0]+If[x82V2>0,400+100 (-1+x82V2),0]+If[x82V4>0,150+25 (-1+x82V4),0]+If[x82V6>0,400+50 (-1+x82V6),0]+If[x82V7>0,40+10 (-1+x82V7),0]+If[x82V9>0,250+25 (-1+x82V9),0]) If[1599-If[x82V1>0,200+50 (-1+x82V1),0]-If[x82V10>0,80+10 (-1+x82V10),0]-If[x82V2>0,400+100 (-1+x82V2),0]-If[x82V4>0,150+25 (-1+x82V4),0]-If[x82V6>0,400+50 (-1+x82V6),0]-If[x82V7>0,40+10 (-1+x82V7),0]-If[x82V9>0,250+25 (-1+x82V9),0]>0,194,0]+(-1520+If[x83V1>0,200+50 (-1+x83V1),0]+If[x83V2>0,50+5 (-1+x83V2),0]+If[x83V9>0,300+50 (-1+x83V9),0]) If[1520-If[x83V1>0,200+50 (-1+x83V1),0]-If[x83V2>0,50+5 (-1+x83V2),0]-If[x83V9>0,300+50 (-1+x83V9),0]>0,136,0]+(-2337+If[x84V10>0,400+50 (-1+x84V10),0]+If[x84V2>0,60+10 (-1+x84V2),0]+If[x84V4>0,300+50 (-1+x84V4),0]+If[x84V5>0,200+50 (-1+x84V5),0]+If[x84V6>0,60+10 (-1+x84V6),0]+If[x84V7>0,60+10 (-1+x84V7),0]+If[x84V8>0,60+10 (-1+x84V8),0]) If[2337-If[x84V10>0,400+50 (-1+x84V10),0]-If[x84V2>0,60+10 (-1+x84V2),0]-If[x84V4>0,300+50 (-1+x84V4),0]-If[x84V5>0,200+50 (-1+x84V5),0]-If[x84V6>0,60+10 (-1+x84V6),0]-If[x84V7>0,60+10 (-1+x84V7),0]-If[x84V8>0,60+10 (-1+x84V8),0]>0,130,0]+(-1720+If[x85V1>0,200+50 (-1+x85V1),0]+If[x85V3>0,200+50 (-1+x85V3),0]+If[x85V4>0,40+10 (-1+x85V4),0]+If[x85V6>0,800+100 (-1+x85V6),0]+If[x85V7>0,30+5 (-1+x85V7),0]+If[x85V8>0,100+25 (-1+x85V8),0]+If[x85V9>0,400+100 (-1+x85V9),0]) If[1720-If[x85V1>0,200+50 (-1+x85V1),0]-If[x85V3>0,200+50 (-1+x85V3),0]-If[x85V4>0,40+10 (-1+x85V4),0]-If[x85V6>0,800+100 (-1+x85V6),0]-If[x85V7>0,30+5 (-1+x85V7),0]-If[x85V8>0,100+25 (-1+x85V8),0]-If[x85V9>0,400+100 (-1+x85V9),0]>0,110,0]+(-1283+If[x86V1>0,50+5 (-1+x86V1),0]+If[x86V10>0,100+25 (-1+x86V10),0]+If[x86V2>0,300+50 (-1+x86V2),0]+If[x86V4>0,100+10 (-1+x86V4),0]+If[x86V6>0,400+100 (-1+x86V6),0]+If[x86V7>0,100+10 (-1+x86V7),0]+If[x86V8>0,40+10 (-1+x86V8),0]+If[x86V9>0,30+5 (-1+x86V9),0]) If[1283-If[x86V1>0,50+5 (-1+x86V1),0]-If[x86V10>0,100+25 (-1+x86V10),0]-If[x86V2>0,300+50 (-1+x86V2),0]-If[x86V4>0,100+10 (-1+x86V4),0]-If[x86V6>0,400+100 (-1+x86V6),0]-If[x86V7>0,100+10 (-1+x86V7),0]-If[x86V8>0,40+10 (-1+x86V8),0]-If[x86V9>0,30+5 (-1+x86V9),0]>0,138,0]+(-784+If[x87V1>0,300+50 (-1+x87V1),0]+If[x87V6>0,30+5 (-1+x87V6),0]+If[x87V7>0,200+25 (-1+x87V7),0]+If[x87V8>0,100+10 (-1+x87V8),0]+If[x87V9>0,200+25 (-1+x87V9),0]) If[784-If[x87V1>0,300+50 (-1+x87V1),0]-If[x87V6>0,30+5 (-1+x87V6),0]-If[x87V7>0,200+25 (-1+x87V7),0]-If[x87V8>0,100+10 (-1+x87V8),0]-If[x87V9>0,200+25 (-1+x87V9),0]>0,109,0]+(-724+If[x88V1>0,100+10 (-1+x88V1),0]+If[x88V10>0,600+100 (-1+x88V10),0]+If[x88V2>0,250+25 (-1+x88V2),0]+If[x88V4>0,100+25 (-1+x88V4),0]+If[x88V5>0,20+5 (-1+x88V5),0]+If[x88V6>0,200+25 (-1+x88V6),0]+If[x88V7>0,200+25 (-1+x88V7),0]+If[x88V8>0,400+100 (-1+x88V8),0]+If[x88V9>0,100+25 (-1+x88V9),0]) If[724-If[x88V1>0,100+10 (-1+x88V1),0]-If[x88V10>0,600+100 (-1+x88V10),0]-If[x88V2>0,250+25 (-1+x88V2),0]-If[x88V4>0,100+25 (-1+x88V4),0]-If[x88V5>0,20+5 (-1+x88V5),0]-If[x88V6>0,200+25 (-1+x88V6),0]-If[x88V7>0,200+25 (-1+x88V7),0]-If[x88V8>0,400+100 (-1+x88V8),0]-If[x88V9>0,100+25 (-1+x88V9),0]>0,140,0]+(-1164+If[x89V2>0,800+100 (-1+x89V2),0]+If[x89V6>0,400+100 (-1+x89V6),0]+If[x89V8>0,400+50 (-1+x89V8),0]) If[1164-If[x89V2>0,800+100 (-1+x89V2),0]-If[x89V6>0,400+100 (-1+x89V6),0]-If[x89V8>0,400+50 (-1+x89V8),0]>0,101,0]+(-2702+If[x8V10>0,50+5 (-1+x8V10),0]+If[x8V3>0,50+5 (-1+x8V3),0]+If[x8V7>0,150+25 (-1+x8V7),0]+If[x8V8>0,20+5 (-1+x8V8),0]) If[2702-If[x8V10>0,50+5 (-1+x8V10),0]-If[x8V3>0,50+5 (-1+x8V3),0]-If[x8V7>0,150+25 (-1+x8V7),0]-If[x8V8>0,20+5 (-1+x8V8),0]>0,153,0]+(-2307+If[x90V1>0,60+10 (-1+x90V1),0]+If[x90V3>0,150+25 (-1+x90V3),0]+If[x90V4>0,100+10 (-1+x90V4),0]+If[x90V5>0,150+25 (-1+x90V5),0]+If[x90V7>0,500+50 (-1+x90V7),0]+If[x90V8>0,250+25 (-1+x90V8),0]+If[x90V9>0,400+100 (-1+x90V9),0]) If[2307-If[x90V1>0,60+10 (-1+x90V1),0]-If[x90V3>0,150+25 (-1+x90V3),0]-If[x90V4>0,100+10 (-1+x90V4),0]-If[x90V5>0,150+25 (-1+x90V5),0]-If[x90V7>0,500+50 (-1+x90V7),0]-If[x90V8>0,250+25 (-1+x90V8),0]-If[x90V9>0,400+100 (-1+x90V9),0]>0,170,0]+(-2023+If[x91V6>0,800+100 (-1+x91V6),0]+If[x91V8>0,100+10 (-1+x91V8),0]) If[2023-If[x91V6>0,800+100 (-1+x91V6),0]-If[x91V8>0,100+10 (-1+x91V8),0]>0,182,0]+(-419+If[x92V10>0,200+25 (-1+x92V10),0]+If[x92V2>0,800+100 (-1+x92V2),0]+If[x92V4>0,20+5 (-1+x92V4),0]+If[x92V7>0,250+25 (-1+x92V7),0]) If[419-If[x92V10>0,200+25 (-1+x92V10),0]-If[x92V2>0,800+100 (-1+x92V2),0]-If[x92V4>0,20+5 (-1+x92V4),0]-If[x92V7>0,250+25 (-1+x92V7),0]>0,186,0]+(-198+If[x93V1>0,200+25 (-1+x93V1),0]+If[x93V2>0,300+50 (-1+x93V2),0]+If[x93V4>0,30+5 (-1+x93V4),0]+If[x93V5>0,1000+100 (-1+x93V5),0]+If[x93V7>0,40+10 (-1+x93V7),0]+If[x93V8>0,20+5 (-1+x93V8),0]) If[198-If[x93V1>0,200+25 (-1+x93V1),0]-If[x93V2>0,300+50 (-1+x93V2),0]-If[x93V4>0,30+5 (-1+x93V4),0]-If[x93V5>0,1000+100 (-1+x93V5),0]-If[x93V7>0,40+10 (-1+x93V7),0]-If[x93V8>0,20+5 (-1+x93V8),0]>0,131,0]+(-1140+If[x94V10>0,30+5 (-1+x94V10),0]+If[x94V2>0,60+10 (-1+x94V2),0]+If[x94V3>0,250+25 (-1+x94V3),0]+If[x94V5>0,200+25 (-1+x94V5),0]+If[x94V7>0,500+50 (-1+x94V7),0]+If[x94V8>0,600+100 (-1+x94V8),0]+If[x94V9>0,400+50 (-1+x94V9),0]) If[1140-If[x94V10>0,30+5 (-1+x94V10),0]-If[x94V2>0,60+10 (-1+x94V2),0]-If[x94V3>0,250+25 (-1+x94V3),0]-If[x94V5>0,200+25 (-1+x94V5),0]-If[x94V7>0,500+50 (-1+x94V7),0]-If[x94V8>0,600+100 (-1+x94V8),0]-If[x94V9>0,400+50 (-1+x94V9),0]>0,127,0]+(-871+If[x95V1>0,200+25 (-1+x95V1),0]+If[x95V10>0,30+5 (-1+x95V10),0]+If[x95V3>0,300+50 (-1+x95V3),0]+If[x95V5>0,400+100 (-1+x95V5),0]+If[x95V8>0,300+50 (-1+x95V8),0]+If[x95V9>0,200+25 (-1+x95V9),0]) If[871-If[x95V1>0,200+25 (-1+x95V1),0]-If[x95V10>0,30+5 (-1+x95V10),0]-If[x95V3>0,300+50 (-1+x95V3),0]-If[x95V5>0,400+100 (-1+x95V5),0]-If[x95V8>0,300+50 (-1+x95V8),0]-If[x95V9>0,200+25 (-1+x95V9),0]>0,198,0]+(-1789+If[x96V10>0,200+25 (-1+x96V10),0]+If[x96V3>0,300+50 (-1+x96V3),0]+If[x96V4>0,200+50 (-1+x96V4),0]+If[x96V5>0,40+10 (-1+x96V5),0]+If[x96V6>0,50+5 (-1+x96V6),0]+If[x96V7>0,250+25 (-1+x96V7),0]+If[x96V9>0,20+5 (-1+x96V9),0]) If[1789-If[x96V10>0,200+25 (-1+x96V10),0]-If[x96V3>0,300+50 (-1+x96V3),0]-If[x96V4>0,200+50 (-1+x96V4),0]-If[x96V5>0,40+10 (-1+x96V5),0]-If[x96V6>0,50+5 (-1+x96V6),0]-If[x96V7>0,250+25 (-1+x96V7),0]-If[x96V9>0,20+5 (-1+x96V9),0]>0,143,0]+(-2116+If[x97V1>0,30+5 (-1+x97V1),0]+If[x97V2>0,50+5 (-1+x97V2),0]+If[x97V3>0,200+25 (-1+x97V3),0]+If[x97V6>0,800+100 (-1+x97V6),0]+If[x97V9>0,100+25 (-1+x97V9),0]) If[2116-If[x97V1>0,30+5 (-1+x97V1),0]-If[x97V2>0,50+5 (-1+x97V2),0]-If[x97V3>0,200+25 (-1+x97V3),0]-If[x97V6>0,800+100 (-1+x97V6),0]-If[x97V9>0,100+25 (-1+x97V9),0]>0,183,0]+(-242+If[x98V1>0,400+100 (-1+x98V1),0]+If[x98V10>0,250+25 (-1+x98V10),0]+If[x98V2>0,1000+100 (-1+x98V2),0]+If[x98V3>0,60+10 (-1+x98V3),0]+If[x98V5>0,40+10 (-1+x98V5),0]+If[x98V6>0,30+5 (-1+x98V6),0]+If[x98V7>0,150+25 (-1+x98V7),0]+If[x98V9>0,50+5 (-1+x98V9),0]) If[242-If[x98V1>0,400+100 (-1+x98V1),0]-If[x98V10>0,250+25 (-1+x98V10),0]-If[x98V2>0,1000+100 (-1+x98V2),0]-If[x98V3>0,60+10 (-1+x98V3),0]-If[x98V5>0,40+10 (-1+x98V5),0]-If[x98V6>0,30+5 (-1+x98V6),0]-If[x98V7>0,150+25 (-1+x98V7),0]-If[x98V9>0,50+5 (-1+x98V9),0]>0,161,0]+(-116+If[x99V3>0,40+10 (-1+x99V3),0]+If[x99V4>0,40+5 (-1+x99V4),0]+If[x99V9>0,600+100 (-1+x99V9),0]) If[116-If[x99V3>0,40+10 (-1+x99V3),0]-If[x99V4>0,40+5 (-1+x99V4),0]-If[x99V9>0,600+100 (-1+x99V9),0]>0,176,0]+(-2685+If[x9V1>0,400+100 (-1+x9V1),0]+If[x9V10>0,600+100 (-1+x9V10),0]+If[x9V2>0,1000+100 (-1+x9V2),0]+If[x9V4>0,150+25 (-1+x9V4),0]+If[x9V6>0,100+25 (-1+x9V6),0]+If[x9V8>0,600+100 (-1+x9V8),0]+If[x9V9>0,200+50 (-1+x9V9),0]) If[2685-If[x9V1>0,400+100 (-1+x9V1),0]-If[x9V10>0,600+100 (-1+x9V10),0]-If[x9V2>0,1000+100 (-1+x9V2),0]-If[x9V4>0,150+25 (-1+x9V4),0]-If[x9V6>0,100+25 (-1+x9V6),0]-If[x9V8>0,600+100 (-1+x9V8),0]-If[x9V9>0,200+50 (-1+x9V9),0]>0,194,0]
Fri 24 Nov 2017 19:05:44, time used: 1.156, total time used: 1.
---------------------------------------------
conf = 

    let conf = 
        {   
            ConfigData.defaultValue with
                noOfContracts = 200
                noOfAssets = 20
                rescaleRates = true
        }


contracts = 

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
            { contractID = 100; baseAsset = 8; amount = 674.; descriptors = [| { asset = 1; minVal = 40.; incr = 10. }; { asset = 2; minVal = 400.; incr = 100. }; { asset = 4; minVal = 600.; incr = 100. }; { asset = 5; minVal = 40.; incr = 5. }; { asset = 6; minVal = 60.; incr = 10. }; { asset = 8; minVal = 100.; incr = 10. };  |]; nonPayingRate = 107. }
            { contractID = 101; baseAsset = 6; amount = 1578.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 8; minVal = 100.; incr = 10. };  |]; nonPayingRate = 105. }
            { contractID = 102; baseAsset = 5; amount = 2063.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 1; minVal = 60.; incr = 10. }; { asset = 2; minVal = 600.; incr = 100. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 5; minVal = 200.; incr = 50. }; { asset = 8; minVal = 400.; incr = 50. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 190. }
            { contractID = 103; baseAsset = 8; amount = 479.; descriptors = [| { asset = 5; minVal = 80.; incr = 10. }; { asset = 8; minVal = 40.; incr = 10. };  |]; nonPayingRate = 133. }
            { contractID = 104; baseAsset = 6; amount = 1941.; descriptors = [| { asset = 5; minVal = 30.; incr = 5. }; { asset = 6; minVal = 600.; incr = 100. };  |]; nonPayingRate = 147. }
            { contractID = 105; baseAsset = 7; amount = 2959.; descriptors = [| { asset = 2; minVal = 40.; incr = 5. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 7; minVal = 100.; incr = 10. }; { asset = 8; minVal = 60.; incr = 10. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 163. }
            { contractID = 106; baseAsset = 8; amount = 645.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 1; minVal = 60.; incr = 10. }; { asset = 4; minVal = 30.; incr = 5. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 8; minVal = 50.; incr = 5. };  |]; nonPayingRate = 137. }
            { contractID = 107; baseAsset = 1; amount = 1988.; descriptors = [| { asset = 1; minVal = 800.; incr = 100. }; { asset = 4; minVal = 60.; incr = 10. };  |]; nonPayingRate = 117. }
            { contractID = 108; baseAsset = 7; amount = 1200.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 5; minVal = 600.; incr = 100. }; { asset = 6; minVal = 200.; incr = 25. }; { asset = 7; minVal = 40.; incr = 5. };  |]; nonPayingRate = 145. }
            { contractID = 109; baseAsset = 6; amount = 2668.; descriptors = [| { asset = 6; minVal = 250.; incr = 25. }; { asset = 9; minVal = 600.; incr = 100. };  |]; nonPayingRate = 149. }
            { contractID = 110; baseAsset = 6; amount = 2836.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 80.; incr = 10. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 7; minVal = 200.; incr = 25. }; { asset = 8; minVal = 200.; incr = 25. };  |]; nonPayingRate = 179. }
            { contractID = 111; baseAsset = 8; amount = 1704.; descriptors = [| { asset = 2; minVal = 60.; incr = 10. }; { asset = 3; minVal = 50.; incr = 5. }; { asset = 4; minVal = 100.; incr = 25. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 6; minVal = 20.; incr = 5. }; { asset = 8; minVal = 250.; incr = 25. };  |]; nonPayingRate = 126. }
            { contractID = 112; baseAsset = 5; amount = 1449.; descriptors = [| { asset = 0; minVal = 100.; incr = 25. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 4; minVal = 40.; incr = 5. }; { asset = 5; minVal = 600.; incr = 100. }; { asset = 6; minVal = 400.; incr = 50. }; { asset = 8; minVal = 800.; incr = 100. }; { asset = 9; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 126. }
            { contractID = 113; baseAsset = 8; amount = 2395.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 1; minVal = 800.; incr = 100. }; { asset = 2; minVal = 600.; incr = 100. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 7; minVal = 400.; incr = 100. }; { asset = 8; minVal = 80.; incr = 10. }; { asset = 9; minVal = 500.; incr = 50. };  |]; nonPayingRate = 177. }
            { contractID = 114; baseAsset = 3; amount = 1987.; descriptors = [| { asset = 0; minVal = 300.; incr = 50. }; { asset = 3; minVal = 1000.; incr = 100. }; { asset = 6; minVal = 1000.; incr = 100. }; { asset = 8; minVal = 80.; incr = 10. };  |]; nonPayingRate = 170. }
            { contractID = 115; baseAsset = 8; amount = 2326.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 1; minVal = 40.; incr = 5. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 5; minVal = 80.; incr = 10. }; { asset = 7; minVal = 60.; incr = 10. }; { asset = 8; minVal = 40.; incr = 10. }; { asset = 9; minVal = 200.; incr = 50. };  |]; nonPayingRate = 137. }
            { contractID = 116; baseAsset = 5; amount = 1880.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 2; minVal = 200.; incr = 25. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 5; minVal = 400.; incr = 100. }; { asset = 6; minVal = 400.; incr = 50. }; { asset = 8; minVal = 600.; incr = 100. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 150. }
            { contractID = 117; baseAsset = 9; amount = 2497.; descriptors = [| { asset = 0; minVal = 100.; incr = 25. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 7; minVal = 500.; incr = 50. }; { asset = 8; minVal = 40.; incr = 5. }; { asset = 9; minVal = 60.; incr = 10. };  |]; nonPayingRate = 165. }
            { contractID = 118; baseAsset = 0; amount = 206.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 8; minVal = 60.; incr = 10. };  |]; nonPayingRate = 184. }
            { contractID = 119; baseAsset = 3; amount = 2763.; descriptors = [| { asset = 3; minVal = 600.; incr = 100. }; { asset = 6; minVal = 800.; incr = 100. }; { asset = 8; minVal = 40.; incr = 10. };  |]; nonPayingRate = 158. }
            { contractID = 120; baseAsset = 5; amount = 1149.; descriptors = [| { asset = 4; minVal = 80.; incr = 10. }; { asset = 5; minVal = 60.; incr = 10. };  |]; nonPayingRate = 132. }
            { contractID = 121; baseAsset = 6; amount = 1259.; descriptors = [| { asset = 4; minVal = 40.; incr = 10. }; { asset = 6; minVal = 40.; incr = 5. }; { asset = 7; minVal = 400.; incr = 100. }; { asset = 8; minVal = 100.; incr = 10. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 180. }
            { contractID = 122; baseAsset = 1; amount = 195.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 20.; incr = 5. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 5; minVal = 500.; incr = 50. }; { asset = 8; minVal = 30.; incr = 5. }; { asset = 9; minVal = 800.; incr = 100. };  |]; nonPayingRate = 160. }
            { contractID = 123; baseAsset = 0; amount = 1803.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 4; minVal = 1000.; incr = 100. }; { asset = 6; minVal = 800.; incr = 100. };  |]; nonPayingRate = 148. }
            { contractID = 124; baseAsset = 3; amount = 2456.; descriptors = [| { asset = 1; minVal = 30.; incr = 5. }; { asset = 3; minVal = 50.; incr = 5. }; { asset = 4; minVal = 600.; incr = 100. }; { asset = 5; minVal = 40.; incr = 5. }; { asset = 6; minVal = 40.; incr = 10. }; { asset = 8; minVal = 200.; incr = 25. }; { asset = 9; minVal = 60.; incr = 10. };  |]; nonPayingRate = 147. }
            { contractID = 125; baseAsset = 5; amount = 320.; descriptors = [| { asset = 0; minVal = 400.; incr = 50. }; { asset = 1; minVal = 600.; incr = 100. }; { asset = 3; minVal = 300.; incr = 50. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 7; minVal = 800.; incr = 100. }; { asset = 8; minVal = 500.; incr = 50. };  |]; nonPayingRate = 181. }
            { contractID = 126; baseAsset = 1; amount = 2958.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 1; minVal = 40.; incr = 5. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 4; minVal = 100.; incr = 10. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 8; minVal = 100.; incr = 10. }; { asset = 9; minVal = 200.; incr = 25. };  |]; nonPayingRate = 159. }
            { contractID = 127; baseAsset = 0; amount = 581.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 3; minVal = 400.; incr = 50. };  |]; nonPayingRate = 145. }
            { contractID = 128; baseAsset = 8; amount = 1958.; descriptors = [| { asset = 1; minVal = 40.; incr = 5. }; { asset = 4; minVal = 600.; incr = 100. }; { asset = 6; minVal = 300.; incr = 50. }; { asset = 8; minVal = 60.; incr = 10. }; { asset = 9; minVal = 200.; incr = 25. };  |]; nonPayingRate = 179. }
            { contractID = 129; baseAsset = 0; amount = 1009.; descriptors = [| { asset = 0; minVal = 20.; incr = 5. }; { asset = 9; minVal = 800.; incr = 100. };  |]; nonPayingRate = 113. }
            { contractID = 130; baseAsset = 2; amount = 1389.; descriptors = [| { asset = 0; minVal = 30.; incr = 5. }; { asset = 1; minVal = 40.; incr = 5. }; { asset = 2; minVal = 80.; incr = 10. }; { asset = 3; minVal = 40.; incr = 10. }; { asset = 4; minVal = 200.; incr = 25. }; { asset = 6; minVal = 500.; incr = 50. };  |]; nonPayingRate = 112. }
            { contractID = 131; baseAsset = 3; amount = 1709.; descriptors = [| { asset = 0; minVal = 800.; incr = 100. }; { asset = 2; minVal = 20.; incr = 5. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 4; minVal = 1000.; incr = 100. }; { asset = 5; minVal = 100.; incr = 10. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 8; minVal = 250.; incr = 25. };  |]; nonPayingRate = 122. }
            { contractID = 132; baseAsset = 9; amount = 2349.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 1; minVal = 30.; incr = 5. }; { asset = 3; minVal = 100.; incr = 25. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 7; minVal = 150.; incr = 25. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 141. }
            { contractID = 133; baseAsset = 7; amount = 444.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 2; minVal = 200.; incr = 25. }; { asset = 4; minVal = 20.; incr = 5. }; { asset = 6; minVal = 50.; incr = 5. }; { asset = 7; minVal = 150.; incr = 25. }; { asset = 8; minVal = 800.; incr = 100. }; { asset = 9; minVal = 500.; incr = 50. };  |]; nonPayingRate = 102. }
            { contractID = 134; baseAsset = 5; amount = 376.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 2; minVal = 40.; incr = 10. }; { asset = 5; minVal = 1000.; incr = 100. }; { asset = 6; minVal = 100.; incr = 25. };  |]; nonPayingRate = 128. }
            { contractID = 135; baseAsset = 5; amount = 981.; descriptors = [| { asset = 1; minVal = 150.; incr = 25. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 4; minVal = 30.; incr = 5. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 6; minVal = 30.; incr = 5. };  |]; nonPayingRate = 195. }
            { contractID = 136; baseAsset = 7; amount = 2392.; descriptors = [| { asset = 1; minVal = 600.; incr = 100. }; { asset = 2; minVal = 100.; incr = 25. }; { asset = 3; minVal = 400.; incr = 50. }; { asset = 7; minVal = 100.; incr = 10. };  |]; nonPayingRate = 174. }
            { contractID = 137; baseAsset = 2; amount = 1481.; descriptors = [| { asset = 1; minVal = 400.; incr = 50. }; { asset = 2; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 200.; incr = 50. }; { asset = 4; minVal = 100.; incr = 25. }; { asset = 5; minVal = 400.; incr = 50. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 7; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 139. }
            { contractID = 138; baseAsset = 5; amount = 1603.; descriptors = [| { asset = 0; minVal = 50.; incr = 5. }; { asset = 2; minVal = 20.; incr = 5. }; { asset = 3; minVal = 1000.; incr = 100. }; { asset = 4; minVal = 200.; incr = 50. }; { asset = 5; minVal = 250.; incr = 25. }; { asset = 7; minVal = 800.; incr = 100. }; { asset = 8; minVal = 400.; incr = 100. };  |]; nonPayingRate = 167. }
            { contractID = 139; baseAsset = 0; amount = 7.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 2; minVal = 40.; incr = 5. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 4; minVal = 800.; incr = 100. }; { asset = 6; minVal = 100.; incr = 25. }; { asset = 7; minVal = 600.; incr = 100. }; { asset = 8; minVal = 800.; incr = 100. };  |]; nonPayingRate = 200. }
            { contractID = 140; baseAsset = 4; amount = 1598.; descriptors = [| { asset = 1; minVal = 80.; incr = 10. }; { asset = 3; minVal = 60.; incr = 10. }; { asset = 4; minVal = 100.; incr = 25. }; { asset = 5; minVal = 500.; incr = 50. }; { asset = 6; minVal = 100.; incr = 25. }; { asset = 7; minVal = 800.; incr = 100. }; { asset = 9; minVal = 100.; incr = 10. };  |]; nonPayingRate = 168. }
            { contractID = 141; baseAsset = 0; amount = 2724.; descriptors = [| { asset = 0; minVal = 800.; incr = 100. }; { asset = 1; minVal = 400.; incr = 50. }; { asset = 2; minVal = 100.; incr = 10. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 5; minVal = 400.; incr = 50. }; { asset = 7; minVal = 150.; incr = 25. }; { asset = 8; minVal = 40.; incr = 5. };  |]; nonPayingRate = 184. }
            { contractID = 142; baseAsset = 8; amount = 2148.; descriptors = [| { asset = 1; minVal = 20.; incr = 5. }; { asset = 5; minVal = 250.; incr = 25. }; { asset = 7; minVal = 20.; incr = 5. }; { asset = 8; minVal = 200.; incr = 50. };  |]; nonPayingRate = 167. }
            { contractID = 143; baseAsset = 0; amount = 1005.; descriptors = [| { asset = 0; minVal = 250.; incr = 25. }; { asset = 6; minVal = 400.; incr = 50. };  |]; nonPayingRate = 185. }
            { contractID = 144; baseAsset = 9; amount = 2525.; descriptors = [| { asset = 1; minVal = 50.; incr = 5. }; { asset = 4; minVal = 40.; incr = 5. }; { asset = 5; minVal = 200.; incr = 50. }; { asset = 6; minVal = 200.; incr = 25. }; { asset = 7; minVal = 400.; incr = 50. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 190. }
            { contractID = 145; baseAsset = 2; amount = 1558.; descriptors = [| { asset = 2; minVal = 400.; incr = 100. }; { asset = 4; minVal = 150.; incr = 25. };  |]; nonPayingRate = 112. }
            { contractID = 146; baseAsset = 5; amount = 1179.; descriptors = [| { asset = 0; minVal = 40.; incr = 5. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 4; minVal = 20.; incr = 5. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 7; minVal = 100.; incr = 10. }; { asset = 9; minVal = 200.; incr = 50. };  |]; nonPayingRate = 181. }
            { contractID = 147; baseAsset = 1; amount = 1283.; descriptors = [| { asset = 1; minVal = 150.; incr = 25. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 4; minVal = 60.; incr = 10. }; { asset = 6; minVal = 100.; incr = 10. }; { asset = 7; minVal = 30.; incr = 5. };  |]; nonPayingRate = 176. }
            { contractID = 148; baseAsset = 5; amount = 1897.; descriptors = [| { asset = 3; minVal = 800.; incr = 100. }; { asset = 5; minVal = 400.; incr = 100. };  |]; nonPayingRate = 115. }
            { contractID = 149; baseAsset = 6; amount = 1114.; descriptors = [| { asset = 3; minVal = 800.; incr = 100. }; { asset = 4; minVal = 400.; incr = 100. }; { asset = 5; minVal = 600.; incr = 100. }; { asset = 6; minVal = 50.; incr = 5. }; { asset = 7; minVal = 500.; incr = 50. };  |]; nonPayingRate = 164. }
            { contractID = 150; baseAsset = 9; amount = 1314.; descriptors = [| { asset = 0; minVal = 800.; incr = 100. }; { asset = 1; minVal = 300.; incr = 50. }; { asset = 4; minVal = 80.; incr = 10. }; { asset = 5; minVal = 200.; incr = 25. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 8; minVal = 200.; incr = 50. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 192. }
            { contractID = 151; baseAsset = 9; amount = 2455.; descriptors = [| { asset = 1; minVal = 30.; incr = 5. }; { asset = 4; minVal = 800.; incr = 100. }; { asset = 6; minVal = 300.; incr = 50. }; { asset = 9; minVal = 150.; incr = 25. };  |]; nonPayingRate = 129. }
            { contractID = 152; baseAsset = 5; amount = 242.; descriptors = [| { asset = 5; minVal = 400.; incr = 50. }; { asset = 6; minVal = 40.; incr = 10. }; { asset = 8; minVal = 30.; incr = 5. };  |]; nonPayingRate = 171. }
            { contractID = 153; baseAsset = 5; amount = 2638.; descriptors = [| { asset = 1; minVal = 800.; incr = 100. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 5; minVal = 800.; incr = 100. }; { asset = 6; minVal = 800.; incr = 100. }; { asset = 8; minVal = 100.; incr = 10. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 148. }
            { contractID = 154; baseAsset = 7; amount = 1172.; descriptors = [| { asset = 2; minVal = 400.; incr = 50. }; { asset = 4; minVal = 20.; incr = 5. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 7; minVal = 100.; incr = 10. }; { asset = 9; minVal = 300.; incr = 50. };  |]; nonPayingRate = 137. }
            { contractID = 155; baseAsset = 8; amount = 998.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 2; minVal = 40.; incr = 10. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 6; minVal = 150.; incr = 25. }; { asset = 8; minVal = 60.; incr = 10. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 142. }
            { contractID = 156; baseAsset = 8; amount = 511.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 2; minVal = 300.; incr = 50. }; { asset = 3; minVal = 200.; incr = 50. }; { asset = 4; minVal = 200.; incr = 25. }; { asset = 5; minVal = 1000.; incr = 100. }; { asset = 8; minVal = 100.; incr = 10. };  |]; nonPayingRate = 179. }
            { contractID = 157; baseAsset = 0; amount = 1176.; descriptors = [| { asset = 0; minVal = 60.; incr = 10. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 5; minVal = 400.; incr = 50. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 123. }
            { contractID = 158; baseAsset = 6; amount = 2530.; descriptors = [| { asset = 6; minVal = 200.; incr = 25. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 129. }
            { contractID = 159; baseAsset = 3; amount = 1523.; descriptors = [| { asset = 1; minVal = 50.; incr = 5. }; { asset = 2; minVal = 200.; incr = 25. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 7; minVal = 1000.; incr = 100. }; { asset = 8; minVal = 400.; incr = 50. };  |]; nonPayingRate = 113. }
            { contractID = 160; baseAsset = 4; amount = 1893.; descriptors = [| { asset = 2; minVal = 200.; incr = 25. }; { asset = 4; minVal = 1000.; incr = 100. }; { asset = 8; minVal = 50.; incr = 5. };  |]; nonPayingRate = 188. }
            { contractID = 161; baseAsset = 7; amount = 2334.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 1; minVal = 80.; incr = 10. }; { asset = 2; minVal = 500.; incr = 50. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 5; minVal = 80.; incr = 10. }; { asset = 6; minVal = 20.; incr = 5. }; { asset = 7; minVal = 500.; incr = 50. }; { asset = 8; minVal = 250.; incr = 25. }; { asset = 9; minVal = 600.; incr = 100. };  |]; nonPayingRate = 115. }
            { contractID = 162; baseAsset = 6; amount = 377.; descriptors = [| { asset = 0; minVal = 300.; incr = 50. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 5; minVal = 100.; incr = 25. }; { asset = 6; minVal = 80.; incr = 10. }; { asset = 7; minVal = 300.; incr = 50. }; { asset = 9; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 102. }
            { contractID = 163; baseAsset = 0; amount = 1682.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 2; minVal = 30.; incr = 5. }; { asset = 3; minVal = 200.; incr = 25. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 5; minVal = 100.; incr = 10. }; { asset = 8; minVal = 60.; incr = 10. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 194. }
            { contractID = 164; baseAsset = 2; amount = 1543.; descriptors = [| { asset = 1; minVal = 100.; incr = 10. }; { asset = 2; minVal = 100.; incr = 10. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 6; minVal = 150.; incr = 25. }; { asset = 7; minVal = 400.; incr = 50. };  |]; nonPayingRate = 118. }
            { contractID = 165; baseAsset = 2; amount = 2759.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 2; minVal = 600.; incr = 100. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 5; minVal = 50.; incr = 5. };  |]; nonPayingRate = 165. }
            { contractID = 166; baseAsset = 8; amount = 377.; descriptors = [| { asset = 5; minVal = 20.; incr = 5. }; { asset = 8; minVal = 200.; incr = 50. };  |]; nonPayingRate = 104. }
            { contractID = 167; baseAsset = 4; amount = 671.; descriptors = [| { asset = 1; minVal = 300.; incr = 50. }; { asset = 2; minVal = 800.; incr = 100. }; { asset = 4; minVal = 500.; incr = 50. }; { asset = 5; minVal = 40.; incr = 5. }; { asset = 8; minVal = 200.; incr = 25. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 123. }
            { contractID = 168; baseAsset = 6; amount = 868.; descriptors = [| { asset = 1; minVal = 800.; incr = 100. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 6; minVal = 800.; incr = 100. }; { asset = 7; minVal = 400.; incr = 100. };  |]; nonPayingRate = 174. }
            { contractID = 169; baseAsset = 3; amount = 629.; descriptors = [| { asset = 1; minVal = 60.; incr = 10. }; { asset = 3; minVal = 100.; incr = 10. }; { asset = 8; minVal = 30.; incr = 5. };  |]; nonPayingRate = 166. }
            { contractID = 170; baseAsset = 5; amount = 270.; descriptors = [| { asset = 2; minVal = 150.; incr = 25. }; { asset = 4; minVal = 40.; incr = 10. }; { asset = 5; minVal = 200.; incr = 50. }; { asset = 6; minVal = 40.; incr = 10. };  |]; nonPayingRate = 107. }
            { contractID = 171; baseAsset = 3; amount = 544.; descriptors = [| { asset = 0; minVal = 40.; incr = 10. }; { asset = 1; minVal = 100.; incr = 25. }; { asset = 3; minVal = 80.; incr = 10. }; { asset = 7; minVal = 30.; incr = 5. }; { asset = 8; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 112. }
            { contractID = 172; baseAsset = 5; amount = 2308.; descriptors = [| { asset = 1; minVal = 200.; incr = 50. }; { asset = 2; minVal = 400.; incr = 50. }; { asset = 3; minVal = 40.; incr = 5. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 9; minVal = 200.; incr = 50. };  |]; nonPayingRate = 113. }
            { contractID = 173; baseAsset = 2; amount = 153.; descriptors = [| { asset = 0; minVal = 250.; incr = 25. }; { asset = 1; minVal = 30.; incr = 5. }; { asset = 2; minVal = 40.; incr = 10. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 5; minVal = 100.; incr = 25. }; { asset = 6; minVal = 100.; incr = 25. }; { asset = 7; minVal = 60.; incr = 10. }; { asset = 9; minVal = 300.; incr = 50. };  |]; nonPayingRate = 105. }
            { contractID = 174; baseAsset = 5; amount = 1394.; descriptors = [| { asset = 1; minVal = 20.; incr = 5. }; { asset = 2; minVal = 150.; incr = 25. }; { asset = 3; minVal = 30.; incr = 5. }; { asset = 4; minVal = 200.; incr = 25. }; { asset = 5; minVal = 400.; incr = 100. }; { asset = 9; minVal = 40.; incr = 5. };  |]; nonPayingRate = 198. }
            { contractID = 175; baseAsset = 5; amount = 957.; descriptors = [| { asset = 2; minVal = 80.; incr = 10. }; { asset = 5; minVal = 200.; incr = 25. };  |]; nonPayingRate = 121. }
            { contractID = 176; baseAsset = 6; amount = 192.; descriptors = [| { asset = 1; minVal = 800.; incr = 100. }; { asset = 6; minVal = 50.; incr = 5. };  |]; nonPayingRate = 193. }
            { contractID = 177; baseAsset = 2; amount = 1284.; descriptors = [| { asset = 0; minVal = 30.; incr = 5. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 2; minVal = 80.; incr = 10. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 4; minVal = 300.; incr = 50. }; { asset = 6; minVal = 50.; incr = 5. }; { asset = 9; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 139. }
            { contractID = 178; baseAsset = 8; amount = 1353.; descriptors = [| { asset = 5; minVal = 20.; incr = 5. }; { asset = 6; minVal = 100.; incr = 10. }; { asset = 8; minVal = 40.; incr = 5. };  |]; nonPayingRate = 190. }
            { contractID = 179; baseAsset = 3; amount = 910.; descriptors = [| { asset = 3; minVal = 100.; incr = 10. }; { asset = 5; minVal = 150.; incr = 25. }; { asset = 6; minVal = 250.; incr = 25. }; { asset = 7; minVal = 40.; incr = 5. }; { asset = 8; minVal = 20.; incr = 5. };  |]; nonPayingRate = 147. }
            { contractID = 180; baseAsset = 7; amount = 897.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 4; minVal = 250.; incr = 25. }; { asset = 7; minVal = 1000.; incr = 100. }; { asset = 9; minVal = 800.; incr = 100. };  |]; nonPayingRate = 117. }
            { contractID = 181; baseAsset = 3; amount = 2919.; descriptors = [| { asset = 0; minVal = 200.; incr = 50. }; { asset = 1; minVal = 30.; incr = 5. }; { asset = 3; minVal = 400.; incr = 50. }; { asset = 7; minVal = 1000.; incr = 100. }; { asset = 8; minVal = 800.; incr = 100. }; { asset = 9; minVal = 30.; incr = 5. };  |]; nonPayingRate = 144. }
            { contractID = 182; baseAsset = 3; amount = 1350.; descriptors = [| { asset = 0; minVal = 600.; incr = 100. }; { asset = 1; minVal = 400.; incr = 100. }; { asset = 3; minVal = 1000.; incr = 100. }; { asset = 7; minVal = 40.; incr = 10. }; { asset = 8; minVal = 200.; incr = 50. };  |]; nonPayingRate = 195. }
            { contractID = 183; baseAsset = 8; amount = 2914.; descriptors = [| { asset = 0; minVal = 100.; incr = 25. }; { asset = 1; minVal = 600.; incr = 100. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 4; minVal = 200.; incr = 50. }; { asset = 5; minVal = 30.; incr = 5. }; { asset = 8; minVal = 300.; incr = 50. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 131. }
            { contractID = 184; baseAsset = 0; amount = 1953.; descriptors = [| { asset = 0; minVal = 400.; incr = 100. }; { asset = 1; minVal = 30.; incr = 5. };  |]; nonPayingRate = 128. }
            { contractID = 185; baseAsset = 9; amount = 26.; descriptors = [| { asset = 0; minVal = 150.; incr = 25. }; { asset = 1; minVal = 100.; incr = 25. }; { asset = 2; minVal = 400.; incr = 100. }; { asset = 3; minVal = 500.; incr = 50. }; { asset = 7; minVal = 500.; incr = 50. }; { asset = 8; minVal = 80.; incr = 10. }; { asset = 9; minVal = 400.; incr = 50. };  |]; nonPayingRate = 199. }
            { contractID = 186; baseAsset = 8; amount = 2814.; descriptors = [| { asset = 2; minVal = 1000.; incr = 100. }; { asset = 5; minVal = 40.; incr = 10. }; { asset = 6; minVal = 30.; incr = 5. }; { asset = 8; minVal = 400.; incr = 100. }; { asset = 9; minVal = 100.; incr = 25. };  |]; nonPayingRate = 174. }
            { contractID = 187; baseAsset = 9; amount = 1494.; descriptors = [| { asset = 3; minVal = 800.; incr = 100. }; { asset = 4; minVal = 80.; incr = 10. }; { asset = 5; minVal = 100.; incr = 25. }; { asset = 6; minVal = 40.; incr = 10. }; { asset = 7; minVal = 500.; incr = 50. }; { asset = 8; minVal = 80.; incr = 10. }; { asset = 9; minVal = 250.; incr = 25. };  |]; nonPayingRate = 145. }
            { contractID = 188; baseAsset = 3; amount = 2348.; descriptors = [| { asset = 0; minVal = 400.; incr = 50. }; { asset = 1; minVal = 60.; incr = 10. }; { asset = 3; minVal = 800.; incr = 100. }; { asset = 5; minVal = 50.; incr = 5. }; { asset = 9; minVal = 300.; incr = 50. };  |]; nonPayingRate = 163. }
            { contractID = 189; baseAsset = 7; amount = 1861.; descriptors = [| { asset = 0; minVal = 40.; incr = 10. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 2; minVal = 40.; incr = 5. }; { asset = 3; minVal = 20.; incr = 5. }; { asset = 5; minVal = 500.; incr = 50. }; { asset = 7; minVal = 80.; incr = 10. };  |]; nonPayingRate = 108. }
            { contractID = 190; baseAsset = 8; amount = 2611.; descriptors = [| { asset = 0; minVal = 400.; incr = 50. }; { asset = 1; minVal = 300.; incr = 50. }; { asset = 3; minVal = 250.; incr = 25. }; { asset = 7; minVal = 40.; incr = 10. }; { asset = 8; minVal = 50.; incr = 5. };  |]; nonPayingRate = 169. }
            { contractID = 191; baseAsset = 4; amount = 927.; descriptors = [| { asset = 0; minVal = 100.; incr = 10. }; { asset = 2; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 40.; incr = 5. }; { asset = 4; minVal = 50.; incr = 5. }; { asset = 5; minVal = 300.; incr = 50. }; { asset = 6; minVal = 200.; incr = 25. }; { asset = 8; minVal = 40.; incr = 10. };  |]; nonPayingRate = 172. }
            { contractID = 192; baseAsset = 8; amount = 2943.; descriptors = [| { asset = 0; minVal = 200.; incr = 25. }; { asset = 4; minVal = 100.; incr = 10. }; { asset = 5; minVal = 600.; incr = 100. }; { asset = 6; minVal = 60.; incr = 10. }; { asset = 8; minVal = 50.; incr = 5. }; { asset = 9; minVal = 400.; incr = 100. };  |]; nonPayingRate = 142. }
            { contractID = 193; baseAsset = 2; amount = 2818.; descriptors = [| { asset = 0; minVal = 250.; incr = 25. }; { asset = 2; minVal = 80.; incr = 10. }; { asset = 6; minVal = 500.; incr = 50. }; { asset = 9; minVal = 1000.; incr = 100. };  |]; nonPayingRate = 118. }
            { contractID = 194; baseAsset = 9; amount = 1320.; descriptors = [| { asset = 1; minVal = 50.; incr = 5. }; { asset = 3; minVal = 200.; incr = 50. }; { asset = 4; minVal = 300.; incr = 50. }; { asset = 7; minVal = 400.; incr = 100. }; { asset = 8; minVal = 500.; incr = 50. }; { asset = 9; minVal = 60.; incr = 10. };  |]; nonPayingRate = 105. }
            { contractID = 195; baseAsset = 9; amount = 712.; descriptors = [| { asset = 1; minVal = 500.; incr = 50. }; { asset = 2; minVal = 400.; incr = 100. }; { asset = 4; minVal = 150.; incr = 25. }; { asset = 6; minVal = 1000.; incr = 100. }; { asset = 9; minVal = 100.; incr = 10. };  |]; nonPayingRate = 199. }
            { contractID = 196; baseAsset = 6; amount = 410.; descriptors = [| { asset = 6; minVal = 1000.; incr = 100. }; { asset = 9; minVal = 400.; incr = 100. };  |]; nonPayingRate = 167. }
            { contractID = 197; baseAsset = 1; amount = 949.; descriptors = [| { asset = 0; minVal = 40.; incr = 10. }; { asset = 1; minVal = 50.; incr = 5. }; { asset = 4; minVal = 30.; incr = 5. }; { asset = 5; minVal = 400.; incr = 100. }; { asset = 8; minVal = 800.; incr = 100. }; { asset = 9; minVal = 50.; incr = 5. };  |]; nonPayingRate = 200. }
            { contractID = 198; baseAsset = 6; amount = 1925.; descriptors = [| { asset = 0; minVal = 1000.; incr = 100. }; { asset = 3; minVal = 600.; incr = 100. }; { asset = 4; minVal = 400.; incr = 50. }; { asset = 6; minVal = 100.; incr = 10. }; { asset = 7; minVal = 500.; incr = 50. }; { asset = 8; minVal = 100.; incr = 10. }; { asset = 9; minVal = 80.; incr = 10. };  |]; nonPayingRate = 133. }
            { contractID = 199; baseAsset = 9; amount = 829.; descriptors = [| { asset = 1; minVal = 200.; incr = 50. }; { asset = 2; minVal = 100.; incr = 10. }; { asset = 3; minVal = 150.; incr = 25. }; { asset = 4; minVal = 400.; incr = 100. }; { asset = 5; minVal = 20.; incr = 5. }; { asset = 9; minVal = 250.; incr = 25. };  |]; nonPayingRate = 165. }
        |]

positions = 

    let positions : PositionData[] = 
        [|
            { asset = 0; balance = 31699.; incomeRate = 39. }
            { asset = 1; balance = 54687.; incomeRate = 38. }
            { asset = 2; balance = 85057.; incomeRate = 30. }
            { asset = 3; balance = 149097.; incomeRate = 3. }
            { asset = 4; balance = 55197.; incomeRate = 37. }
            { asset = 5; balance = 35370.; incomeRate = 97. }
            { asset = 6; balance = 135474.; incomeRate = 21. }
            { asset = 7; balance = 80689.; incomeRate = 52. }
            { asset = 8; balance = 11090.; incomeRate = 68. }
            { asset = 9; balance = 97419.; incomeRate = 18. }
            { asset = 10; balance = 79605.; incomeRate = 11. }
            { asset = 11; balance = 28959.; incomeRate = 92. }
            { asset = 12; balance = 147861.; incomeRate = 74. }
            { asset = 13; balance = 72971.; incomeRate = 42. }
            { asset = 14; balance = 71512.; incomeRate = 70. }
            { asset = 15; balance = 93277.; incomeRate = 47. }
            { asset = 16; balance = 73274.; incomeRate = 28. }
            { asset = 17; balance = 50701.; incomeRate = 48. }
            { asset = 18; balance = 30051.; incomeRate = 31. }
            { asset = 19; balance = 95550.; incomeRate = 85. }
        |]

Calling NMaximize...
Sun 26 Nov 2017 23:56:57, time used: 190273, total time used: 190274.
---------------------------------------------
sol = {5.63461*10^7,{x1V5->0,x1V6->5,x1V7->9,x2V2->0,x2V5->2,x2V6->0,x2V7->1,x2V9->3,x2V10->4,x3V1->48,x3V3->1,x4V1->0,x4V2->0,x4V4->297,x4V8->4,x4V9->0,x4V10->17,x5V1->0,x5V2->0,x5V4->212,x5V6->0,x5V7->14,x5V8->0,x5V9->0,x6V1->8,x6V4->65,x6V8->3,x6V10->0,x7V2->0,x7V4->54,x7V6->0,x7V10->2,x8V3->20,x8V7->21,x8V8->0,x8V10->373,x9V1->0,x9V2->1,x9V4->7,x9V6->0,x9V8->0,x9V9->0,x9V10->9,x10V5->100,x10V7->56,x10V8->0,x10V10->156,x11V1->0,x11V2->1,x11V3->0,x11V4->19,x11V9->0,x11V10->0,x12V1->0,x12V3->0,x12V4->214,x12V5->0,x12V8->0,x12V9->0,x12V10->26,x13V1->0,x13V3->0,x13V4->4,x13V7->4,x13V10->0,x14V1->1,x14V2->0,x14V3->3,x14V4->174,x14V5->0,x14V6->0,x14V7->0,x14V10->1,x15V1->12,x15V2->46,x15V5->48,x16V1->81,x16V3->5,x17V2->0,x17V3->0,x17V4->42,x17V5->0,x17V6->0,x17V8->0,x17V9->0,x17V10->0,x18V1->0,x18V2->1,x18V3->0,x18V4->74,x18V5->0,x18V6->0,x18V7->2,x18V10->32,x19V3->3,x19V5->1,x19V8->9,x19V9->0,x19V10->10,x20V1->1,x20V2->24,x20V3->6,x20V6->0,x20V8->0,x20V10->82,x21V3->0,x21V5->0,x21V6->0,x21V7->3,x22V1->6,x22V3->1,x22V4->0,x22V6->0,x22V10->61,x23V4->53,x23V5->4,x24V1->0,x24V3->0,x24V4->1,x24V5->0,x24V7->0,x24V9->0,x24V10->0,x25V2->0,x25V4->2,x25V7->0,x25V9->0,x26V1->15,x26V6->0,x26V7->13,x26V9->1,x27V2->2,x27V4->48,x27V5->0,x28V1->4,x28V6->0,x28V7->174,x28V8->2,x28V9->0,x29V1->0,x29V2->1,x29V3->0,x29V4->62,x29V5->0,x29V8->0,x29V9->0,x30V1->0,x30V4->41,x31V1->0,x31V3->1,x31V5->0,x31V6->0,x31V7->0,x31V10->2,x32V1->0,x32V3->2,x32V8->0,x32V9->0,x32V10->282,x33V7->151,x33V10->166,x34V4->127,x34V6->0,x34V7->41,x34V8->16,x34V10->27,x35V3->1,x35V4->233,x35V8->0,x36V1->0,x36V2->0,x36V3->1,x36V4->19,x36V6->0,x36V9->0,x37V2->63,x37V3->488,x37V9->1,x38V8->125,x38V9->2,x39V2->0,x39V4->37,x39V8->1,x39V9->0,x40V1->377,x40V9->0,x41V1->0,x41V3->6,x41V5->36,x41V6->0,x41V7->6,x41V8->0,x41V9->0,x41V10->161,x42V1->0,x42V3->0,x42V4->1,x42V6->0,x42V7->69,x42V9->0,x42V10->37,x43V1->0,x43V3->0,x43V4->1,x43V5->8,x43V8->0,x43V10->0,x44V2->0,x44V4->156,x44V6->0,x44V7->2,x44V10->6,x45V1->1,x45V3->1,x45V8->0,x45V10->5,x46V1->0,x46V2->1,x46V3->0,x46V4->29,x46V5->0,x46V6->0,x47V1->0,x47V3->1,x47V5->10,x47V6->0,x47V8->4,x47V10->13,x48V2->0,x48V3->0,x48V4->13,x48V6->0,x48V7->0,x48V8->0,x49V2->1,x49V3->0,x49V4->241,x49V5->0,x49V9->1,x50V1->0,x50V3->6,x50V6->0,x50V8->1,x50V9->5,x51V1->0,x51V4->1,x51V6->0,x51V8->0,x51V9->0,x52V3->457,x52V8->1,x53V1->0,x53V2->1,x53V7->5,x53V8->0,x53V9->0,x53V10->128,x54V4->35,x54V10->69,x55V1->10,x55V2->1,x55V4->14,x55V6->0,x55V7->0,x55V8->1,x55V9->0,x55V10->0,x56V2->57,x56V3->84,x56V8->0,x57V1->0,x57V4->0,x57V5->11,x57V6->0,x57V8->9,x57V9->0,x57V10->0,x58V1->0,x58V2->0,x58V3->0,x58V4->77,x58V5->0,x58V6->0,x58V7->0,x58V8->0,x58V9->0,x58V10->0,x59V5->25,x59V7->109,x60V2->0,x60V4->7,x60V7->169,x60V9->0,x60V10->87,x61V2->8,x61V5->1,x61V7->96,x61V9->0,x62V2->2,x62V7->86,x63V3->8,x63V4->18,x63V5->0,x63V6->0,x63V7->32,x63V8->4,x63V9->0,x64V2->0,x64V4->176,x64V8->0,x65V1->0,x65V3->0,x65V4->0,x65V9->0,x65V10->1,x66V2->0,x66V3->1,x66V4->105,x66V5->0,x66V6->0,x66V8->5,x66V9->0,x67V4->240,x67V8->0,x68V1->0,x68V4->1,x68V5->0,x68V7->0,x68V8->0,x68V9->0,x69V1->0,x69V2->0,x69V4->87,x69V5->0,x69V7->1,x69V8->0,x69V9->0,x70V2->0,x70V3->0,x70V4->261,x70V6->0,x70V7->2,x70V10->1,x71V1->26,x71V3->7,x71V7->10,x71V8->2,x71V9->0,x72V1->0,x72V7->1,x73V1->6,x73V2->21,x73V6->2,x73V8->48,x73V9->0,x74V2->1,x74V3->0,x74V4->180,x74V6->0,x75V1->0,x75V4->2,x75V5->0,x75V6->0,x75V7->2,x75V9->0,x75V10->0,x76V2->0,x76V3->1,x76V4->7,x76V7->69,x76V8->0,x76V9->0,x76V10->20,x77V3->0,x77V4->1,x77V6->0,x77V10->0,x78V1->19,x78V2->24,x78V3->6,x78V4->117,x78V5->88,x78V6->0,x79V2->60,x79V6->0,x79V7->55,x80V1->51,x80V5->16,x80V9->0,x81V1->54,x81V3->13,x81V8->2,x81V9->1,x82V1->0,x82V2->0,x82V4->42,x82V6->0,x82V7->24,x82V9->0,x82V10->8,x83V1->20,x83V2->65,x83V9->0,x84V2->0,x84V4->42,x84V5->0,x84V6->0,x84V7->0,x84V8->0,x84V10->0,x85V1->1,x85V3->0,x85V4->149,x85V6->0,x85V7->0,x85V8->0,x85V9->0,x86V1->0,x86V2->1,x86V4->47,x86V6->0,x86V7->1,x86V8->1,x86V9->2,x86V10->7,x87V1->5,x87V6->0,x87V7->0,x87V8->19,x87V9->0,x88V1->4,x88V2->0,x88V4->13,x88V5->0,x88V6->0,x88V7->1,x88V8->0,x88V9->0,x88V10->0,x89V2->1,x89V6->0,x89V8->1,x90V1->0,x90V3->4,x90V4->140,x90V5->0,x90V7->3,x90V8->0,x90V9->0,x91V6->0,x91V8->194,x92V2->0,x92V4->16,x92V7->0,x92V10->6,x93V1->0,x93V2->0,x93V4->17,x93V5->0,x93V7->4,x93V8->1,x94V2->3,x94V3->0,x94V5->2,x94V7->5,x94V8->0,x94V9->0,x94V10->22,x95V1->0,x95V3->3,x95V5->1,x95V8->0,x95V9->0,x95V10->10,x96V3->0,x96V4->20,x96V5->0,x96V6->0,x96V7->1,x96V9->0,x96V10->9,x97V1->3,x97V2->1,x97V3->68,x97V6->0,x97V9->3,x98V1->0,x98V2->0,x98V3->16,x98V5->1,x98V6->0,x98V7->0,x98V9->0,x98V10->0,x99V3->4,x99V4->3,x99V9->0,x100V1->27,x100V3->0,x100V4->24,x100V6->4,x100V8->0,x100V9->15,x100V10->0,x101V2->18,x101V3->1,x101V5->0,x101V6->0,x101V7->2,x101V9->0,x102V1->0,x102V3->0,x102V4->130,x102V7->33,x102V9->0,x103V1->0,x103V2->0,x103V3->0,x103V4->12,x103V6->0,x103V9->0,x103V10->66,x104V6->5,x104V9->33,x105V6->0,x105V7->15,x106V3->50,x106V4->220,x106V6->0,x106V8->0,x106V9->1,x106V10->10,x107V1->0,x107V2->4,x107V5->0,x107V7->1,x107V9->2,x108V2->3,x108V5->94,x109V1->0,x109V4->91,x109V6->0,x109V7->1,x109V8->0,x110V7->58,x110V10->5,x111V1->0,x111V2->0,x111V3->2,x111V4->15,x111V7->6,x111V8->0,x111V9->0,x112V3->4,x112V4->218,x112V5->2,x112V6->0,x112V7->68,x112V9->0,x113V1->0,x113V4->279,x113V5->1,x113V6->0,x113V7->0,x113V9->0,x113V10->0,x114V1->1,x114V2->0,x114V3->0,x114V4->72,x114V6->0,x114V7->16,x114V8->0,x114V9->0,x114V10->9,x115V1->0,x115V4->1,x115V7->1,x115V9->0,x116V1->3,x116V2->0,x116V4->175,x116V5->0,x116V6->0,x116V8->3,x116V9->0,x116V10->5,x117V1->0,x117V3->1,x117V4->3,x117V6->0,x117V7->8,x117V9->0,x117V10->3,x118V1->3,x118V7->49,x118V8->0,x118V9->0,x118V10->203,x119V1->1,x119V9->0,x120V4->23,x120V7->0,x120V9->0,x121V5->101,x121V6->2,x122V5->16,x122V7->17,x122V8->2,x122V9->0,x122V10->2,x123V1->2,x123V2->0,x123V4->23,x123V6->0,x123V9->0,x123V10->0,x124V1->0,x124V5->1,x124V7->1,x125V2->0,x125V4->457,x125V5->0,x125V6->0,x125V7->1,x125V9->0,x125V10->4,x126V1->0,x126V2->0,x126V4->2,x126V6->0,x126V8->0,x126V9->0,x127V1->0,x127V2->0,x127V4->186,x127V5->0,x127V6->0,x127V7->15,x127V9->0,x127V10->11,x128V1->1,x128V4->2,x129V2->8,x129V5->0,x129V7->9,x129V9->1,x129V10->38,x130V1->1,x130V10->3,x131V1->1,x131V2->7,x131V3->8,x131V4->26,x131V5->1,x131V7->4,x132V1->0,x132V3->1,x132V4->327,x132V5->0,x132V6->0,x132V7->1,x132V9->0,x133V1->0,x133V2->0,x133V4->86,x133V5->0,x133V8->0,x133V10->20,x134V1->7,x134V2->1,x134V3->2,x134V5->1,x134V7->5,x134V8->0,x134V9->0,x134V10->0,x135V1->0,x135V3->0,x135V6->0,x135V7->12,x136V2->4,x136V3->8,x136V5->20,x136V6->0,x136V7->57,x137V2->5,x137V3->0,x137V4->21,x137V8->0,x138V2->1,x138V3->0,x138V4->6,x138V5->9,x138V6->0,x138V7->62,x138V8->0,x139V1->0,x139V3->0,x139V4->8,x139V5->0,x139V6->0,x139V8->0,x139V9->0,x140V1->0,x140V3->0,x140V4->1,x140V5->0,x140V7->0,x140V8->0,x140V9->0,x141V2->10,x141V4->28,x141V5->2,x141V6->0,x141V7->24,x141V8->0,x141V10->21,x142V1->0,x142V2->0,x142V3->6,x142V4->249,x142V6->0,x142V8->0,x142V9->0,x143V2->260,x143V6->0,x143V8->114,x143V9->2,x144V1->1,x144V7->8,x145V2->2,x145V5->4,x145V6->0,x145V7->34,x145V8->0,x145V10->273,x146V3->6,x146V5->21,x147V1->6,x147V4->164,x147V5->1,x147V6->0,x147V8->0,x147V10->2,x148V2->0,x148V4->25,x148V5->1,x148V7->26,x148V8->1,x149V4->12,x149V6->0,x150V4->2,x150V5->0,x150V6->0,x150V7->34,x150V8->0,x151V1->0,x151V2->2,x151V5->2,x151V6->0,x151V7->85,x151V9->0,x151V10->14,x152V2->0,x152V5->3,x152V7->10,x152V10->24,x153V6->0,x153V7->11,x153V9->16,x154V2->0,x154V4->10,x154V5->0,x154V6->0,x154V7->0,x154V9->0,x154V10->181,x155V3->1,x155V5->2,x155V7->7,x155V8->0,x155V10->2,x156V1->0,x156V3->2,x156V4->9,x156V6->0,x156V7->5,x156V9->0,x156V10->45,x157V1->0,x157V3->0,x157V4->8,x157V5->0,x157V6->0,x157V9->0,x158V1->17,x158V5->0,x158V6->0,x158V10->187,x159V7->13,x159V10->34,x160V2->0,x160V3->0,x160V4->9,x160V8->0,x160V9->0,x161V3->23,x161V5->2,x161V9->1,x162V1->0,x162V2->0,x162V3->1,x162V4->171,x162V6->0,x162V7->4,x162V8->0,x162V9->0,x162V10->0,x163V1->0,x163V4->5,x163V6->0,x163V7->6,x163V8->0,x163V10->0,x164V1->1,x164V3->13,x164V4->30,x164V5->0,x164V6->0,x164V9->0,x164V10->99,x165V2->16,x165V3->3,x165V6->0,x165V7->26,x165V8->1,x166V1->1,x166V3->0,x166V4->18,x166V6->3,x167V6->2,x167V9->4,x168V2->0,x168V3->0,x168V5->1,x168V6->0,x168V9->0,x168V10->1,x169V2->0,x169V4->4,x169V7->0,x169V8->0,x170V2->2,x170V4->47,x170V9->0,x171V3->0,x171V5->13,x171V6->0,x171V7->8,x172V1->5,x172V2->4,x172V4->22,x172V8->0,x172V9->0,x173V2->0,x173V3->0,x173V4->415,x173V6->0,x173V10->1,x174V1->0,x174V2->0,x174V3->0,x174V4->2,x174V6->0,x174V7->0,x174V8->0,x174V10->0,x175V2->0,x175V3->0,x175V4->204,x175V5->1,x175V6->0,x175V10->23,x176V3->14,x176V6->23,x177V2->0,x177V7->30,x178V1->0,x178V2->15,x178V3->5,x178V4->6,x178V5->0,x178V7->0,x178V10->1,x179V6->17,x179V7->112,x179V9->2,x180V4->57,x180V6->0,x180V7->1,x180V8->0,x180V9->0,x181V1->0,x181V5->0,x181V8->0,x181V10->2,x182V1->0,x182V2->0,x182V4->44,x182V8->0,x182V9->0,x182V10->69,x183V1->0,x183V2->0,x183V4->5,x183V8->0,x183V9->0,x184V1->1,x184V2->0,x184V4->71,x184V5->2,x184V6->3,x184V9->0,x184V10->96,x185V1->5,x185V2->226,x186V1->0,x186V2->0,x186V3->0,x186V4->1,x186V8->0,x186V9->0,x186V10->0,x187V3->5,x187V6->0,x187V7->68,x187V9->0,x187V10->39,x188V4->6,x188V5->0,x188V6->0,x188V7->17,x188V8->0,x188V9->0,x188V10->0,x189V1->0,x189V2->0,x189V4->13,x189V6->0,x189V10->2,x190V1->0,x190V2->0,x190V3->14,x190V4->349,x190V6->0,x190V8->0,x191V1->0,x191V2->0,x191V4->94,x191V8->1,x191V9->0,x192V1->0,x192V3->0,x192V4->107,x192V5->3,x192V6->0,x192V7->5,x192V9->0,x193V1->11,x193V5->20,x193V6->0,x193V7->156,x193V9->0,x193V10->3,x194V1->5,x194V3->45,x194V7->8,x194V10->2,x195V2->0,x195V4->21,x195V5->0,x195V8->0,x195V9->0,x195V10->7,x196V2->0,x196V3->0,x196V5->1,x196V7->0,x196V10->48,x197V7->0,x197V10->1,x198V1->8,x198V2->60,x198V5->3,x198V6->0,x198V9->0,x198V10->82,x199V1->0,x199V4->12,x199V5->0,x199V7->1,x199V8->0,x199V9->0,x199V10->6,x200V2->0,x200V3->12,x200V4->6,x200V5->0,x200V6->0,x200V10->5}}
Initial balances
(31699
54687
85057
149097
55197
35370
135474
80689
11090
97419
79605
28959
147861
72971
71512
93277
73274
50701
30051
95550

)
=============================================

Updated balances:
(17029
39352
61947
15147
36337
34085
87849
72414
8765
53214
79605
28959
147861
72971
71512
93277
73274
50701
30051
95550

)
=============================================

Initial contract amounts
(193
1161
654
1685
2639
1994
1523
2702
2685
1641
1207
2373
991
1042
2805
2438
2209
2438
2015
1391
357
573
1992
428
86
1145
508
2013
1858
1095
718
2522
1651
1424
2618
897
2855
883
1292
1910
2920
2758
906
2681
1270
1445
2150
88
1356
666
794
2629
2710
2957
353
2623
470
1980
2941
2928
2274
2666
1708
1826
665
938
2447
79
1253
2762
2504
357
1374
1906
484
2003
103
2059
2189
2860
1010
1599
1520
2337
1720
1283
784
724
1164
2307
2023
419
198
1140
871
1789
2116
242
116
613
674
1578
2063
479
1941
2959
645
1988
1200
2668
2836
1704
1449
2395
1987
2326
1880
2497
206
2763
1149
1259
195
1803
2456
320
2958
581
1958
1009
1389
1709
2349
444
376
981
2392
1481
1603
7
1598
2724
2148
1005
2525
1558
1179
1283
1897
1114
1314
2455
242
2638
1172
998
511
1176
2530
1523
1893
2334
377
1682
1543
2759
377
671
868
629
270
544
2308
153
1394
957
192
1284
1353
910
897
2919
1350
2914
1953
26
2814
1494
2348
1861
2611
927
2943
2818
1320
712
410
949
1925
829

)
=============================================

Updated contract amounts.
(-7
1
-1
0
-6
-1
-2
-3
-15
-4
-13
-2
1
-3
0
-12
-41
-2
0
-4
-43
-2
-8
-72
-4
5
-2
-2
-17
-5
-22
-3
-4
-1
-2
-3
0
-2
-8
0
0
-2
-4
-9
-30
-5
0
-2
-4
1
-6
-1
0
-3
-2
-2
0
-20
1
-2
-1
-9
-2
-4
-135
-2
-3
-321
-7
-3
-1
-43
-1
-4
-1
-2
-297
-1
-1
0
0
4
0
-13
0
-2
4
-6
-36
-8
-7
-1
-2
0
-4
-11
1
-8
-4
-2
-6
-2
-2
-1
-59
-1
0
-2
0
-7
-4
-1
-1
0
-13
-4
-20
-3
-394
-37
-1
-1
0
3
-4
-30
-2
-19
-2
-11
-1
-1
-1
-1
1
-4
-8
-4
-97
-73
-2
-6
-2
5
0
8
-1
-7
-3
-1
-1
-20
-3
-2
-3
-2
-39
-4
-20
-77
-7
-1
-3
-3
-2
-1
2
-229
-32
-1
0
-1
-2
-22
-1
-3
-3
-1
-2
0
-3
-1
-50
-1
-2
-474
-1
-6
-2
-4
-4
-3
-7
-2
0
-8
10
-1
-5
-6

)
=============================================
