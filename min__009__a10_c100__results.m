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


(* Test #4 *)
noOfContracts = 100;
noOfAssets = 10;


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

)
allExchangeRates
(1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1
1	1	1	1	1	1	1	1	1	1

)
allInterestRates
(54	1000000
40	1000000
93	1000000
50	1000000
6	1000000
8	1000000
81	1000000
38	1000000
80	1000000
34	1000000

)
allAssets
(7237
6700
50497
40180
37400
43915
58895
2335
68369
14162

)
Contract Cost
contr = {6,193,{{5,200,50},{6,20,5},{7,80,10}},148,0}
descr = {{5,200,50},{6,20,5},{7,80,10}}
payVal = {0,0,5}
cost = -10804
change = {0,0,0,0,0,0,-120,0,0,0}
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

)
allVarsLinear
{x1V5,x1V6,x1V7,x2V2,x2V5,x2V6,x2V7,x2V9,x2V10,x3V1,x3V3,x4V1,x4V2,x4V4,x4V8,x4V9,x4V10,x5V1,x5V2,x5V4,x5V6,x5V7,x5V8,x5V9,x6V1,x6V4,x6V8,x6V10,x7V2,x7V4,x7V6,x7V10,x8V3,x8V7,x8V8,x8V10,x9V1,x9V2,x9V4,x9V6,x9V8,x9V9,x9V10,x10V5,x10V7,x10V8,x10V10,x11V1,x11V2,x11V3,x11V4,x11V9,x11V10,x12V1,x12V3,x12V4,x12V5,x12V8,x12V9,x12V10,x13V1,x13V3,x13V4,x13V7,x13V10,x14V1,x14V2,x14V3,x14V4,x14V5,x14V6,x14V7,x14V10,x15V1,x15V2,x15V5,x16V1,x16V3,x17V2,x17V3,x17V4,x17V5,x17V6,x17V8,x17V9,x17V10,x18V1,x18V2,x18V3,x18V4,x18V5,x18V6,x18V7,x18V10,x19V3,x19V5,x19V8,x19V9,x19V10,x20V1,x20V2,x20V3,x20V6,x20V8,x20V10,x21V3,x21V5,x21V6,x21V7,x22V1,x22V3,x22V4,x22V6,x22V10,x23V4,x23V5,x24V1,x24V3,x24V4,x24V5,x24V7,x24V9,x24V10,x25V2,x25V4,x25V7,x25V9,x26V1,x26V6,x26V7,x26V9,x27V2,x27V4,x27V5,x28V1,x28V6,x28V7,x28V8,x28V9,x29V1,x29V2,x29V3,x29V4,x29V5,x29V8,x29V9,x30V1,x30V4,x31V1,x31V3,x31V5,x31V6,x31V7,x31V10,x32V1,x32V3,x32V8,x32V9,x32V10,x33V7,x33V10,x34V4,x34V6,x34V7,x34V8,x34V10,x35V3,x35V4,x35V8,x36V1,x36V2,x36V3,x36V4,x36V6,x36V9,x37V2,x37V3,x37V9,x38V8,x38V9,x39V2,x39V4,x39V8,x39V9,x40V1,x40V9,x41V1,x41V3,x41V5,x41V6,x41V7,x41V8,x41V9,x41V10,x42V1,x42V3,x42V4,x42V6,x42V7,x42V9,x42V10,x43V1,x43V3,x43V4,x43V5,x43V8,x43V10,x44V2,x44V4,x44V6,x44V7,x44V10,x45V1,x45V3,x45V8,x45V10,x46V1,x46V2,x46V3,x46V4,x46V5,x46V6,x47V1,x47V3,x47V5,x47V6,x47V8,x47V10,x48V2,x48V3,x48V4,x48V6,x48V7,x48V8,x49V2,x49V3,x49V4,x49V5,x49V9,x50V1,x50V3,x50V6,x50V8,x50V9,x51V1,x51V4,x51V6,x51V8,x51V9,x52V3,x52V8,x53V1,x53V2,x53V7,x53V8,x53V9,x53V10,x54V4,x54V10,x55V1,x55V2,x55V4,x55V6,x55V7,x55V8,x55V9,x55V10,x56V2,x56V3,x56V8,x57V1,x57V4,x57V5,x57V6,x57V8,x57V9,x57V10,x58V1,x58V2,x58V3,x58V4,x58V5,x58V6,x58V7,x58V8,x58V9,x58V10,x59V5,x59V7,x60V2,x60V4,x60V7,x60V9,x60V10,x61V2,x61V5,x61V7,x61V9,x62V2,x62V7,x63V3,x63V4,x63V5,x63V6,x63V7,x63V8,x63V9,x64V2,x64V4,x64V8,x65V1,x65V3,x65V4,x65V9,x65V10,x66V2,x66V3,x66V4,x66V5,x66V6,x66V8,x66V9,x67V4,x67V8,x68V1,x68V4,x68V5,x68V7,x68V8,x68V9,x69V1,x69V2,x69V4,x69V5,x69V7,x69V8,x69V9,x70V2,x70V3,x70V4,x70V6,x70V7,x70V10,x71V1,x71V3,x71V7,x71V8,x71V9,x72V1,x72V7,x73V1,x73V2,x73V6,x73V8,x73V9,x74V2,x74V3,x74V4,x74V6,x75V1,x75V4,x75V5,x75V6,x75V7,x75V9,x75V10,x76V2,x76V3,x76V4,x76V7,x76V8,x76V9,x76V10,x77V3,x77V4,x77V6,x77V10,x78V1,x78V2,x78V3,x78V4,x78V5,x78V6,x79V2,x79V6,x79V7,x80V1,x80V5,x80V9,x81V1,x81V3,x81V8,x81V9,x82V1,x82V2,x82V4,x82V6,x82V7,x82V9,x82V10,x83V1,x83V2,x83V9,x84V2,x84V4,x84V5,x84V6,x84V7,x84V8,x84V10,x85V1,x85V3,x85V4,x85V6,x85V7,x85V8,x85V9,x86V1,x86V2,x86V4,x86V6,x86V7,x86V8,x86V9,x86V10,x87V1,x87V6,x87V7,x87V8,x87V9,x88V1,x88V2,x88V4,x88V5,x88V6,x88V7,x88V8,x88V9,x88V10,x89V2,x89V6,x89V8,x90V1,x90V3,x90V4,x90V5,x90V7,x90V8,x90V9,x91V6,x91V8,x92V2,x92V4,x92V7,x92V10,x93V1,x93V2,x93V4,x93V5,x93V7,x93V8,x94V2,x94V3,x94V5,x94V7,x94V8,x94V9,x94V10,x95V1,x95V3,x95V5,x95V8,x95V9,x95V10,x96V3,x96V4,x96V5,x96V6,x96V7,x96V9,x96V10,x97V1,x97V2,x97V3,x97V6,x97V9,x98V1,x98V2,x98V3,x98V5,x98V6,x98V7,x98V9,x98V10,x99V3,x99V4,x99V9,x100V1,x100V3,x100V4,x100V6,x100V8,x100V9,x100V10}
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

)
allVarsWithRangeLinear
{{x1V5,0,1},{x1V6,0,36},{x1V7,0,13},{x2V2,0,3},{x2V5,0,112},{x2V6,0,114},{x2V7,0,15},{x2V9,0,228},{x2V10,0,17},{x3V1,0,128},{x3V3,0,4},{x4V1,0,25},{x4V2,0,27},{x4V4,0,331},{x4V8,0,335},{x4V9,0,333},{x4V10,0,329},{x5V1,0,22},{x5V2,0,20},{x5V4,0,259},{x5V6,0,101},{x5V7,0,101},{x5V8,0,22},{x5V9,0,257},{x6V1,0,390},{x6V4,0,71},{x6V8,0,390},{x6V10,0,35},{x7V2,0,144},{x7V4,0,56},{x7V6,0,144},{x7V10,0,150},{x8V3,0,532},{x8V7,0,104},{x8V8,0,538},{x8V10,0,532},{x9V1,0,24},{x9V2,0,18},{x9V4,0,103},{x9V6,0,105},{x9V8,0,22},{x9V9,0,51},{x9V10,0,22},{x10V5,0,320},{x10V7,0,326},{x10V8,0,63},{x10V10,0,324},{x11V1,0,112},{x11V2,0,239},{x11V3,0,20},{x11V4,0,20},{x11V9,0,46},{x11V10,0,40},{x12V1,0,19},{x12V3,0,235},{x12V4,0,229},{x12V5,0,235},{x12V8,0,231},{x12V9,0,17},{x12V10,0,472},{x13V1,0,91},{x13V3,0,17},{x13V4,0,5},{x13V7,0,95},{x13V10,0,5},{x14V1,0,202},{x14V2,0,96},{x14V3,0,204},{x14V4,0,200},{x14V5,0,14},{x14V6,0,33},{x14V7,0,33},{x14V10,0,200},{x15V1,0,553},{x15V2,0,108},{x15V5,0,104},{x16V1,0,93},{x16V3,0,91},{x17V2,0,20},{x17V3,0,437},{x17V4,0,42},{x17V5,0,80},{x17V6,0,40},{x17V8,0,437},{x17V9,0,84},{x17V10,0,20},{x18V1,0,22},{x18V2,0,485},{x18V3,0,239},{x18V4,0,93},{x18V5,0,22},{x18V6,0,91},{x18V7,0,46},{x18V10,0,481},{x19V3,0,16},{x19V5,0,14},{x19V8,0,395},{x19V9,0,12},{x19V10,0,78},{x20V1,0,9},{x20V2,0,274},{x20V3,0,53},{x20V6,0,47},{x20V8,0,11},{x20V10,0,276},{x21V3,0,1},{x21V5,0,12},{x21V6,0,63},{x21V7,0,3},{x22V1,0,106},{x22V3,0,18},{x22V4,0,1},{x22V6,0,112},{x22V10,0,106},{x23V4,0,71},{x23V5,0,35},{x24V1,0,1},{x24V3,0,13},{x24V4,0,1},{x24V5,0,6},{x24V7,0,4},{x24V9,0,6},{x24V10,0,1},{x25V2,0,1},{x25V4,0,2},{x25V7,0,9},{x25V9,0,1},{x26V1,0,221},{x26V6,0,225},{x26V7,0,16},{x26V9,0,227},{x27V2,0,14},{x27V4,0,93},{x27V5,0,12},{x28V1,0,32},{x28V6,0,199},{x28V7,0,394},{x28V8,0,34},{x28V9,0,394},{x29V1,0,35},{x29V2,0,70},{x29V3,0,29},{x29V4,0,68},{x29V5,0,14},{x29V8,0,72},{x29V9,0,14},{x30V1,0,2},{x30V4,0,41},{x31V1,0,1},{x31V3,0,137},{x31V5,0,6},{x31V6,0,6},{x31V7,0,24},{x31V10,0,3},{x32V1,0,23},{x32V3,0,17},{x32V8,0,21},{x32V9,0,44},{x32V10,0,502},{x33V7,0,324},{x33V10,0,324},{x34V4,0,278},{x34V6,0,138},{x34V7,0,136},{x34V8,0,280},{x34V10,0,278},{x35V3,0,50},{x35V4,0,253},{x35V8,0,98},{x36V1,0,9},{x36V2,0,11},{x36V3,0,15},{x36V4,0,27},{x36V6,0,171},{x36V9,0,173},{x37V2,0,569},{x37V3,0,567},{x37V9,0,281},{x38V8,0,170},{x38V9,0,29},{x39V2,0,49},{x39V4,0,45},{x39V8,0,45},{x39V9,0,123},{x40V1,0,378},{x40V9,0,11},{x41V1,0,110},{x41V3,0,54},{x41V5,0,284},{x41V6,0,23},{x41V7,0,25},{x41V8,0,288},{x41V9,0,290},{x41V10,0,582},{x42V1,0,19},{x42V3,0,47},{x42V4,0,19},{x42V6,0,21},{x42V7,0,269},{x42V9,0,104},{x42V10,0,108},{x43V1,0,34},{x43V3,0,32},{x43V4,0,3},{x43V5,0,88},{x43V8,0,16},{x43V10,0,14},{x44V2,0,47},{x44V4,0,266},{x44V6,0,262},{x44V7,0,49},{x44V10,0,45},{x45V1,0,8},{x45V3,0,119},{x45V8,0,125},{x45V10,0,19},{x46V1,0,49},{x46V2,0,20},{x46V3,0,140},{x46V4,0,49},{x46V5,0,26},{x46V6,0,10},{x47V1,0,13},{x47V3,0,422},{x47V5,0,35},{x47V6,0,426},{x47V8,0,41},{x47V10,0,41},{x48V2,0,1},{x48V3,0,1},{x48V4,0,13},{x48V6,0,1},{x48V7,0,1},{x48V8,0,1},{x49V2,0,133},{x49V3,0,7},{x49V4,0,269},{x49V5,0,23},{x49V9,0,52},{x50V1,0,22},{x50V3,0,131},{x50V6,0,5},{x50V8,0,5},{x50V9,0,60},{x51V1,0,71},{x51V4,0,1},{x51V6,0,13},{x51V8,0,11},{x51V9,0,5},{x52V3,0,517},{x52V8,0,48},{x53V1,0,46},{x53V2,0,23},{x53V7,0,25},{x53V8,0,267},{x53V9,0,104},{x53V10,0,269},{x54V4,0,51},{x54V10,0,289},{x55V1,0,33},{x55V2,0,33},{x55V4,0,68},{x55V6,0,1},{x55V7,0,1},{x55V8,0,27},{x55V9,0,29},{x55V10,0,1},{x56V2,0,522},{x56V3,0,96},{x56V8,0,18},{x57V1,0,1},{x57V4,0,1},{x57V5,0,14},{x57V6,0,43},{x57V8,0,90},{x57V9,0,1},{x57V10,0,1},{x58V1,0,71},{x58V2,0,13},{x58V3,0,31},{x58V4,0,77},{x58V5,0,33},{x58V6,0,394},{x58V7,0,190},{x58V8,0,194},{x58V9,0,190},{x58V10,0,11},{x59V5,0,586},{x59V7,0,115},{x60V2,0,109},{x60V4,0,21},{x60V7,0,583},{x60V9,0,290},{x60V10,0,579},{x61V2,0,82},{x61V5,0,16},{x61V7,0,219},{x61V9,0,82},{x62V2,0,49},{x62V7,0,100},{x63V3,0,66},{x63V4,0,32},{x63V5,0,64},{x63V6,0,28},{x63V7,0,168},{x63V8,0,339},{x63V9,0,337},{x64V2,0,67},{x64V4,0,176},{x64V8,0,357},{x65V1,0,7},{x65V3,0,9},{x65V4,0,24},{x65V9,0,5},{x65V10,0,1},{x66V2,0,5},{x66V3,0,29},{x66V4,0,183},{x66V5,0,33},{x66V6,0,179},{x66V8,0,85},{x66V9,0,10},{x67V4,0,240},{x67V8,0,22},{x68V1,0,1},{x68V4,0,1},{x68V5,0,11},{x68V7,0,1},{x68V8,0,7},{x68V9,0,1},{x69V1,0,242},{x69V2,0,121},{x69V4,0,117},{x69V5,0,42},{x69V7,0,21},{x69V8,0,46},{x69V9,0,244},{x70V2,0,546},{x70V3,0,272},{x70V4,0,272},{x70V6,0,106},{x70V7,0,544},{x70V10,0,544},{x71V1,0,496},{x71V3,0,92},{x71V7,0,17},{x71V8,0,248},{x71V9,0,494},{x72V1,0,1},{x72V7,0,1},{x73V1,0,21},{x73V2,0,266},{x73V6,0,272},{x73V8,0,131},{x73V9,0,23},{x74V2,0,375},{x74V3,0,30},{x74V4,0,184},{x74V6,0,379},{x75V1,0,1},{x75V4,0,92},{x75V5,0,3},{x75V6,0,40},{x75V7,0,3},{x75V9,0,94},{x75V10,0,94},{x76V2,0,16},{x76V3,0,78},{x76V4,0,32},{x76V7,0,394},{x76V8,0,194},{x76V9,0,12},{x76V10,0,72},{x77V3,0,1},{x77V4,0,1},{x77V6,0,1},{x77V10,0,16},{x78V1,0,409},{x78V2,0,199},{x78V3,0,199},{x78V4,0,409},{x78V5,0,203},{x78V6,0,203},{x79V2,0,210},{x79V6,0,15},{x79V7,0,83},{x80V1,0,282},{x80V5,0,22},{x80V9,0,278},{x81V1,0,200},{x81V3,0,38},{x81V8,0,36},{x81V9,0,36},{x82V1,0,29},{x82V2,0,13},{x82V4,0,59},{x82V6,0,25},{x82V7,0,157},{x82V9,0,55},{x82V10,0,153},{x83V1,0,28},{x83V2,0,296},{x83V9,0,26},{x84V2,0,229},{x84V4,0,42},{x84V5,0,44},{x84V6,0,229},{x84V7,0,229},{x84V8,0,229},{x84V10,0,40},{x85V1,0,32},{x85V3,0,32},{x85V4,0,170},{x85V6,0,11},{x85V7,0,340},{x85V8,0,66},{x85V9,0,15},{x86V1,0,248},{x86V2,0,21},{x86V4,0,120},{x86V6,0,10},{x86V7,0,120},{x86V8,0,126},{x86V9,0,252},{x86V10,0,49},{x87V1,0,11},{x87V6,0,152},{x87V7,0,25},{x87V8,0,70},{x87V9,0,25},{x88V1,0,64},{x88V2,0,20},{x88V4,0,26},{x88V5,0,142},{x88V6,0,22},{x88V7,0,22},{x88V8,0,5},{x88V9,0,26},{x88V10,0,3},{x89V2,0,5},{x89V6,0,9},{x89V8,0,17},{x90V1,0,226},{x90V3,0,88},{x90V4,0,222},{x90V5,0,88},{x90V7,0,38},{x90V8,0,84},{x90V9,0,21},{x91V6,0,14},{x91V8,0,194},{x92V2,0,1},{x92V4,0,81},{x92V7,0,8},{x92V10,0,10},{x93V1,0,1},{x93V2,0,1},{x93V4,0,35},{x93V5,0,1},{x93V7,0,17},{x93V8,0,37},{x94V2,0,110},{x94V3,0,37},{x94V5,0,39},{x94V7,0,14},{x94V8,0,7},{x94V9,0,16},{x94V10,0,224},{x95V1,0,28},{x95V3,0,13},{x95V5,0,6},{x95V8,0,13},{x95V9,0,28},{x95V10,0,170},{x96V3,0,31},{x96V4,0,33},{x96V5,0,176},{x96V6,0,349},{x96V7,0,63},{x96V9,0,355},{x96V10,0,65},{x97V1,0,419},{x97V2,0,415},{x97V3,0,78},{x97V6,0,15},{x97V9,0,82},{x98V1,0,1},{x98V2,0,1},{x98V3,0,20},{x98V5,0,22},{x98V6,0,44},{x98V7,0,5},{x98V9,0,40},{x98V10,0,1},{x99V3,0,9},{x99V4,0,17},{x99V9,0,1},{x100V1,0,118},{x100V3,0,2},{x100V4,0,55},{x100V6,0,116},{x100V8,0,1},{x100V9,0,120},{x100V10,0,1}}
nonNegativeCond
x1V5>=0&&x1V6>=0&&x1V7>=0&&x2V2>=0&&x2V5>=0&&x2V6>=0&&x2V7>=0&&x2V9>=0&&x2V10>=0&&x3V1>=0&&x3V3>=0&&x4V1>=0&&x4V2>=0&&x4V4>=0&&x4V8>=0&&x4V9>=0&&x4V10>=0&&x5V1>=0&&x5V2>=0&&x5V4>=0&&x5V6>=0&&x5V7>=0&&x5V8>=0&&x5V9>=0&&x6V1>=0&&x6V4>=0&&x6V8>=0&&x6V10>=0&&x7V2>=0&&x7V4>=0&&x7V6>=0&&x7V10>=0&&x8V3>=0&&x8V7>=0&&x8V8>=0&&x8V10>=0&&x9V1>=0&&x9V2>=0&&x9V4>=0&&x9V6>=0&&x9V8>=0&&x9V9>=0&&x9V10>=0&&x10V5>=0&&x10V7>=0&&x10V8>=0&&x10V10>=0&&x11V1>=0&&x11V2>=0&&x11V3>=0&&x11V4>=0&&x11V9>=0&&x11V10>=0&&x12V1>=0&&x12V3>=0&&x12V4>=0&&x12V5>=0&&x12V8>=0&&x12V9>=0&&x12V10>=0&&x13V1>=0&&x13V3>=0&&x13V4>=0&&x13V7>=0&&x13V10>=0&&x14V1>=0&&x14V2>=0&&x14V3>=0&&x14V4>=0&&x14V5>=0&&x14V6>=0&&x14V7>=0&&x14V10>=0&&x15V1>=0&&x15V2>=0&&x15V5>=0&&x16V1>=0&&x16V3>=0&&x17V2>=0&&x17V3>=0&&x17V4>=0&&x17V5>=0&&x17V6>=0&&x17V8>=0&&x17V9>=0&&x17V10>=0&&x18V1>=0&&x18V2>=0&&x18V3>=0&&x18V4>=0&&x18V5>=0&&x18V6>=0&&x18V7>=0&&x18V10>=0&&x19V3>=0&&x19V5>=0&&x19V8>=0&&x19V9>=0&&x19V10>=0&&x20V1>=0&&x20V2>=0&&x20V3>=0&&x20V6>=0&&x20V8>=0&&x20V10>=0&&x21V3>=0&&x21V5>=0&&x21V6>=0&&x21V7>=0&&x22V1>=0&&x22V3>=0&&x22V4>=0&&x22V6>=0&&x22V10>=0&&x23V4>=0&&x23V5>=0&&x24V1>=0&&x24V3>=0&&x24V4>=0&&x24V5>=0&&x24V7>=0&&x24V9>=0&&x24V10>=0&&x25V2>=0&&x25V4>=0&&x25V7>=0&&x25V9>=0&&x26V1>=0&&x26V6>=0&&x26V7>=0&&x26V9>=0&&x27V2>=0&&x27V4>=0&&x27V5>=0&&x28V1>=0&&x28V6>=0&&x28V7>=0&&x28V8>=0&&x28V9>=0&&x29V1>=0&&x29V2>=0&&x29V3>=0&&x29V4>=0&&x29V5>=0&&x29V8>=0&&x29V9>=0&&x30V1>=0&&x30V4>=0&&x31V1>=0&&x31V3>=0&&x31V5>=0&&x31V6>=0&&x31V7>=0&&x31V10>=0&&x32V1>=0&&x32V3>=0&&x32V8>=0&&x32V9>=0&&x32V10>=0&&x33V7>=0&&x33V10>=0&&x34V4>=0&&x34V6>=0&&x34V7>=0&&x34V8>=0&&x34V10>=0&&x35V3>=0&&x35V4>=0&&x35V8>=0&&x36V1>=0&&x36V2>=0&&x36V3>=0&&x36V4>=0&&x36V6>=0&&x36V9>=0&&x37V2>=0&&x37V3>=0&&x37V9>=0&&x38V8>=0&&x38V9>=0&&x39V2>=0&&x39V4>=0&&x39V8>=0&&x39V9>=0&&x40V1>=0&&x40V9>=0&&x41V1>=0&&x41V3>=0&&x41V5>=0&&x41V6>=0&&x41V7>=0&&x41V8>=0&&x41V9>=0&&x41V10>=0&&x42V1>=0&&x42V3>=0&&x42V4>=0&&x42V6>=0&&x42V7>=0&&x42V9>=0&&x42V10>=0&&x43V1>=0&&x43V3>=0&&x43V4>=0&&x43V5>=0&&x43V8>=0&&x43V10>=0&&x44V2>=0&&x44V4>=0&&x44V6>=0&&x44V7>=0&&x44V10>=0&&x45V1>=0&&x45V3>=0&&x45V8>=0&&x45V10>=0&&x46V1>=0&&x46V2>=0&&x46V3>=0&&x46V4>=0&&x46V5>=0&&x46V6>=0&&x47V1>=0&&x47V3>=0&&x47V5>=0&&x47V6>=0&&x47V8>=0&&x47V10>=0&&x48V2>=0&&x48V3>=0&&x48V4>=0&&x48V6>=0&&x48V7>=0&&x48V8>=0&&x49V2>=0&&x49V3>=0&&x49V4>=0&&x49V5>=0&&x49V9>=0&&x50V1>=0&&x50V3>=0&&x50V6>=0&&x50V8>=0&&x50V9>=0&&x51V1>=0&&x51V4>=0&&x51V6>=0&&x51V8>=0&&x51V9>=0&&x52V3>=0&&x52V8>=0&&x53V1>=0&&x53V2>=0&&x53V7>=0&&x53V8>=0&&x53V9>=0&&x53V10>=0&&x54V4>=0&&x54V10>=0&&x55V1>=0&&x55V2>=0&&x55V4>=0&&x55V6>=0&&x55V7>=0&&x55V8>=0&&x55V9>=0&&x55V10>=0&&x56V2>=0&&x56V3>=0&&x56V8>=0&&x57V1>=0&&x57V4>=0&&x57V5>=0&&x57V6>=0&&x57V8>=0&&x57V9>=0&&x57V10>=0&&x58V1>=0&&x58V2>=0&&x58V3>=0&&x58V4>=0&&x58V5>=0&&x58V6>=0&&x58V7>=0&&x58V8>=0&&x58V9>=0&&x58V10>=0&&x59V5>=0&&x59V7>=0&&x60V2>=0&&x60V4>=0&&x60V7>=0&&x60V9>=0&&x60V10>=0&&x61V2>=0&&x61V5>=0&&x61V7>=0&&x61V9>=0&&x62V2>=0&&x62V7>=0&&x63V3>=0&&x63V4>=0&&x63V5>=0&&x63V6>=0&&x63V7>=0&&x63V8>=0&&x63V9>=0&&x64V2>=0&&x64V4>=0&&x64V8>=0&&x65V1>=0&&x65V3>=0&&x65V4>=0&&x65V9>=0&&x65V10>=0&&x66V2>=0&&x66V3>=0&&x66V4>=0&&x66V5>=0&&x66V6>=0&&x66V8>=0&&x66V9>=0&&x67V4>=0&&x67V8>=0&&x68V1>=0&&x68V4>=0&&x68V5>=0&&x68V7>=0&&x68V8>=0&&x68V9>=0&&x69V1>=0&&x69V2>=0&&x69V4>=0&&x69V5>=0&&x69V7>=0&&x69V8>=0&&x69V9>=0&&x70V2>=0&&x70V3>=0&&x70V4>=0&&x70V6>=0&&x70V7>=0&&x70V10>=0&&x71V1>=0&&x71V3>=0&&x71V7>=0&&x71V8>=0&&x71V9>=0&&x72V1>=0&&x72V7>=0&&x73V1>=0&&x73V2>=0&&x73V6>=0&&x73V8>=0&&x73V9>=0&&x74V2>=0&&x74V3>=0&&x74V4>=0&&x74V6>=0&&x75V1>=0&&x75V4>=0&&x75V5>=0&&x75V6>=0&&x75V7>=0&&x75V9>=0&&x75V10>=0&&x76V2>=0&&x76V3>=0&&x76V4>=0&&x76V7>=0&&x76V8>=0&&x76V9>=0&&x76V10>=0&&x77V3>=0&&x77V4>=0&&x77V6>=0&&x77V10>=0&&x78V1>=0&&x78V2>=0&&x78V3>=0&&x78V4>=0&&x78V5>=0&&x78V6>=0&&x79V2>=0&&x79V6>=0&&x79V7>=0&&x80V1>=0&&x80V5>=0&&x80V9>=0&&x81V1>=0&&x81V3>=0&&x81V8>=0&&x81V9>=0&&x82V1>=0&&x82V2>=0&&x82V4>=0&&x82V6>=0&&x82V7>=0&&x82V9>=0&&x82V10>=0&&x83V1>=0&&x83V2>=0&&x83V9>=0&&x84V2>=0&&x84V4>=0&&x84V5>=0&&x84V6>=0&&x84V7>=0&&x84V8>=0&&x84V10>=0&&x85V1>=0&&x85V3>=0&&x85V4>=0&&x85V6>=0&&x85V7>=0&&x85V8>=0&&x85V9>=0&&x86V1>=0&&x86V2>=0&&x86V4>=0&&x86V6>=0&&x86V7>=0&&x86V8>=0&&x86V9>=0&&x86V10>=0&&x87V1>=0&&x87V6>=0&&x87V7>=0&&x87V8>=0&&x87V9>=0&&x88V1>=0&&x88V2>=0&&x88V4>=0&&x88V5>=0&&x88V6>=0&&x88V7>=0&&x88V8>=0&&x88V9>=0&&x88V10>=0&&x89V2>=0&&x89V6>=0&&x89V8>=0&&x90V1>=0&&x90V3>=0&&x90V4>=0&&x90V5>=0&&x90V7>=0&&x90V8>=0&&x90V9>=0&&x91V6>=0&&x91V8>=0&&x92V2>=0&&x92V4>=0&&x92V7>=0&&x92V10>=0&&x93V1>=0&&x93V2>=0&&x93V4>=0&&x93V5>=0&&x93V7>=0&&x93V8>=0&&x94V2>=0&&x94V3>=0&&x94V5>=0&&x94V7>=0&&x94V8>=0&&x94V9>=0&&x94V10>=0&&x95V1>=0&&x95V3>=0&&x95V5>=0&&x95V8>=0&&x95V9>=0&&x95V10>=0&&x96V3>=0&&x96V4>=0&&x96V5>=0&&x96V6>=0&&x96V7>=0&&x96V9>=0&&x96V10>=0&&x97V1>=0&&x97V2>=0&&x97V3>=0&&x97V6>=0&&x97V9>=0&&x98V1>=0&&x98V2>=0&&x98V3>=0&&x98V5>=0&&x98V6>=0&&x98V7>=0&&x98V9>=0&&x98V10>=0&&x99V3>=0&&x99V4>=0&&x99V9>=0&&x100V1>=0&&x100V3>=0&&x100V4>=0&&x100V6>=0&&x100V8>=0&&x100V9>=0&&x100V10>=0
upperBoundCond
x1V5<=1&&x1V6<=36&&x1V7<=13&&x2V2<=3&&x2V5<=112&&x2V6<=114&&x2V7<=15&&x2V9<=228&&x2V10<=17&&x3V1<=128&&x3V3<=4&&x4V1<=25&&x4V2<=27&&x4V4<=331&&x4V8<=335&&x4V9<=333&&x4V10<=329&&x5V1<=22&&x5V2<=20&&x5V4<=259&&x5V6<=101&&x5V7<=101&&x5V8<=22&&x5V9<=257&&x6V1<=390&&x6V4<=71&&x6V8<=390&&x6V10<=35&&x7V2<=144&&x7V4<=56&&x7V6<=144&&x7V10<=150&&x8V3<=532&&x8V7<=104&&x8V8<=538&&x8V10<=532&&x9V1<=24&&x9V2<=18&&x9V4<=103&&x9V6<=105&&x9V8<=22&&x9V9<=51&&x9V10<=22&&x10V5<=320&&x10V7<=326&&x10V8<=63&&x10V10<=324&&x11V1<=112&&x11V2<=239&&x11V3<=20&&x11V4<=20&&x11V9<=46&&x11V10<=40&&x12V1<=19&&x12V3<=235&&x12V4<=229&&x12V5<=235&&x12V8<=231&&x12V9<=17&&x12V10<=472&&x13V1<=91&&x13V3<=17&&x13V4<=5&&x13V7<=95&&x13V10<=5&&x14V1<=202&&x14V2<=96&&x14V3<=204&&x14V4<=200&&x14V5<=14&&x14V6<=33&&x14V7<=33&&x14V10<=200&&x15V1<=553&&x15V2<=108&&x15V5<=104&&x16V1<=93&&x16V3<=91&&x17V2<=20&&x17V3<=437&&x17V4<=42&&x17V5<=80&&x17V6<=40&&x17V8<=437&&x17V9<=84&&x17V10<=20&&x18V1<=22&&x18V2<=485&&x18V3<=239&&x18V4<=93&&x18V5<=22&&x18V6<=91&&x18V7<=46&&x18V10<=481&&x19V3<=16&&x19V5<=14&&x19V8<=395&&x19V9<=12&&x19V10<=78&&x20V1<=9&&x20V2<=274&&x20V3<=53&&x20V6<=47&&x20V8<=11&&x20V10<=276&&x21V3<=1&&x21V5<=12&&x21V6<=63&&x21V7<=3&&x22V1<=106&&x22V3<=18&&x22V4<=1&&x22V6<=112&&x22V10<=106&&x23V4<=71&&x23V5<=35&&x24V1<=1&&x24V3<=13&&x24V4<=1&&x24V5<=6&&x24V7<=4&&x24V9<=6&&x24V10<=1&&x25V2<=1&&x25V4<=2&&x25V7<=9&&x25V9<=1&&x26V1<=221&&x26V6<=225&&x26V7<=16&&x26V9<=227&&x27V2<=14&&x27V4<=93&&x27V5<=12&&x28V1<=32&&x28V6<=199&&x28V7<=394&&x28V8<=34&&x28V9<=394&&x29V1<=35&&x29V2<=70&&x29V3<=29&&x29V4<=68&&x29V5<=14&&x29V8<=72&&x29V9<=14&&x30V1<=2&&x30V4<=41&&x31V1<=1&&x31V3<=137&&x31V5<=6&&x31V6<=6&&x31V7<=24&&x31V10<=3&&x32V1<=23&&x32V3<=17&&x32V8<=21&&x32V9<=44&&x32V10<=502&&x33V7<=324&&x33V10<=324&&x34V4<=278&&x34V6<=138&&x34V7<=136&&x34V8<=280&&x34V10<=278&&x35V3<=50&&x35V4<=253&&x35V8<=98&&x36V1<=9&&x36V2<=11&&x36V3<=15&&x36V4<=27&&x36V6<=171&&x36V9<=173&&x37V2<=569&&x37V3<=567&&x37V9<=281&&x38V8<=170&&x38V9<=29&&x39V2<=49&&x39V4<=45&&x39V8<=45&&x39V9<=123&&x40V1<=378&&x40V9<=11&&x41V1<=110&&x41V3<=54&&x41V5<=284&&x41V6<=23&&x41V7<=25&&x41V8<=288&&x41V9<=290&&x41V10<=582&&x42V1<=19&&x42V3<=47&&x42V4<=19&&x42V6<=21&&x42V7<=269&&x42V9<=104&&x42V10<=108&&x43V1<=34&&x43V3<=32&&x43V4<=3&&x43V5<=88&&x43V8<=16&&x43V10<=14&&x44V2<=47&&x44V4<=266&&x44V6<=262&&x44V7<=49&&x44V10<=45&&x45V1<=8&&x45V3<=119&&x45V8<=125&&x45V10<=19&&x46V1<=49&&x46V2<=20&&x46V3<=140&&x46V4<=49&&x46V5<=26&&x46V6<=10&&x47V1<=13&&x47V3<=422&&x47V5<=35&&x47V6<=426&&x47V8<=41&&x47V10<=41&&x48V2<=1&&x48V3<=1&&x48V4<=13&&x48V6<=1&&x48V7<=1&&x48V8<=1&&x49V2<=133&&x49V3<=7&&x49V4<=269&&x49V5<=23&&x49V9<=52&&x50V1<=22&&x50V3<=131&&x50V6<=5&&x50V8<=5&&x50V9<=60&&x51V1<=71&&x51V4<=1&&x51V6<=13&&x51V8<=11&&x51V9<=5&&x52V3<=517&&x52V8<=48&&x53V1<=46&&x53V2<=23&&x53V7<=25&&x53V8<=267&&x53V9<=104&&x53V10<=269&&x54V4<=51&&x54V10<=289&&x55V1<=33&&x55V2<=33&&x55V4<=68&&x55V6<=1&&x55V7<=1&&x55V8<=27&&x55V9<=29&&x55V10<=1&&x56V2<=522&&x56V3<=96&&x56V8<=18&&x57V1<=1&&x57V4<=1&&x57V5<=14&&x57V6<=43&&x57V8<=90&&x57V9<=1&&x57V10<=1&&x58V1<=71&&x58V2<=13&&x58V3<=31&&x58V4<=77&&x58V5<=33&&x58V6<=394&&x58V7<=190&&x58V8<=194&&x58V9<=190&&x58V10<=11&&x59V5<=586&&x59V7<=115&&x60V2<=109&&x60V4<=21&&x60V7<=583&&x60V9<=290&&x60V10<=579&&x61V2<=82&&x61V5<=16&&x61V7<=219&&x61V9<=82&&x62V2<=49&&x62V7<=100&&x63V3<=66&&x63V4<=32&&x63V5<=64&&x63V6<=28&&x63V7<=168&&x63V8<=339&&x63V9<=337&&x64V2<=67&&x64V4<=176&&x64V8<=357&&x65V1<=7&&x65V3<=9&&x65V4<=24&&x65V9<=5&&x65V10<=1&&x66V2<=5&&x66V3<=29&&x66V4<=183&&x66V5<=33&&x66V6<=179&&x66V8<=85&&x66V9<=10&&x67V4<=240&&x67V8<=22&&x68V1<=1&&x68V4<=1&&x68V5<=11&&x68V7<=1&&x68V8<=7&&x68V9<=1&&x69V1<=242&&x69V2<=121&&x69V4<=117&&x69V5<=42&&x69V7<=21&&x69V8<=46&&x69V9<=244&&x70V2<=546&&x70V3<=272&&x70V4<=272&&x70V6<=106&&x70V7<=544&&x70V10<=544&&x71V1<=496&&x71V3<=92&&x71V7<=17&&x71V8<=248&&x71V9<=494&&x72V1<=1&&x72V7<=1&&x73V1<=21&&x73V2<=266&&x73V6<=272&&x73V8<=131&&x73V9<=23&&x74V2<=375&&x74V3<=30&&x74V4<=184&&x74V6<=379&&x75V1<=1&&x75V4<=92&&x75V5<=3&&x75V6<=40&&x75V7<=3&&x75V9<=94&&x75V10<=94&&x76V2<=16&&x76V3<=78&&x76V4<=32&&x76V7<=394&&x76V8<=194&&x76V9<=12&&x76V10<=72&&x77V3<=1&&x77V4<=1&&x77V6<=1&&x77V10<=16&&x78V1<=409&&x78V2<=199&&x78V3<=199&&x78V4<=409&&x78V5<=203&&x78V6<=203&&x79V2<=210&&x79V6<=15&&x79V7<=83&&x80V1<=282&&x80V5<=22&&x80V9<=278&&x81V1<=200&&x81V3<=38&&x81V8<=36&&x81V9<=36&&x82V1<=29&&x82V2<=13&&x82V4<=59&&x82V6<=25&&x82V7<=157&&x82V9<=55&&x82V10<=153&&x83V1<=28&&x83V2<=296&&x83V9<=26&&x84V2<=229&&x84V4<=42&&x84V5<=44&&x84V6<=229&&x84V7<=229&&x84V8<=229&&x84V10<=40&&x85V1<=32&&x85V3<=32&&x85V4<=170&&x85V6<=11&&x85V7<=340&&x85V8<=66&&x85V9<=15&&x86V1<=248&&x86V2<=21&&x86V4<=120&&x86V6<=10&&x86V7<=120&&x86V8<=126&&x86V9<=252&&x86V10<=49&&x87V1<=11&&x87V6<=152&&x87V7<=25&&x87V8<=70&&x87V9<=25&&x88V1<=64&&x88V2<=20&&x88V4<=26&&x88V5<=142&&x88V6<=22&&x88V7<=22&&x88V8<=5&&x88V9<=26&&x88V10<=3&&x89V2<=5&&x89V6<=9&&x89V8<=17&&x90V1<=226&&x90V3<=88&&x90V4<=222&&x90V5<=88&&x90V7<=38&&x90V8<=84&&x90V9<=21&&x91V6<=14&&x91V8<=194&&x92V2<=1&&x92V4<=81&&x92V7<=8&&x92V10<=10&&x93V1<=1&&x93V2<=1&&x93V4<=35&&x93V5<=1&&x93V7<=17&&x93V8<=37&&x94V2<=110&&x94V3<=37&&x94V5<=39&&x94V7<=14&&x94V8<=7&&x94V9<=16&&x94V10<=224&&x95V1<=28&&x95V3<=13&&x95V5<=6&&x95V8<=13&&x95V9<=28&&x95V10<=170&&x96V3<=31&&x96V4<=33&&x96V5<=176&&x96V6<=349&&x96V7<=63&&x96V9<=355&&x96V10<=65&&x97V1<=419&&x97V2<=415&&x97V3<=78&&x97V6<=15&&x97V9<=82&&x98V1<=1&&x98V2<=1&&x98V3<=20&&x98V5<=22&&x98V6<=44&&x98V7<=5&&x98V9<=40&&x98V10<=1&&x99V3<=9&&x99V4<=17&&x99V9<=1&&x100V1<=118&&x100V3<=2&&x100V4<=55&&x100V6<=116&&x100V8<=1&&x100V9<=120&&x100V10<=1
nonNegBalanceCond
7237-If[x100V1>0,30+5 (-1+x100V1),0]-If[x11V1>0,100+10 (-1+x11V1),0]-If[x12V1>0,600+100 (-1+x12V1),0]-If[x13V1>0,100+10 (-1+x13V1),0]-If[x14V1>0,40+5 (-1+x14V1),0]-If[x15V1>0,50+5 (-1+x15V1),0]-If[x16V1>0,150+25 (-1+x16V1),0]-If[x18V1>0,400+100 (-1+x18V1),0]-If[x20V1>0,600+100 (-1+x20V1),0]-If[x22V1>0,50+5 (-1+x22V1),0]-If[x24V1>0,500+50 (-1+x24V1),0]-If[x26V1>0,50+5 (-1+x26V1),0]-If[x28V1>0,500+50 (-1+x28V1),0]-If[x29V1>0,200+50 (-1+x29V1),0]-If[x30V1>0,1000+100 (-1+x30V1),0]-If[x31V1>0,1000+100 (-1+x31V1),0]-If[x32V1>0,400+100 (-1+x32V1),0]-If[x36V1>0,500+50 (-1+x36V1),0]-If[x3V1>0,20+5 (-1+x3V1),0]-If[x40V1>0,30+5 (-1+x40V1),0]-If[x41V1>0,200+25 (-1+x41V1),0]-If[x42V1>0,1000+100 (-1+x42V1),0]-If[x43V1>0,100+25 (-1+x43V1),0]-If[x45V1>0,600+100 (-1+x45V1),0]-If[x46V1>0,250+25 (-1+x46V1),0]-If[x47V1>0,1000+100 (-1+x47V1),0]-If[x4V1>0,500+50 (-1+x4V1),0]-If[x50V1>0,150+25 (-1+x50V1),0]-If[x51V1>0,100+10 (-1+x51V1),0]-If[x53V1>0,500+50 (-1+x53V1),0]-If[x55V1>0,40+10 (-1+x55V1),0]-If[x57V1>0,500+50 (-1+x57V1),0]-If[x58V1>0,250+25 (-1+x58V1),0]-If[x5V1>0,600+100 (-1+x5V1),0]-If[x65V1>0,400+50 (-1+x65V1),0]-If[x68V1>0,150+25 (-1+x68V1),0]-If[x69V1>0,50+5 (-1+x69V1),0]-If[x6V1>0,50+5 (-1+x6V1),0]-If[x71V1>0,30+5 (-1+x71V1),0]-If[x72V1>0,800+100 (-1+x72V1),0]-If[x73V1>0,400+50 (-1+x73V1),0]-If[x75V1>0,500+50 (-1+x75V1),0]-If[x78V1>0,20+5 (-1+x78V1),0]-If[x80V1>0,60+10 (-1+x80V1),0]-If[x81V1>0,20+5 (-1+x81V1),0]-If[x82V1>0,200+50 (-1+x82V1),0]-If[x83V1>0,200+50 (-1+x83V1),0]-If[x85V1>0,200+50 (-1+x85V1),0]-If[x86V1>0,50+5 (-1+x86V1),0]-If[x87V1>0,300+50 (-1+x87V1),0]-If[x88V1>0,100+10 (-1+x88V1),0]-If[x90V1>0,60+10 (-1+x90V1),0]-If[x93V1>0,200+25 (-1+x93V1),0]-If[x95V1>0,200+25 (-1+x95V1),0]-If[x97V1>0,30+5 (-1+x97V1),0]-If[x98V1>0,400+100 (-1+x98V1),0]-If[x9V1>0,400+100 (-1+x9V1),0]>=0&&6700-If[x11V2>0,20+5 (-1+x11V2),0]-If[x14V2>0,100+10 (-1+x14V2),0]-If[x15V2>0,150+25 (-1+x15V2),0]-If[x17V2>0,400+100 (-1+x17V2),0]-If[x18V2>0,20+5 (-1+x18V2),0]-If[x20V2>0,30+5 (-1+x20V2),0]-If[x25V2>0,1000+100 (-1+x25V2),0]-If[x27V2>0,200+25 (-1+x27V2),0]-If[x29V2>0,150+25 (-1+x29V2),0]-If[x2V2>0,1000+100 (-1+x2V2),0]-If[x36V2>0,400+50 (-1+x36V2),0]-If[x37V2>0,20+5 (-1+x37V2),0]-If[x39V2>0,100+25 (-1+x39V2),0]-If[x44V2>0,400+50 (-1+x44V2),0]-If[x46V2>0,500+50 (-1+x46V2),0]-If[x48V2>0,300+50 (-1+x48V2),0]-If[x49V2>0,40+10 (-1+x49V2),0]-If[x4V2>0,400+50 (-1+x4V2),0]-If[x53V2>0,600+100 (-1+x53V2),0]-If[x55V2>0,40+10 (-1+x55V2),0]-If[x56V2>0,20+5 (-1+x56V2),0]-If[x58V2>0,800+100 (-1+x58V2),0]-If[x5V2>0,800+100 (-1+x5V2),0]-If[x60V2>0,250+25 (-1+x60V2),0]-If[x61V2>0,250+25 (-1+x61V2),0]-If[x62V2>0,300+50 (-1+x62V2),0]-If[x64V2>0,200+25 (-1+x64V2),0]-If[x66V2>0,600+100 (-1+x66V2),0]-If[x69V2>0,60+10 (-1+x69V2),0]-If[x70V2>0,40+5 (-1+x70V2),0]-If[x73V2>0,50+5 (-1+x73V2),0]-If[x74V2>0,40+5 (-1+x74V2),0]-If[x76V2>0,600+100 (-1+x76V2),0]-If[x78V2>0,80+10 (-1+x78V2),0]-If[x79V2>0,100+10 (-1+x79V2),0]-If[x7V2>0,100+10 (-1+x7V2),0]-If[x82V2>0,400+100 (-1+x82V2),0]-If[x83V2>0,50+5 (-1+x83V2),0]-If[x84V2>0,60+10 (-1+x84V2),0]-If[x86V2>0,300+50 (-1+x86V2),0]-If[x88V2>0,250+25 (-1+x88V2),0]-If[x89V2>0,800+100 (-1+x89V2),0]-If[x92V2>0,800+100 (-1+x92V2),0]-If[x93V2>0,300+50 (-1+x93V2),0]-If[x94V2>0,60+10 (-1+x94V2),0]-If[x97V2>0,50+5 (-1+x97V2),0]-If[x98V2>0,1000+100 (-1+x98V2),0]-If[x9V2>0,1000+100 (-1+x9V2),0]>=0&&50497-If[x100V3>0,600+100 (-1+x100V3),0]-If[x11V3>0,300+50 (-1+x11V3),0]-If[x12V3>0,40+10 (-1+x12V3),0]-If[x13V3>0,200+50 (-1+x13V3),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x16V3>0,200+25 (-1+x16V3),0]-If[x17V3>0,30+5 (-1+x17V3),0]-If[x18V3>0,60+10 (-1+x18V3),0]-If[x19V3>0,600+100 (-1+x19V3),0]-If[x20V3>0,100+25 (-1+x20V3),0]-If[x21V3>0,600+100 (-1+x21V3),0]-If[x22V3>0,150+25 (-1+x22V3),0]-If[x24V3>0,150+25 (-1+x24V3),0]-If[x29V3>0,500+50 (-1+x29V3),0]-If[x31V3>0,40+5 (-1+x31V3),0]-If[x32V3>0,1000+100 (-1+x32V3),0]-If[x35V3>0,200+50 (-1+x35V3),0]-If[x36V3>0,200+50 (-1+x36V3),0]-If[x37V3>0,30+5 (-1+x37V3),0]-If[x3V3>0,400+100 (-1+x3V3),0]-If[x41V3>0,300+50 (-1+x41V3),0]-If[x42V3>0,500+50 (-1+x42V3),0]-If[x43V3>0,150+25 (-1+x43V3),0]-If[x45V3>0,100+10 (-1+x45V3),0]-If[x46V3>0,60+10 (-1+x46V3),0]-If[x47V3>0,50+5 (-1+x47V3),0]-If[x48V3>0,200+50 (-1+x48V3),0]-If[x49V3>0,800+100 (-1+x49V3),0]-If[x50V3>0,20+5 (-1+x50V3),0]-If[x52V3>0,50+5 (-1+x52V3),0]-If[x56V3>0,250+25 (-1+x56V3),0]-If[x58V3>0,500+50 (-1+x58V3),0]-If[x63V3>0,100+25 (-1+x63V3),0]-If[x65V3>0,300+50 (-1+x65V3),0]-If[x66V3>0,250+25 (-1+x66V3),0]-If[x70V3>0,60+10 (-1+x70V3),0]-If[x71V3>0,250+25 (-1+x71V3),0]-If[x74V3>0,500+50 (-1+x74V3),0]-If[x76V3>0,100+25 (-1+x76V3),0]-If[x77V3>0,1000+100 (-1+x77V3),0]-If[x78V3>0,80+10 (-1+x78V3),0]-If[x81V3>0,100+25 (-1+x81V3),0]-If[x85V3>0,200+50 (-1+x85V3),0]-If[x8V3>0,50+5 (-1+x8V3),0]-If[x90V3>0,150+25 (-1+x90V3),0]-If[x94V3>0,250+25 (-1+x94V3),0]-If[x95V3>0,300+50 (-1+x95V3),0]-If[x96V3>0,300+50 (-1+x96V3),0]-If[x97V3>0,200+25 (-1+x97V3),0]-If[x98V3>0,60+10 (-1+x98V3),0]-If[x99V3>0,40+10 (-1+x99V3),0]>=0&&40180-If[x100V4>0,80+10 (-1+x100V4),0]-If[x11V4>0,300+50 (-1+x11V4),0]-If[x12V4>0,100+10 (-1+x12V4),0]-If[x13V4>0,600+100 (-1+x13V4),0]-If[x14V4>0,50+5 (-1+x14V4),0]-If[x17V4>0,200+50 (-1+x17V4),0]-If[x18V4>0,150+25 (-1+x18V4),0]-If[x22V4>0,800+100 (-1+x22V4),0]-If[x23V4>0,250+25 (-1+x23V4),0]-If[x24V4>0,500+50 (-1+x24V4),0]-If[x25V4>0,80+10 (-1+x25V4),0]-If[x27V4>0,50+5 (-1+x27V4),0]-If[x29V4>0,200+25 (-1+x29V4),0]-If[x30V4>0,100+25 (-1+x30V4),0]-If[x34V4>0,40+5 (-1+x34V4),0]-If[x35V4>0,100+10 (-1+x35V4),0]-If[x36V4>0,250+25 (-1+x36V4),0]-If[x39V4>0,200+25 (-1+x39V4),0]-If[x42V4>0,1000+100 (-1+x42V4),0]-If[x43V4>0,800+100 (-1+x43V4),0]-If[x44V4>0,40+10 (-1+x44V4),0]-If[x46V4>0,250+25 (-1+x46V4),0]-If[x48V4>0,30+5 (-1+x48V4),0]-If[x49V4>0,20+5 (-1+x49V4),0]-If[x4V4>0,40+5 (-1+x4V4),0]-If[x51V4>0,800+100 (-1+x51V4),0]-If[x54V4>0,500+50 (-1+x54V4),0]-If[x55V4>0,20+5 (-1+x55V4),0]-If[x57V4>0,1000+100 (-1+x57V4),0]-If[x58V4>0,100+25 (-1+x58V4),0]-If[x5V4>0,60+10 (-1+x5V4),0]-If[x60V4>0,1000+100 (-1+x60V4),0]-If[x63V4>0,200+50 (-1+x63V4),0]-If[x64V4>0,80+10 (-1+x64V4),0]-If[x65V4>0,100+25 (-1+x65V4),0]-If[x66V4>0,30+5 (-1+x66V4),0]-If[x67V4>0,60+10 (-1+x67V4),0]-If[x68V4>0,400+100 (-1+x68V4),0]-If[x69V4>0,100+10 (-1+x69V4),0]-If[x6V4>0,250+25 (-1+x6V4),0]-If[x70V4>0,60+10 (-1+x70V4),0]-If[x74V4>0,80+10 (-1+x74V4),0]-If[x75V4>0,30+5 (-1+x75V4),0]-If[x76V4>0,500+50 (-1+x76V4),0]-If[x77V4>0,400+100 (-1+x77V4),0]-If[x78V4>0,20+5 (-1+x78V4),0]-If[x7V4>0,150+25 (-1+x7V4),0]-If[x82V4>0,150+25 (-1+x82V4),0]-If[x84V4>0,300+50 (-1+x84V4),0]-If[x85V4>0,40+10 (-1+x85V4),0]-If[x86V4>0,100+10 (-1+x86V4),0]-If[x88V4>0,100+25 (-1+x88V4),0]-If[x90V4>0,100+10 (-1+x90V4),0]-If[x92V4>0,20+5 (-1+x92V4),0]-If[x93V4>0,30+5 (-1+x93V4),0]-If[x96V4>0,200+50 (-1+x96V4),0]-If[x99V4>0,40+5 (-1+x99V4),0]-If[x9V4>0,150+25 (-1+x9V4),0]>=0&&37400-If[x10V5>0,50+5 (-1+x10V5),0]-If[x12V5>0,40+10 (-1+x12V5),0]-If[x14V5>0,400+50 (-1+x14V5),0]-If[x15V5>0,250+25 (-1+x15V5),0]-If[x17V5>0,250+25 (-1+x17V5),0]-If[x18V5>0,400+100 (-1+x18V5),0]-If[x19V5>0,800+100 (-1+x19V5),0]-If[x1V5>0,200+50 (-1+x1V5),0]-If[x21V5>0,100+25 (-1+x21V5),0]-If[x23V5>0,300+50 (-1+x23V5),0]-If[x24V5>0,200+50 (-1+x24V5),0]-If[x27V5>0,250+25 (-1+x27V5),0]-If[x29V5>0,600+100 (-1+x29V5),0]-If[x2V5>0,60+10 (-1+x2V5),0]-If[x31V5>0,500+50 (-1+x31V5),0]-If[x41V5>0,100+10 (-1+x41V5),0]-If[x43V5>0,40+10 (-1+x43V5),0]-If[x46V5>0,200+50 (-1+x46V5),0]-If[x47V5>0,500+50 (-1+x47V5),0]-If[x49V5>0,300+50 (-1+x49V5),0]-If[x57V5>0,150+25 (-1+x57V5),0]-If[x58V5>0,400+50 (-1+x58V5),0]-If[x59V5>0,20+5 (-1+x59V5),0]-If[x61V5>0,800+100 (-1+x61V5),0]-If[x63V5>0,150+25 (-1+x63V5),0]-If[x66V5>0,150+25 (-1+x66V5),0]-If[x68V5>0,30+5 (-1+x68V5),0]-If[x69V5>0,250+25 (-1+x69V5),0]-If[x75V5>0,400+50 (-1+x75V5),0]-If[x78V5>0,40+10 (-1+x78V5),0]-If[x80V5>0,800+100 (-1+x80V5),0]-If[x84V5>0,200+50 (-1+x84V5),0]-If[x88V5>0,20+5 (-1+x88V5),0]-If[x90V5>0,150+25 (-1+x90V5),0]-If[x93V5>0,1000+100 (-1+x93V5),0]-If[x94V5>0,200+25 (-1+x94V5),0]-If[x95V5>0,400+100 (-1+x95V5),0]-If[x96V5>0,40+10 (-1+x96V5),0]-If[x98V5>0,40+10 (-1+x98V5),0]>=0&&43915-If[x100V6>0,40+5 (-1+x100V6),0]-If[x14V6>0,250+25 (-1+x14V6),0]-If[x17V6>0,300+50 (-1+x17V6),0]-If[x18V6>0,200+25 (-1+x18V6),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x20V6>0,250+25 (-1+x20V6),0]-If[x21V6>0,50+5 (-1+x21V6),0]-If[x22V6>0,20+5 (-1+x22V6),0]-If[x26V6>0,30+5 (-1+x26V6),0]-If[x28V6>0,40+10 (-1+x28V6),0]-If[x2V6>0,40+10 (-1+x2V6),0]-If[x31V6>0,500+50 (-1+x31V6),0]-If[x34V6>0,60+10 (-1+x34V6),0]-If[x36V6>0,50+5 (-1+x36V6),0]-If[x41V6>0,800+100 (-1+x41V6),0]-If[x42V6>0,800+100 (-1+x42V6),0]-If[x44V6>0,80+10 (-1+x44V6),0]-If[x46V6>0,600+100 (-1+x46V6),0]-If[x47V6>0,30+5 (-1+x47V6),0]-If[x48V6>0,100+10 (-1+x48V6),0]-If[x50V6>0,500+50 (-1+x50V6),0]-If[x51V6>0,200+50 (-1+x51V6),0]-If[x55V6>0,1000+100 (-1+x55V6),0]-If[x57V6>0,60+10 (-1+x57V6),0]-If[x58V6>0,20+5 (-1+x58V6),0]-If[x5V6>0,150+25 (-1+x5V6),0]-If[x63V6>0,400+50 (-1+x63V6),0]-If[x66V6>0,50+5 (-1+x66V6),0]-If[x70V6>0,150+25 (-1+x70V6),0]-If[x73V6>0,20+5 (-1+x73V6),0]-If[x74V6>0,20+5 (-1+x74V6),0]-If[x75V6>0,100+10 (-1+x75V6),0]-If[x77V6>0,400+100 (-1+x77V6),0]-If[x78V6>0,40+10 (-1+x78V6),0]-If[x79V6>0,800+100 (-1+x79V6),0]-If[x7V6>0,100+10 (-1+x7V6),0]-If[x82V6>0,400+50 (-1+x82V6),0]-If[x84V6>0,60+10 (-1+x84V6),0]-If[x85V6>0,800+100 (-1+x85V6),0]-If[x86V6>0,400+100 (-1+x86V6),0]-If[x87V6>0,30+5 (-1+x87V6),0]-If[x88V6>0,200+25 (-1+x88V6),0]-If[x89V6>0,400+100 (-1+x89V6),0]-If[x91V6>0,800+100 (-1+x91V6),0]-If[x96V6>0,50+5 (-1+x96V6),0]-If[x97V6>0,800+100 (-1+x97V6),0]-If[x98V6>0,30+5 (-1+x98V6),0]-If[x9V6>0,100+25 (-1+x9V6),0]>=0&&58895-If[x10V7>0,20+5 (-1+x10V7),0]-If[x13V7>0,60+10 (-1+x13V7),0]-If[x14V7>0,250+25 (-1+x14V7),0]-If[x18V7>0,200+50 (-1+x18V7),0]-If[x1V7>0,80+10 (-1+x1V7),0]-If[x21V7>0,300+50 (-1+x21V7),0]-If[x24V7>0,300+50 (-1+x24V7),0]-If[x25V7>0,50+5 (-1+x25V7),0]-If[x26V7>0,400+50 (-1+x26V7),0]-If[x28V7>0,50+5 (-1+x28V7),0]-If[x2V7>0,500+50 (-1+x2V7),0]-If[x31V7>0,150+25 (-1+x31V7),0]-If[x33V7>0,40+5 (-1+x33V7),0]-If[x34V7>0,80+10 (-1+x34V7),0]-If[x41V7>0,600+100 (-1+x41V7),0]-If[x42V7>0,80+10 (-1+x42V7),0]-If[x44V7>0,300+50 (-1+x44V7),0]-If[x48V7>0,200+25 (-1+x48V7),0]-If[x53V7>0,400+100 (-1+x53V7),0]-If[x55V7>0,500+50 (-1+x55V7),0]-If[x58V7>0,100+10 (-1+x58V7),0]-If[x59V7>0,100+25 (-1+x59V7),0]-If[x5V7>0,150+25 (-1+x5V7),0]-If[x60V7>0,20+5 (-1+x60V7),0]-If[x61V7>0,100+10 (-1+x61V7),0]-If[x62V7>0,200+25 (-1+x62V7),0]-If[x63V7>0,40+10 (-1+x63V7),0]-If[x68V7>0,500+50 (-1+x68V7),0]-If[x69V7>0,300+50 (-1+x69V7),0]-If[x70V7>0,50+5 (-1+x70V7),0]-If[x71V7>0,1000+100 (-1+x71V7),0]-If[x72V7>0,400+100 (-1+x72V7),0]-If[x75V7>0,400+50 (-1+x75V7),0]-If[x76V7>0,40+5 (-1+x76V7),0]-If[x79V7>0,150+25 (-1+x79V7),0]-If[x82V7>0,40+10 (-1+x82V7),0]-If[x84V7>0,60+10 (-1+x84V7),0]-If[x85V7>0,30+5 (-1+x85V7),0]-If[x86V7>0,100+10 (-1+x86V7),0]-If[x87V7>0,200+25 (-1+x87V7),0]-If[x88V7>0,200+25 (-1+x88V7),0]-If[x8V7>0,150+25 (-1+x8V7),0]-If[x90V7>0,500+50 (-1+x90V7),0]-If[x92V7>0,250+25 (-1+x92V7),0]-If[x93V7>0,40+10 (-1+x93V7),0]-If[x94V7>0,500+50 (-1+x94V7),0]-If[x96V7>0,250+25 (-1+x96V7),0]-If[x98V7>0,150+25 (-1+x98V7),0]>=0&&2335-If[x100V8>0,1000+100 (-1+x100V8),0]-If[x10V8>0,100+25 (-1+x10V8),0]-If[x12V8>0,80+10 (-1+x12V8),0]-If[x17V8>0,30+5 (-1+x17V8),0]-If[x19V8>0,50+5 (-1+x19V8),0]-If[x20V8>0,400+100 (-1+x20V8),0]-If[x28V8>0,400+50 (-1+x28V8),0]-If[x29V8>0,100+25 (-1+x29V8),0]-If[x32V8>0,600+100 (-1+x32V8),0]-If[x34V8>0,30+5 (-1+x34V8),0]-If[x35V8>0,200+25 (-1+x35V8),0]-If[x38V8>0,40+5 (-1+x38V8),0]-If[x39V8>0,200+25 (-1+x39V8),0]-If[x41V8>0,60+10 (-1+x41V8),0]-If[x43V8>0,200+50 (-1+x43V8),0]-If[x45V8>0,40+10 (-1+x45V8),0]-If[x47V8>0,200+50 (-1+x47V8),0]-If[x48V8>0,500+50 (-1+x48V8),0]-If[x4V8>0,20+5 (-1+x4V8),0]-If[x50V8>0,500+50 (-1+x50V8),0]-If[x51V8>0,300+50 (-1+x51V8),0]-If[x52V8>0,300+50 (-1+x52V8),0]-If[x53V8>0,60+10 (-1+x53V8),0]-If[x55V8>0,100+10 (-1+x55V8),0]-If[x56V8>0,1000+100 (-1+x56V8),0]-If[x57V8>0,30+5 (-1+x57V8),0]-If[x58V8>0,60+10 (-1+x58V8),0]-If[x5V8>0,600+100 (-1+x5V8),0]-If[x63V8>0,20+5 (-1+x63V8),0]-If[x64V8>0,50+5 (-1+x64V8),0]-If[x66V8>0,100+10 (-1+x66V8),0]-If[x67V8>0,400+100 (-1+x67V8),0]-If[x68V8>0,50+5 (-1+x68V8),0]-If[x69V8>0,150+25 (-1+x69V8),0]-If[x6V8>0,50+5 (-1+x6V8),0]-If[x71V8>0,40+10 (-1+x71V8),0]-If[x73V8>0,80+10 (-1+x73V8),0]-If[x76V8>0,80+10 (-1+x76V8),0]-If[x81V8>0,150+25 (-1+x81V8),0]-If[x84V8>0,60+10 (-1+x84V8),0]-If[x85V8>0,100+25 (-1+x85V8),0]-If[x86V8>0,40+10 (-1+x86V8),0]-If[x87V8>0,100+10 (-1+x87V8),0]-If[x88V8>0,400+100 (-1+x88V8),0]-If[x89V8>0,400+50 (-1+x89V8),0]-If[x8V8>0,20+5 (-1+x8V8),0]-If[x90V8>0,250+25 (-1+x90V8),0]-If[x91V8>0,100+10 (-1+x91V8),0]-If[x93V8>0,20+5 (-1+x93V8),0]-If[x94V8>0,600+100 (-1+x94V8),0]-If[x95V8>0,300+50 (-1+x95V8),0]-If[x9V8>0,600+100 (-1+x9V8),0]>=0&&68369-If[x100V9>0,20+5 (-1+x100V9),0]-If[x11V9>0,100+25 (-1+x11V9),0]-If[x12V9>0,800+100 (-1+x12V9),0]-If[x17V9>0,150+25 (-1+x17V9),0]-If[x19V9>0,1000+100 (-1+x19V9),0]-If[x24V9>0,200+50 (-1+x24V9),0]-If[x25V9>0,600+100 (-1+x25V9),0]-If[x26V9>0,20+5 (-1+x26V9),0]-If[x28V9>0,50+5 (-1+x28V9),0]-If[x29V9>0,600+100 (-1+x29V9),0]-If[x2V9>0,30+5 (-1+x2V9),0]-If[x32V9>0,400+50 (-1+x32V9),0]-If[x36V9>0,40+5 (-1+x36V9),0]-If[x37V9>0,60+10 (-1+x37V9),0]-If[x38V9>0,200+25 (-1+x38V9),0]-If[x39V9>0,80+10 (-1+x39V9),0]-If[x40V9>0,1000+100 (-1+x40V9),0]-If[x41V9>0,40+10 (-1+x41V9),0]-If[x42V9>0,200+25 (-1+x42V9),0]-If[x49V9>0,100+25 (-1+x49V9),0]-If[x4V9>0,30+5 (-1+x4V9),0]-If[x50V9>0,80+10 (-1+x50V9),0]-If[x51V9>0,400+100 (-1+x51V9),0]-If[x53V9>0,150+25 (-1+x53V9),0]-If[x55V9>0,80+10 (-1+x55V9),0]-If[x57V9>0,800+100 (-1+x57V9),0]-If[x58V9>0,100+10 (-1+x58V9),0]-If[x5V9>0,80+10 (-1+x5V9),0]-If[x60V9>0,40+10 (-1+x60V9),0]-If[x61V9>0,250+25 (-1+x61V9),0]-If[x63V9>0,30+5 (-1+x63V9),0]-If[x65V9>0,500+50 (-1+x65V9),0]-If[x66V9>0,500+50 (-1+x66V9),0]-If[x68V9>0,150+25 (-1+x68V9),0]-If[x69V9>0,40+5 (-1+x69V9),0]-If[x71V9>0,40+5 (-1+x71V9),0]-If[x73V9>0,300+50 (-1+x73V9),0]-If[x75V9>0,20+5 (-1+x75V9),0]-If[x76V9>0,1000+100 (-1+x76V9),0]-If[x80V9>0,100+10 (-1+x80V9),0]-If[x81V9>0,150+25 (-1+x81V9),0]-If[x82V9>0,250+25 (-1+x82V9),0]-If[x83V9>0,300+50 (-1+x83V9),0]-If[x85V9>0,400+100 (-1+x85V9),0]-If[x86V9>0,30+5 (-1+x86V9),0]-If[x87V9>0,200+25 (-1+x87V9),0]-If[x88V9>0,100+25 (-1+x88V9),0]-If[x90V9>0,400+100 (-1+x90V9),0]-If[x94V9>0,400+50 (-1+x94V9),0]-If[x95V9>0,200+25 (-1+x95V9),0]-If[x96V9>0,20+5 (-1+x96V9),0]-If[x97V9>0,100+25 (-1+x97V9),0]-If[x98V9>0,50+5 (-1+x98V9),0]-If[x99V9>0,600+100 (-1+x99V9),0]-If[x9V9>0,200+50 (-1+x9V9),0]>=0&&14162-If[x100V10>0,1000+100 (-1+x100V10),0]-If[x10V10>0,30+5 (-1+x10V10),0]-If[x11V10>0,250+25 (-1+x11V10),0]-If[x12V10>0,20+5 (-1+x12V10),0]-If[x13V10>0,600+100 (-1+x13V10),0]-If[x14V10>0,50+5 (-1+x14V10),0]-If[x17V10>0,400+100 (-1+x17V10),0]-If[x18V10>0,40+5 (-1+x18V10),0]-If[x19V10>0,100+25 (-1+x19V10),0]-If[x20V10>0,20+5 (-1+x20V10),0]-If[x22V10>0,50+5 (-1+x22V10),0]-If[x24V10>0,500+50 (-1+x24V10),0]-If[x2V10>0,400+50 (-1+x2V10),0]-If[x31V10>0,600+100 (-1+x31V10),0]-If[x32V10>0,20+5 (-1+x32V10),0]-If[x33V10>0,40+5 (-1+x33V10),0]-If[x34V10>0,40+5 (-1+x34V10),0]-If[x41V10>0,20+5 (-1+x41V10),0]-If[x42V10>0,100+25 (-1+x42V10),0]-If[x43V10>0,300+50 (-1+x43V10),0]-If[x44V10>0,500+50 (-1+x44V10),0]-If[x45V10>0,400+50 (-1+x45V10),0]-If[x47V10>0,200+50 (-1+x47V10),0]-If[x4V10>0,50+5 (-1+x4V10),0]-If[x53V10>0,40+10 (-1+x53V10),0]-If[x54V10>0,80+10 (-1+x54V10),0]-If[x55V10>0,400+50 (-1+x55V10),0]-If[x57V10>0,800+100 (-1+x57V10),0]-If[x58V10>0,1000+100 (-1+x58V10),0]-If[x60V10>0,40+5 (-1+x60V10),0]-If[x65V10>0,800+100 (-1+x65V10),0]-If[x6V10>0,300+50 (-1+x6V10),0]-If[x70V10>0,50+5 (-1+x70V10),0]-If[x75V10>0,20+5 (-1+x75V10),0]-If[x76V10>0,250+25 (-1+x76V10),0]-If[x77V10>0,30+5 (-1+x77V10),0]-If[x7V10>0,40+10 (-1+x7V10),0]-If[x82V10>0,80+10 (-1+x82V10),0]-If[x84V10>0,400+50 (-1+x84V10),0]-If[x86V10>0,100+25 (-1+x86V10),0]-If[x88V10>0,600+100 (-1+x88V10),0]-If[x8V10>0,50+5 (-1+x8V10),0]-If[x92V10>0,200+25 (-1+x92V10),0]-If[x94V10>0,30+5 (-1+x94V10),0]-If[x95V10>0,30+5 (-1+x95V10),0]-If[x96V10>0,200+25 (-1+x96V10),0]-If[x98V10>0,250+25 (-1+x98V10),0]-If[x9V10>0,600+100 (-1+x9V10),0]>=0
generateContractCost
generateContractCost[1] = (-193+If[x1V5>0,200+50 (-1+x1V5),0]+If[x1V6>0,20+5 (-1+x1V6),0]+If[x1V7>0,80+10 (-1+x1V7),0]) If[193-If[x1V5>0,200+50 (-1+x1V5),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x1V7>0,80+10 (-1+x1V7),0]>0,148,0]
generatePositionChange
{0,0,0,0,-If[x1V5>0,200+50 (-1+x1V5),0],-If[x1V6>0,20+5 (-1+x1V6),0],-If[x1V7>0,80+10 (-1+x1V7),0],0,0,0}
generateMaxFunc
6 (37400-If[x10V5>0,50+5 (-1+x10V5),0]-If[x12V5>0,40+10 (-1+x12V5),0]-If[x14V5>0,400+50 (-1+x14V5),0]-If[x15V5>0,250+25 (-1+x15V5),0]-If[x17V5>0,250+25 (-1+x17V5),0]-If[x18V5>0,400+100 (-1+x18V5),0]-If[x19V5>0,800+100 (-1+x19V5),0]-If[x1V5>0,200+50 (-1+x1V5),0]-If[x21V5>0,100+25 (-1+x21V5),0]-If[x23V5>0,300+50 (-1+x23V5),0]-If[x24V5>0,200+50 (-1+x24V5),0]-If[x27V5>0,250+25 (-1+x27V5),0]-If[x29V5>0,600+100 (-1+x29V5),0]-If[x2V5>0,60+10 (-1+x2V5),0]-If[x31V5>0,500+50 (-1+x31V5),0]-If[x41V5>0,100+10 (-1+x41V5),0]-If[x43V5>0,40+10 (-1+x43V5),0]-If[x46V5>0,200+50 (-1+x46V5),0]-If[x47V5>0,500+50 (-1+x47V5),0]-If[x49V5>0,300+50 (-1+x49V5),0]-If[x57V5>0,150+25 (-1+x57V5),0]-If[x58V5>0,400+50 (-1+x58V5),0]-If[x59V5>0,20+5 (-1+x59V5),0]-If[x61V5>0,800+100 (-1+x61V5),0]-If[x63V5>0,150+25 (-1+x63V5),0]-If[x66V5>0,150+25 (-1+x66V5),0]-If[x68V5>0,30+5 (-1+x68V5),0]-If[x69V5>0,250+25 (-1+x69V5),0]-If[x75V5>0,400+50 (-1+x75V5),0]-If[x78V5>0,40+10 (-1+x78V5),0]-If[x80V5>0,800+100 (-1+x80V5),0]-If[x84V5>0,200+50 (-1+x84V5),0]-If[x88V5>0,20+5 (-1+x88V5),0]-If[x90V5>0,150+25 (-1+x90V5),0]-If[x93V5>0,1000+100 (-1+x93V5),0]-If[x94V5>0,200+25 (-1+x94V5),0]-If[x95V5>0,400+100 (-1+x95V5),0]-If[x96V5>0,40+10 (-1+x96V5),0]-If[x98V5>0,40+10 (-1+x98V5),0])+81 (58895-If[x10V7>0,20+5 (-1+x10V7),0]-If[x13V7>0,60+10 (-1+x13V7),0]-If[x14V7>0,250+25 (-1+x14V7),0]-If[x18V7>0,200+50 (-1+x18V7),0]-If[x1V7>0,80+10 (-1+x1V7),0]-If[x21V7>0,300+50 (-1+x21V7),0]-If[x24V7>0,300+50 (-1+x24V7),0]-If[x25V7>0,50+5 (-1+x25V7),0]-If[x26V7>0,400+50 (-1+x26V7),0]-If[x28V7>0,50+5 (-1+x28V7),0]-If[x2V7>0,500+50 (-1+x2V7),0]-If[x31V7>0,150+25 (-1+x31V7),0]-If[x33V7>0,40+5 (-1+x33V7),0]-If[x34V7>0,80+10 (-1+x34V7),0]-If[x41V7>0,600+100 (-1+x41V7),0]-If[x42V7>0,80+10 (-1+x42V7),0]-If[x44V7>0,300+50 (-1+x44V7),0]-If[x48V7>0,200+25 (-1+x48V7),0]-If[x53V7>0,400+100 (-1+x53V7),0]-If[x55V7>0,500+50 (-1+x55V7),0]-If[x58V7>0,100+10 (-1+x58V7),0]-If[x59V7>0,100+25 (-1+x59V7),0]-If[x5V7>0,150+25 (-1+x5V7),0]-If[x60V7>0,20+5 (-1+x60V7),0]-If[x61V7>0,100+10 (-1+x61V7),0]-If[x62V7>0,200+25 (-1+x62V7),0]-If[x63V7>0,40+10 (-1+x63V7),0]-If[x68V7>0,500+50 (-1+x68V7),0]-If[x69V7>0,300+50 (-1+x69V7),0]-If[x70V7>0,50+5 (-1+x70V7),0]-If[x71V7>0,1000+100 (-1+x71V7),0]-If[x72V7>0,400+100 (-1+x72V7),0]-If[x75V7>0,400+50 (-1+x75V7),0]-If[x76V7>0,40+5 (-1+x76V7),0]-If[x79V7>0,150+25 (-1+x79V7),0]-If[x82V7>0,40+10 (-1+x82V7),0]-If[x84V7>0,60+10 (-1+x84V7),0]-If[x85V7>0,30+5 (-1+x85V7),0]-If[x86V7>0,100+10 (-1+x86V7),0]-If[x87V7>0,200+25 (-1+x87V7),0]-If[x88V7>0,200+25 (-1+x88V7),0]-If[x8V7>0,150+25 (-1+x8V7),0]-If[x90V7>0,500+50 (-1+x90V7),0]-If[x92V7>0,250+25 (-1+x92V7),0]-If[x93V7>0,40+10 (-1+x93V7),0]-If[x94V7>0,500+50 (-1+x94V7),0]-If[x96V7>0,250+25 (-1+x96V7),0]-If[x98V7>0,150+25 (-1+x98V7),0])+93 (50497-If[x100V3>0,600+100 (-1+x100V3),0]-If[x11V3>0,300+50 (-1+x11V3),0]-If[x12V3>0,40+10 (-1+x12V3),0]-If[x13V3>0,200+50 (-1+x13V3),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x16V3>0,200+25 (-1+x16V3),0]-If[x17V3>0,30+5 (-1+x17V3),0]-If[x18V3>0,60+10 (-1+x18V3),0]-If[x19V3>0,600+100 (-1+x19V3),0]-If[x20V3>0,100+25 (-1+x20V3),0]-If[x21V3>0,600+100 (-1+x21V3),0]-If[x22V3>0,150+25 (-1+x22V3),0]-If[x24V3>0,150+25 (-1+x24V3),0]-If[x29V3>0,500+50 (-1+x29V3),0]-If[x31V3>0,40+5 (-1+x31V3),0]-If[x32V3>0,1000+100 (-1+x32V3),0]-If[x35V3>0,200+50 (-1+x35V3),0]-If[x36V3>0,200+50 (-1+x36V3),0]-If[x37V3>0,30+5 (-1+x37V3),0]-If[x3V3>0,400+100 (-1+x3V3),0]-If[x41V3>0,300+50 (-1+x41V3),0]-If[x42V3>0,500+50 (-1+x42V3),0]-If[x43V3>0,150+25 (-1+x43V3),0]-If[x45V3>0,100+10 (-1+x45V3),0]-If[x46V3>0,60+10 (-1+x46V3),0]-If[x47V3>0,50+5 (-1+x47V3),0]-If[x48V3>0,200+50 (-1+x48V3),0]-If[x49V3>0,800+100 (-1+x49V3),0]-If[x50V3>0,20+5 (-1+x50V3),0]-If[x52V3>0,50+5 (-1+x52V3),0]-If[x56V3>0,250+25 (-1+x56V3),0]-If[x58V3>0,500+50 (-1+x58V3),0]-If[x63V3>0,100+25 (-1+x63V3),0]-If[x65V3>0,300+50 (-1+x65V3),0]-If[x66V3>0,250+25 (-1+x66V3),0]-If[x70V3>0,60+10 (-1+x70V3),0]-If[x71V3>0,250+25 (-1+x71V3),0]-If[x74V3>0,500+50 (-1+x74V3),0]-If[x76V3>0,100+25 (-1+x76V3),0]-If[x77V3>0,1000+100 (-1+x77V3),0]-If[x78V3>0,80+10 (-1+x78V3),0]-If[x81V3>0,100+25 (-1+x81V3),0]-If[x85V3>0,200+50 (-1+x85V3),0]-If[x8V3>0,50+5 (-1+x8V3),0]-If[x90V3>0,150+25 (-1+x90V3),0]-If[x94V3>0,250+25 (-1+x94V3),0]-If[x95V3>0,300+50 (-1+x95V3),0]-If[x96V3>0,300+50 (-1+x96V3),0]-If[x97V3>0,200+25 (-1+x97V3),0]-If[x98V3>0,60+10 (-1+x98V3),0]-If[x99V3>0,40+10 (-1+x99V3),0])+54 (7237-If[x100V1>0,30+5 (-1+x100V1),0]-If[x11V1>0,100+10 (-1+x11V1),0]-If[x12V1>0,600+100 (-1+x12V1),0]-If[x13V1>0,100+10 (-1+x13V1),0]-If[x14V1>0,40+5 (-1+x14V1),0]-If[x15V1>0,50+5 (-1+x15V1),0]-If[x16V1>0,150+25 (-1+x16V1),0]-If[x18V1>0,400+100 (-1+x18V1),0]-If[x20V1>0,600+100 (-1+x20V1),0]-If[x22V1>0,50+5 (-1+x22V1),0]-If[x24V1>0,500+50 (-1+x24V1),0]-If[x26V1>0,50+5 (-1+x26V1),0]-If[x28V1>0,500+50 (-1+x28V1),0]-If[x29V1>0,200+50 (-1+x29V1),0]-If[x30V1>0,1000+100 (-1+x30V1),0]-If[x31V1>0,1000+100 (-1+x31V1),0]-If[x32V1>0,400+100 (-1+x32V1),0]-If[x36V1>0,500+50 (-1+x36V1),0]-If[x3V1>0,20+5 (-1+x3V1),0]-If[x40V1>0,30+5 (-1+x40V1),0]-If[x41V1>0,200+25 (-1+x41V1),0]-If[x42V1>0,1000+100 (-1+x42V1),0]-If[x43V1>0,100+25 (-1+x43V1),0]-If[x45V1>0,600+100 (-1+x45V1),0]-If[x46V1>0,250+25 (-1+x46V1),0]-If[x47V1>0,1000+100 (-1+x47V1),0]-If[x4V1>0,500+50 (-1+x4V1),0]-If[x50V1>0,150+25 (-1+x50V1),0]-If[x51V1>0,100+10 (-1+x51V1),0]-If[x53V1>0,500+50 (-1+x53V1),0]-If[x55V1>0,40+10 (-1+x55V1),0]-If[x57V1>0,500+50 (-1+x57V1),0]-If[x58V1>0,250+25 (-1+x58V1),0]-If[x5V1>0,600+100 (-1+x5V1),0]-If[x65V1>0,400+50 (-1+x65V1),0]-If[x68V1>0,150+25 (-1+x68V1),0]-If[x69V1>0,50+5 (-1+x69V1),0]-If[x6V1>0,50+5 (-1+x6V1),0]-If[x71V1>0,30+5 (-1+x71V1),0]-If[x72V1>0,800+100 (-1+x72V1),0]-If[x73V1>0,400+50 (-1+x73V1),0]-If[x75V1>0,500+50 (-1+x75V1),0]-If[x78V1>0,20+5 (-1+x78V1),0]-If[x80V1>0,60+10 (-1+x80V1),0]-If[x81V1>0,20+5 (-1+x81V1),0]-If[x82V1>0,200+50 (-1+x82V1),0]-If[x83V1>0,200+50 (-1+x83V1),0]-If[x85V1>0,200+50 (-1+x85V1),0]-If[x86V1>0,50+5 (-1+x86V1),0]-If[x87V1>0,300+50 (-1+x87V1),0]-If[x88V1>0,100+10 (-1+x88V1),0]-If[x90V1>0,60+10 (-1+x90V1),0]-If[x93V1>0,200+25 (-1+x93V1),0]-If[x95V1>0,200+25 (-1+x95V1),0]-If[x97V1>0,30+5 (-1+x97V1),0]-If[x98V1>0,400+100 (-1+x98V1),0]-If[x9V1>0,400+100 (-1+x9V1),0])+34 (14162-If[x100V10>0,1000+100 (-1+x100V10),0]-If[x10V10>0,30+5 (-1+x10V10),0]-If[x11V10>0,250+25 (-1+x11V10),0]-If[x12V10>0,20+5 (-1+x12V10),0]-If[x13V10>0,600+100 (-1+x13V10),0]-If[x14V10>0,50+5 (-1+x14V10),0]-If[x17V10>0,400+100 (-1+x17V10),0]-If[x18V10>0,40+5 (-1+x18V10),0]-If[x19V10>0,100+25 (-1+x19V10),0]-If[x20V10>0,20+5 (-1+x20V10),0]-If[x22V10>0,50+5 (-1+x22V10),0]-If[x24V10>0,500+50 (-1+x24V10),0]-If[x2V10>0,400+50 (-1+x2V10),0]-If[x31V10>0,600+100 (-1+x31V10),0]-If[x32V10>0,20+5 (-1+x32V10),0]-If[x33V10>0,40+5 (-1+x33V10),0]-If[x34V10>0,40+5 (-1+x34V10),0]-If[x41V10>0,20+5 (-1+x41V10),0]-If[x42V10>0,100+25 (-1+x42V10),0]-If[x43V10>0,300+50 (-1+x43V10),0]-If[x44V10>0,500+50 (-1+x44V10),0]-If[x45V10>0,400+50 (-1+x45V10),0]-If[x47V10>0,200+50 (-1+x47V10),0]-If[x4V10>0,50+5 (-1+x4V10),0]-If[x53V10>0,40+10 (-1+x53V10),0]-If[x54V10>0,80+10 (-1+x54V10),0]-If[x55V10>0,400+50 (-1+x55V10),0]-If[x57V10>0,800+100 (-1+x57V10),0]-If[x58V10>0,1000+100 (-1+x58V10),0]-If[x60V10>0,40+5 (-1+x60V10),0]-If[x65V10>0,800+100 (-1+x65V10),0]-If[x6V10>0,300+50 (-1+x6V10),0]-If[x70V10>0,50+5 (-1+x70V10),0]-If[x75V10>0,20+5 (-1+x75V10),0]-If[x76V10>0,250+25 (-1+x76V10),0]-If[x77V10>0,30+5 (-1+x77V10),0]-If[x7V10>0,40+10 (-1+x7V10),0]-If[x82V10>0,80+10 (-1+x82V10),0]-If[x84V10>0,400+50 (-1+x84V10),0]-If[x86V10>0,100+25 (-1+x86V10),0]-If[x88V10>0,600+100 (-1+x88V10),0]-If[x8V10>0,50+5 (-1+x8V10),0]-If[x92V10>0,200+25 (-1+x92V10),0]-If[x94V10>0,30+5 (-1+x94V10),0]-If[x95V10>0,30+5 (-1+x95V10),0]-If[x96V10>0,200+25 (-1+x96V10),0]-If[x98V10>0,250+25 (-1+x98V10),0]-If[x9V10>0,600+100 (-1+x9V10),0])+40 (6700-If[x11V2>0,20+5 (-1+x11V2),0]-If[x14V2>0,100+10 (-1+x14V2),0]-If[x15V2>0,150+25 (-1+x15V2),0]-If[x17V2>0,400+100 (-1+x17V2),0]-If[x18V2>0,20+5 (-1+x18V2),0]-If[x20V2>0,30+5 (-1+x20V2),0]-If[x25V2>0,1000+100 (-1+x25V2),0]-If[x27V2>0,200+25 (-1+x27V2),0]-If[x29V2>0,150+25 (-1+x29V2),0]-If[x2V2>0,1000+100 (-1+x2V2),0]-If[x36V2>0,400+50 (-1+x36V2),0]-If[x37V2>0,20+5 (-1+x37V2),0]-If[x39V2>0,100+25 (-1+x39V2),0]-If[x44V2>0,400+50 (-1+x44V2),0]-If[x46V2>0,500+50 (-1+x46V2),0]-If[x48V2>0,300+50 (-1+x48V2),0]-If[x49V2>0,40+10 (-1+x49V2),0]-If[x4V2>0,400+50 (-1+x4V2),0]-If[x53V2>0,600+100 (-1+x53V2),0]-If[x55V2>0,40+10 (-1+x55V2),0]-If[x56V2>0,20+5 (-1+x56V2),0]-If[x58V2>0,800+100 (-1+x58V2),0]-If[x5V2>0,800+100 (-1+x5V2),0]-If[x60V2>0,250+25 (-1+x60V2),0]-If[x61V2>0,250+25 (-1+x61V2),0]-If[x62V2>0,300+50 (-1+x62V2),0]-If[x64V2>0,200+25 (-1+x64V2),0]-If[x66V2>0,600+100 (-1+x66V2),0]-If[x69V2>0,60+10 (-1+x69V2),0]-If[x70V2>0,40+5 (-1+x70V2),0]-If[x73V2>0,50+5 (-1+x73V2),0]-If[x74V2>0,40+5 (-1+x74V2),0]-If[x76V2>0,600+100 (-1+x76V2),0]-If[x78V2>0,80+10 (-1+x78V2),0]-If[x79V2>0,100+10 (-1+x79V2),0]-If[x7V2>0,100+10 (-1+x7V2),0]-If[x82V2>0,400+100 (-1+x82V2),0]-If[x83V2>0,50+5 (-1+x83V2),0]-If[x84V2>0,60+10 (-1+x84V2),0]-If[x86V2>0,300+50 (-1+x86V2),0]-If[x88V2>0,250+25 (-1+x88V2),0]-If[x89V2>0,800+100 (-1+x89V2),0]-If[x92V2>0,800+100 (-1+x92V2),0]-If[x93V2>0,300+50 (-1+x93V2),0]-If[x94V2>0,60+10 (-1+x94V2),0]-If[x97V2>0,50+5 (-1+x97V2),0]-If[x98V2>0,1000+100 (-1+x98V2),0]-If[x9V2>0,1000+100 (-1+x9V2),0])+50 (40180-If[x100V4>0,80+10 (-1+x100V4),0]-If[x11V4>0,300+50 (-1+x11V4),0]-If[x12V4>0,100+10 (-1+x12V4),0]-If[x13V4>0,600+100 (-1+x13V4),0]-If[x14V4>0,50+5 (-1+x14V4),0]-If[x17V4>0,200+50 (-1+x17V4),0]-If[x18V4>0,150+25 (-1+x18V4),0]-If[x22V4>0,800+100 (-1+x22V4),0]-If[x23V4>0,250+25 (-1+x23V4),0]-If[x24V4>0,500+50 (-1+x24V4),0]-If[x25V4>0,80+10 (-1+x25V4),0]-If[x27V4>0,50+5 (-1+x27V4),0]-If[x29V4>0,200+25 (-1+x29V4),0]-If[x30V4>0,100+25 (-1+x30V4),0]-If[x34V4>0,40+5 (-1+x34V4),0]-If[x35V4>0,100+10 (-1+x35V4),0]-If[x36V4>0,250+25 (-1+x36V4),0]-If[x39V4>0,200+25 (-1+x39V4),0]-If[x42V4>0,1000+100 (-1+x42V4),0]-If[x43V4>0,800+100 (-1+x43V4),0]-If[x44V4>0,40+10 (-1+x44V4),0]-If[x46V4>0,250+25 (-1+x46V4),0]-If[x48V4>0,30+5 (-1+x48V4),0]-If[x49V4>0,20+5 (-1+x49V4),0]-If[x4V4>0,40+5 (-1+x4V4),0]-If[x51V4>0,800+100 (-1+x51V4),0]-If[x54V4>0,500+50 (-1+x54V4),0]-If[x55V4>0,20+5 (-1+x55V4),0]-If[x57V4>0,1000+100 (-1+x57V4),0]-If[x58V4>0,100+25 (-1+x58V4),0]-If[x5V4>0,60+10 (-1+x5V4),0]-If[x60V4>0,1000+100 (-1+x60V4),0]-If[x63V4>0,200+50 (-1+x63V4),0]-If[x64V4>0,80+10 (-1+x64V4),0]-If[x65V4>0,100+25 (-1+x65V4),0]-If[x66V4>0,30+5 (-1+x66V4),0]-If[x67V4>0,60+10 (-1+x67V4),0]-If[x68V4>0,400+100 (-1+x68V4),0]-If[x69V4>0,100+10 (-1+x69V4),0]-If[x6V4>0,250+25 (-1+x6V4),0]-If[x70V4>0,60+10 (-1+x70V4),0]-If[x74V4>0,80+10 (-1+x74V4),0]-If[x75V4>0,30+5 (-1+x75V4),0]-If[x76V4>0,500+50 (-1+x76V4),0]-If[x77V4>0,400+100 (-1+x77V4),0]-If[x78V4>0,20+5 (-1+x78V4),0]-If[x7V4>0,150+25 (-1+x7V4),0]-If[x82V4>0,150+25 (-1+x82V4),0]-If[x84V4>0,300+50 (-1+x84V4),0]-If[x85V4>0,40+10 (-1+x85V4),0]-If[x86V4>0,100+10 (-1+x86V4),0]-If[x88V4>0,100+25 (-1+x88V4),0]-If[x90V4>0,100+10 (-1+x90V4),0]-If[x92V4>0,20+5 (-1+x92V4),0]-If[x93V4>0,30+5 (-1+x93V4),0]-If[x96V4>0,200+50 (-1+x96V4),0]-If[x99V4>0,40+5 (-1+x99V4),0]-If[x9V4>0,150+25 (-1+x9V4),0])+8 (43915-If[x100V6>0,40+5 (-1+x100V6),0]-If[x14V6>0,250+25 (-1+x14V6),0]-If[x17V6>0,300+50 (-1+x17V6),0]-If[x18V6>0,200+25 (-1+x18V6),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x20V6>0,250+25 (-1+x20V6),0]-If[x21V6>0,50+5 (-1+x21V6),0]-If[x22V6>0,20+5 (-1+x22V6),0]-If[x26V6>0,30+5 (-1+x26V6),0]-If[x28V6>0,40+10 (-1+x28V6),0]-If[x2V6>0,40+10 (-1+x2V6),0]-If[x31V6>0,500+50 (-1+x31V6),0]-If[x34V6>0,60+10 (-1+x34V6),0]-If[x36V6>0,50+5 (-1+x36V6),0]-If[x41V6>0,800+100 (-1+x41V6),0]-If[x42V6>0,800+100 (-1+x42V6),0]-If[x44V6>0,80+10 (-1+x44V6),0]-If[x46V6>0,600+100 (-1+x46V6),0]-If[x47V6>0,30+5 (-1+x47V6),0]-If[x48V6>0,100+10 (-1+x48V6),0]-If[x50V6>0,500+50 (-1+x50V6),0]-If[x51V6>0,200+50 (-1+x51V6),0]-If[x55V6>0,1000+100 (-1+x55V6),0]-If[x57V6>0,60+10 (-1+x57V6),0]-If[x58V6>0,20+5 (-1+x58V6),0]-If[x5V6>0,150+25 (-1+x5V6),0]-If[x63V6>0,400+50 (-1+x63V6),0]-If[x66V6>0,50+5 (-1+x66V6),0]-If[x70V6>0,150+25 (-1+x70V6),0]-If[x73V6>0,20+5 (-1+x73V6),0]-If[x74V6>0,20+5 (-1+x74V6),0]-If[x75V6>0,100+10 (-1+x75V6),0]-If[x77V6>0,400+100 (-1+x77V6),0]-If[x78V6>0,40+10 (-1+x78V6),0]-If[x79V6>0,800+100 (-1+x79V6),0]-If[x7V6>0,100+10 (-1+x7V6),0]-If[x82V6>0,400+50 (-1+x82V6),0]-If[x84V6>0,60+10 (-1+x84V6),0]-If[x85V6>0,800+100 (-1+x85V6),0]-If[x86V6>0,400+100 (-1+x86V6),0]-If[x87V6>0,30+5 (-1+x87V6),0]-If[x88V6>0,200+25 (-1+x88V6),0]-If[x89V6>0,400+100 (-1+x89V6),0]-If[x91V6>0,800+100 (-1+x91V6),0]-If[x96V6>0,50+5 (-1+x96V6),0]-If[x97V6>0,800+100 (-1+x97V6),0]-If[x98V6>0,30+5 (-1+x98V6),0]-If[x9V6>0,100+25 (-1+x9V6),0])+38 (2335-If[x100V8>0,1000+100 (-1+x100V8),0]-If[x10V8>0,100+25 (-1+x10V8),0]-If[x12V8>0,80+10 (-1+x12V8),0]-If[x17V8>0,30+5 (-1+x17V8),0]-If[x19V8>0,50+5 (-1+x19V8),0]-If[x20V8>0,400+100 (-1+x20V8),0]-If[x28V8>0,400+50 (-1+x28V8),0]-If[x29V8>0,100+25 (-1+x29V8),0]-If[x32V8>0,600+100 (-1+x32V8),0]-If[x34V8>0,30+5 (-1+x34V8),0]-If[x35V8>0,200+25 (-1+x35V8),0]-If[x38V8>0,40+5 (-1+x38V8),0]-If[x39V8>0,200+25 (-1+x39V8),0]-If[x41V8>0,60+10 (-1+x41V8),0]-If[x43V8>0,200+50 (-1+x43V8),0]-If[x45V8>0,40+10 (-1+x45V8),0]-If[x47V8>0,200+50 (-1+x47V8),0]-If[x48V8>0,500+50 (-1+x48V8),0]-If[x4V8>0,20+5 (-1+x4V8),0]-If[x50V8>0,500+50 (-1+x50V8),0]-If[x51V8>0,300+50 (-1+x51V8),0]-If[x52V8>0,300+50 (-1+x52V8),0]-If[x53V8>0,60+10 (-1+x53V8),0]-If[x55V8>0,100+10 (-1+x55V8),0]-If[x56V8>0,1000+100 (-1+x56V8),0]-If[x57V8>0,30+5 (-1+x57V8),0]-If[x58V8>0,60+10 (-1+x58V8),0]-If[x5V8>0,600+100 (-1+x5V8),0]-If[x63V8>0,20+5 (-1+x63V8),0]-If[x64V8>0,50+5 (-1+x64V8),0]-If[x66V8>0,100+10 (-1+x66V8),0]-If[x67V8>0,400+100 (-1+x67V8),0]-If[x68V8>0,50+5 (-1+x68V8),0]-If[x69V8>0,150+25 (-1+x69V8),0]-If[x6V8>0,50+5 (-1+x6V8),0]-If[x71V8>0,40+10 (-1+x71V8),0]-If[x73V8>0,80+10 (-1+x73V8),0]-If[x76V8>0,80+10 (-1+x76V8),0]-If[x81V8>0,150+25 (-1+x81V8),0]-If[x84V8>0,60+10 (-1+x84V8),0]-If[x85V8>0,100+25 (-1+x85V8),0]-If[x86V8>0,40+10 (-1+x86V8),0]-If[x87V8>0,100+10 (-1+x87V8),0]-If[x88V8>0,400+100 (-1+x88V8),0]-If[x89V8>0,400+50 (-1+x89V8),0]-If[x8V8>0,20+5 (-1+x8V8),0]-If[x90V8>0,250+25 (-1+x90V8),0]-If[x91V8>0,100+10 (-1+x91V8),0]-If[x93V8>0,20+5 (-1+x93V8),0]-If[x94V8>0,600+100 (-1+x94V8),0]-If[x95V8>0,300+50 (-1+x95V8),0]-If[x9V8>0,600+100 (-1+x9V8),0])+80 (68369-If[x100V9>0,20+5 (-1+x100V9),0]-If[x11V9>0,100+25 (-1+x11V9),0]-If[x12V9>0,800+100 (-1+x12V9),0]-If[x17V9>0,150+25 (-1+x17V9),0]-If[x19V9>0,1000+100 (-1+x19V9),0]-If[x24V9>0,200+50 (-1+x24V9),0]-If[x25V9>0,600+100 (-1+x25V9),0]-If[x26V9>0,20+5 (-1+x26V9),0]-If[x28V9>0,50+5 (-1+x28V9),0]-If[x29V9>0,600+100 (-1+x29V9),0]-If[x2V9>0,30+5 (-1+x2V9),0]-If[x32V9>0,400+50 (-1+x32V9),0]-If[x36V9>0,40+5 (-1+x36V9),0]-If[x37V9>0,60+10 (-1+x37V9),0]-If[x38V9>0,200+25 (-1+x38V9),0]-If[x39V9>0,80+10 (-1+x39V9),0]-If[x40V9>0,1000+100 (-1+x40V9),0]-If[x41V9>0,40+10 (-1+x41V9),0]-If[x42V9>0,200+25 (-1+x42V9),0]-If[x49V9>0,100+25 (-1+x49V9),0]-If[x4V9>0,30+5 (-1+x4V9),0]-If[x50V9>0,80+10 (-1+x50V9),0]-If[x51V9>0,400+100 (-1+x51V9),0]-If[x53V9>0,150+25 (-1+x53V9),0]-If[x55V9>0,80+10 (-1+x55V9),0]-If[x57V9>0,800+100 (-1+x57V9),0]-If[x58V9>0,100+10 (-1+x58V9),0]-If[x5V9>0,80+10 (-1+x5V9),0]-If[x60V9>0,40+10 (-1+x60V9),0]-If[x61V9>0,250+25 (-1+x61V9),0]-If[x63V9>0,30+5 (-1+x63V9),0]-If[x65V9>0,500+50 (-1+x65V9),0]-If[x66V9>0,500+50 (-1+x66V9),0]-If[x68V9>0,150+25 (-1+x68V9),0]-If[x69V9>0,40+5 (-1+x69V9),0]-If[x71V9>0,40+5 (-1+x71V9),0]-If[x73V9>0,300+50 (-1+x73V9),0]-If[x75V9>0,20+5 (-1+x75V9),0]-If[x76V9>0,1000+100 (-1+x76V9),0]-If[x80V9>0,100+10 (-1+x80V9),0]-If[x81V9>0,150+25 (-1+x81V9),0]-If[x82V9>0,250+25 (-1+x82V9),0]-If[x83V9>0,300+50 (-1+x83V9),0]-If[x85V9>0,400+100 (-1+x85V9),0]-If[x86V9>0,30+5 (-1+x86V9),0]-If[x87V9>0,200+25 (-1+x87V9),0]-If[x88V9>0,100+25 (-1+x88V9),0]-If[x90V9>0,400+100 (-1+x90V9),0]-If[x94V9>0,400+50 (-1+x94V9),0]-If[x95V9>0,200+25 (-1+x95V9),0]-If[x96V9>0,20+5 (-1+x96V9),0]-If[x97V9>0,100+25 (-1+x97V9),0]-If[x98V9>0,50+5 (-1+x98V9),0]-If[x99V9>0,600+100 (-1+x99V9),0]-If[x9V9>0,200+50 (-1+x9V9),0])+(-613+If[x100V1>0,30+5 (-1+x100V1),0]+If[x100V10>0,1000+100 (-1+x100V10),0]+If[x100V3>0,600+100 (-1+x100V3),0]+If[x100V4>0,80+10 (-1+x100V4),0]+If[x100V6>0,40+5 (-1+x100V6),0]+If[x100V8>0,1000+100 (-1+x100V8),0]+If[x100V9>0,20+5 (-1+x100V9),0]) If[613-If[x100V1>0,30+5 (-1+x100V1),0]-If[x100V10>0,1000+100 (-1+x100V10),0]-If[x100V3>0,600+100 (-1+x100V3),0]-If[x100V4>0,80+10 (-1+x100V4),0]-If[x100V6>0,40+5 (-1+x100V6),0]-If[x100V8>0,1000+100 (-1+x100V8),0]-If[x100V9>0,20+5 (-1+x100V9),0]>0,164,0]+(-1641+If[x10V10>0,30+5 (-1+x10V10),0]+If[x10V5>0,50+5 (-1+x10V5),0]+If[x10V7>0,20+5 (-1+x10V7),0]+If[x10V8>0,100+25 (-1+x10V8),0]) If[1641-If[x10V10>0,30+5 (-1+x10V10),0]-If[x10V5>0,50+5 (-1+x10V5),0]-If[x10V7>0,20+5 (-1+x10V7),0]-If[x10V8>0,100+25 (-1+x10V8),0]>0,132,0]+(-1207+If[x11V1>0,100+10 (-1+x11V1),0]+If[x11V10>0,250+25 (-1+x11V10),0]+If[x11V2>0,20+5 (-1+x11V2),0]+If[x11V3>0,300+50 (-1+x11V3),0]+If[x11V4>0,300+50 (-1+x11V4),0]+If[x11V9>0,100+25 (-1+x11V9),0]) If[1207-If[x11V1>0,100+10 (-1+x11V1),0]-If[x11V10>0,250+25 (-1+x11V10),0]-If[x11V2>0,20+5 (-1+x11V2),0]-If[x11V3>0,300+50 (-1+x11V3),0]-If[x11V4>0,300+50 (-1+x11V4),0]-If[x11V9>0,100+25 (-1+x11V9),0]>0,120,0]+(-2373+If[x12V1>0,600+100 (-1+x12V1),0]+If[x12V10>0,20+5 (-1+x12V10),0]+If[x12V3>0,40+10 (-1+x12V3),0]+If[x12V4>0,100+10 (-1+x12V4),0]+If[x12V5>0,40+10 (-1+x12V5),0]+If[x12V8>0,80+10 (-1+x12V8),0]+If[x12V9>0,800+100 (-1+x12V9),0]) If[2373-If[x12V1>0,600+100 (-1+x12V1),0]-If[x12V10>0,20+5 (-1+x12V10),0]-If[x12V3>0,40+10 (-1+x12V3),0]-If[x12V4>0,100+10 (-1+x12V4),0]-If[x12V5>0,40+10 (-1+x12V5),0]-If[x12V8>0,80+10 (-1+x12V8),0]-If[x12V9>0,800+100 (-1+x12V9),0]>0,161,0]+(-991+If[x13V1>0,100+10 (-1+x13V1),0]+If[x13V10>0,600+100 (-1+x13V10),0]+If[x13V3>0,200+50 (-1+x13V3),0]+If[x13V4>0,600+100 (-1+x13V4),0]+If[x13V7>0,60+10 (-1+x13V7),0]) If[991-If[x13V1>0,100+10 (-1+x13V1),0]-If[x13V10>0,600+100 (-1+x13V10),0]-If[x13V3>0,200+50 (-1+x13V3),0]-If[x13V4>0,600+100 (-1+x13V4),0]-If[x13V7>0,60+10 (-1+x13V7),0]>0,175,0]+(-1042+If[x14V1>0,40+5 (-1+x14V1),0]+If[x14V10>0,50+5 (-1+x14V10),0]+If[x14V2>0,100+10 (-1+x14V2),0]+If[x14V3>0,30+5 (-1+x14V3),0]+If[x14V4>0,50+5 (-1+x14V4),0]+If[x14V5>0,400+50 (-1+x14V5),0]+If[x14V6>0,250+25 (-1+x14V6),0]+If[x14V7>0,250+25 (-1+x14V7),0]) If[1042-If[x14V1>0,40+5 (-1+x14V1),0]-If[x14V10>0,50+5 (-1+x14V10),0]-If[x14V2>0,100+10 (-1+x14V2),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x14V4>0,50+5 (-1+x14V4),0]-If[x14V5>0,400+50 (-1+x14V5),0]-If[x14V6>0,250+25 (-1+x14V6),0]-If[x14V7>0,250+25 (-1+x14V7),0]>0,127,0]+(-2805+If[x15V1>0,50+5 (-1+x15V1),0]+If[x15V2>0,150+25 (-1+x15V2),0]+If[x15V5>0,250+25 (-1+x15V5),0]) If[2805-If[x15V1>0,50+5 (-1+x15V1),0]-If[x15V2>0,150+25 (-1+x15V2),0]-If[x15V5>0,250+25 (-1+x15V5),0]>0,179,0]+(-2438+If[x16V1>0,150+25 (-1+x16V1),0]+If[x16V3>0,200+25 (-1+x16V3),0]) If[2438-If[x16V1>0,150+25 (-1+x16V1),0]-If[x16V3>0,200+25 (-1+x16V3),0]>0,154,0]+(-2209+If[x17V10>0,400+100 (-1+x17V10),0]+If[x17V2>0,400+100 (-1+x17V2),0]+If[x17V3>0,30+5 (-1+x17V3),0]+If[x17V4>0,200+50 (-1+x17V4),0]+If[x17V5>0,250+25 (-1+x17V5),0]+If[x17V6>0,300+50 (-1+x17V6),0]+If[x17V8>0,30+5 (-1+x17V8),0]+If[x17V9>0,150+25 (-1+x17V9),0]) If[2209-If[x17V10>0,400+100 (-1+x17V10),0]-If[x17V2>0,400+100 (-1+x17V2),0]-If[x17V3>0,30+5 (-1+x17V3),0]-If[x17V4>0,200+50 (-1+x17V4),0]-If[x17V5>0,250+25 (-1+x17V5),0]-If[x17V6>0,300+50 (-1+x17V6),0]-If[x17V8>0,30+5 (-1+x17V8),0]-If[x17V9>0,150+25 (-1+x17V9),0]>0,149,0]+(-2438+If[x18V1>0,400+100 (-1+x18V1),0]+If[x18V10>0,40+5 (-1+x18V10),0]+If[x18V2>0,20+5 (-1+x18V2),0]+If[x18V3>0,60+10 (-1+x18V3),0]+If[x18V4>0,150+25 (-1+x18V4),0]+If[x18V5>0,400+100 (-1+x18V5),0]+If[x18V6>0,200+25 (-1+x18V6),0]+If[x18V7>0,200+50 (-1+x18V7),0]) If[2438-If[x18V1>0,400+100 (-1+x18V1),0]-If[x18V10>0,40+5 (-1+x18V10),0]-If[x18V2>0,20+5 (-1+x18V2),0]-If[x18V3>0,60+10 (-1+x18V3),0]-If[x18V4>0,150+25 (-1+x18V4),0]-If[x18V5>0,400+100 (-1+x18V5),0]-If[x18V6>0,200+25 (-1+x18V6),0]-If[x18V7>0,200+50 (-1+x18V7),0]>0,119,0]+(-2015+If[x19V10>0,100+25 (-1+x19V10),0]+If[x19V3>0,600+100 (-1+x19V3),0]+If[x19V5>0,800+100 (-1+x19V5),0]+If[x19V8>0,50+5 (-1+x19V8),0]+If[x19V9>0,1000+100 (-1+x19V9),0]) If[2015-If[x19V10>0,100+25 (-1+x19V10),0]-If[x19V3>0,600+100 (-1+x19V3),0]-If[x19V5>0,800+100 (-1+x19V5),0]-If[x19V8>0,50+5 (-1+x19V8),0]-If[x19V9>0,1000+100 (-1+x19V9),0]>0,143,0]+(-193+If[x1V5>0,200+50 (-1+x1V5),0]+If[x1V6>0,20+5 (-1+x1V6),0]+If[x1V7>0,80+10 (-1+x1V7),0]) If[193-If[x1V5>0,200+50 (-1+x1V5),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x1V7>0,80+10 (-1+x1V7),0]>0,148,0]+(-1391+If[x20V1>0,600+100 (-1+x20V1),0]+If[x20V10>0,20+5 (-1+x20V10),0]+If[x20V2>0,30+5 (-1+x20V2),0]+If[x20V3>0,100+25 (-1+x20V3),0]+If[x20V6>0,250+25 (-1+x20V6),0]+If[x20V8>0,400+100 (-1+x20V8),0]) If[1391-If[x20V1>0,600+100 (-1+x20V1),0]-If[x20V10>0,20+5 (-1+x20V10),0]-If[x20V2>0,30+5 (-1+x20V2),0]-If[x20V3>0,100+25 (-1+x20V3),0]-If[x20V6>0,250+25 (-1+x20V6),0]-If[x20V8>0,400+100 (-1+x20V8),0]>0,137,0]+(-357+If[x21V3>0,600+100 (-1+x21V3),0]+If[x21V5>0,100+25 (-1+x21V5),0]+If[x21V6>0,50+5 (-1+x21V6),0]+If[x21V7>0,300+50 (-1+x21V7),0]) If[357-If[x21V3>0,600+100 (-1+x21V3),0]-If[x21V5>0,100+25 (-1+x21V5),0]-If[x21V6>0,50+5 (-1+x21V6),0]-If[x21V7>0,300+50 (-1+x21V7),0]>0,196,0]+(-573+If[x22V1>0,50+5 (-1+x22V1),0]+If[x22V10>0,50+5 (-1+x22V10),0]+If[x22V3>0,150+25 (-1+x22V3),0]+If[x22V4>0,800+100 (-1+x22V4),0]+If[x22V6>0,20+5 (-1+x22V6),0]) If[573-If[x22V1>0,50+5 (-1+x22V1),0]-If[x22V10>0,50+5 (-1+x22V10),0]-If[x22V3>0,150+25 (-1+x22V3),0]-If[x22V4>0,800+100 (-1+x22V4),0]-If[x22V6>0,20+5 (-1+x22V6),0]>0,167,0]+(-1992+If[x23V4>0,250+25 (-1+x23V4),0]+If[x23V5>0,300+50 (-1+x23V5),0]) If[1992-If[x23V4>0,250+25 (-1+x23V4),0]-If[x23V5>0,300+50 (-1+x23V5),0]>0,179,0]+(-428+If[x24V1>0,500+50 (-1+x24V1),0]+If[x24V10>0,500+50 (-1+x24V10),0]+If[x24V3>0,150+25 (-1+x24V3),0]+If[x24V4>0,500+50 (-1+x24V4),0]+If[x24V5>0,200+50 (-1+x24V5),0]+If[x24V7>0,300+50 (-1+x24V7),0]+If[x24V9>0,200+50 (-1+x24V9),0]) If[428-If[x24V1>0,500+50 (-1+x24V1),0]-If[x24V10>0,500+50 (-1+x24V10),0]-If[x24V3>0,150+25 (-1+x24V3),0]-If[x24V4>0,500+50 (-1+x24V4),0]-If[x24V5>0,200+50 (-1+x24V5),0]-If[x24V7>0,300+50 (-1+x24V7),0]-If[x24V9>0,200+50 (-1+x24V9),0]>0,167,0]+(-86+If[x25V2>0,1000+100 (-1+x25V2),0]+If[x25V4>0,80+10 (-1+x25V4),0]+If[x25V7>0,50+5 (-1+x25V7),0]+If[x25V9>0,600+100 (-1+x25V9),0]) If[86-If[x25V2>0,1000+100 (-1+x25V2),0]-If[x25V4>0,80+10 (-1+x25V4),0]-If[x25V7>0,50+5 (-1+x25V7),0]-If[x25V9>0,600+100 (-1+x25V9),0]>0,141,0]+(-1145+If[x26V1>0,50+5 (-1+x26V1),0]+If[x26V6>0,30+5 (-1+x26V6),0]+If[x26V7>0,400+50 (-1+x26V7),0]+If[x26V9>0,20+5 (-1+x26V9),0]) If[1145-If[x26V1>0,50+5 (-1+x26V1),0]-If[x26V6>0,30+5 (-1+x26V6),0]-If[x26V7>0,400+50 (-1+x26V7),0]-If[x26V9>0,20+5 (-1+x26V9),0]>0,180,0]+(-508+If[x27V2>0,200+25 (-1+x27V2),0]+If[x27V4>0,50+5 (-1+x27V4),0]+If[x27V5>0,250+25 (-1+x27V5),0]) If[508-If[x27V2>0,200+25 (-1+x27V2),0]-If[x27V4>0,50+5 (-1+x27V4),0]-If[x27V5>0,250+25 (-1+x27V5),0]>0,173,0]+(-2013+If[x28V1>0,500+50 (-1+x28V1),0]+If[x28V6>0,40+10 (-1+x28V6),0]+If[x28V7>0,50+5 (-1+x28V7),0]+If[x28V8>0,400+50 (-1+x28V8),0]+If[x28V9>0,50+5 (-1+x28V9),0]) If[2013-If[x28V1>0,500+50 (-1+x28V1),0]-If[x28V6>0,40+10 (-1+x28V6),0]-If[x28V7>0,50+5 (-1+x28V7),0]-If[x28V8>0,400+50 (-1+x28V8),0]-If[x28V9>0,50+5 (-1+x28V9),0]>0,156,0]+(-1858+If[x29V1>0,200+50 (-1+x29V1),0]+If[x29V2>0,150+25 (-1+x29V2),0]+If[x29V3>0,500+50 (-1+x29V3),0]+If[x29V4>0,200+25 (-1+x29V4),0]+If[x29V5>0,600+100 (-1+x29V5),0]+If[x29V8>0,100+25 (-1+x29V8),0]+If[x29V9>0,600+100 (-1+x29V9),0]) If[1858-If[x29V1>0,200+50 (-1+x29V1),0]-If[x29V2>0,150+25 (-1+x29V2),0]-If[x29V3>0,500+50 (-1+x29V3),0]-If[x29V4>0,200+25 (-1+x29V4),0]-If[x29V5>0,600+100 (-1+x29V5),0]-If[x29V8>0,100+25 (-1+x29V8),0]-If[x29V9>0,600+100 (-1+x29V9),0]>0,123,0]+(-1161+If[x2V10>0,400+50 (-1+x2V10),0]+If[x2V2>0,1000+100 (-1+x2V2),0]+If[x2V5>0,60+10 (-1+x2V5),0]+If[x2V6>0,40+10 (-1+x2V6),0]+If[x2V7>0,500+50 (-1+x2V7),0]+If[x2V9>0,30+5 (-1+x2V9),0]) If[1161-If[x2V10>0,400+50 (-1+x2V10),0]-If[x2V2>0,1000+100 (-1+x2V2),0]-If[x2V5>0,60+10 (-1+x2V5),0]-If[x2V6>0,40+10 (-1+x2V6),0]-If[x2V7>0,500+50 (-1+x2V7),0]-If[x2V9>0,30+5 (-1+x2V9),0]>0,105,0]+(-1095+If[x30V1>0,1000+100 (-1+x30V1),0]+If[x30V4>0,100+25 (-1+x30V4),0]) If[1095-If[x30V1>0,1000+100 (-1+x30V1),0]-If[x30V4>0,100+25 (-1+x30V4),0]>0,193,0]+(-718+If[x31V1>0,1000+100 (-1+x31V1),0]+If[x31V10>0,600+100 (-1+x31V10),0]+If[x31V3>0,40+5 (-1+x31V3),0]+If[x31V5>0,500+50 (-1+x31V5),0]+If[x31V6>0,500+50 (-1+x31V6),0]+If[x31V7>0,150+25 (-1+x31V7),0]) If[718-If[x31V1>0,1000+100 (-1+x31V1),0]-If[x31V10>0,600+100 (-1+x31V10),0]-If[x31V3>0,40+5 (-1+x31V3),0]-If[x31V5>0,500+50 (-1+x31V5),0]-If[x31V6>0,500+50 (-1+x31V6),0]-If[x31V7>0,150+25 (-1+x31V7),0]>0,133,0]+(-2522+If[x32V1>0,400+100 (-1+x32V1),0]+If[x32V10>0,20+5 (-1+x32V10),0]+If[x32V3>0,1000+100 (-1+x32V3),0]+If[x32V8>0,600+100 (-1+x32V8),0]+If[x32V9>0,400+50 (-1+x32V9),0]) If[2522-If[x32V1>0,400+100 (-1+x32V1),0]-If[x32V10>0,20+5 (-1+x32V10),0]-If[x32V3>0,1000+100 (-1+x32V3),0]-If[x32V8>0,600+100 (-1+x32V8),0]-If[x32V9>0,400+50 (-1+x32V9),0]>0,177,0]+(-1651+If[x33V10>0,40+5 (-1+x33V10),0]+If[x33V7>0,40+5 (-1+x33V7),0]) If[1651-If[x33V10>0,40+5 (-1+x33V10),0]-If[x33V7>0,40+5 (-1+x33V7),0]>0,187,0]+(-1424+If[x34V10>0,40+5 (-1+x34V10),0]+If[x34V4>0,40+5 (-1+x34V4),0]+If[x34V6>0,60+10 (-1+x34V6),0]+If[x34V7>0,80+10 (-1+x34V7),0]+If[x34V8>0,30+5 (-1+x34V8),0]) If[1424-If[x34V10>0,40+5 (-1+x34V10),0]-If[x34V4>0,40+5 (-1+x34V4),0]-If[x34V6>0,60+10 (-1+x34V6),0]-If[x34V7>0,80+10 (-1+x34V7),0]-If[x34V8>0,30+5 (-1+x34V8),0]>0,156,0]+(-2618+If[x35V3>0,200+50 (-1+x35V3),0]+If[x35V4>0,100+10 (-1+x35V4),0]+If[x35V8>0,200+25 (-1+x35V8),0]) If[2618-If[x35V3>0,200+50 (-1+x35V3),0]-If[x35V4>0,100+10 (-1+x35V4),0]-If[x35V8>0,200+25 (-1+x35V8),0]>0,169,0]+(-897+If[x36V1>0,500+50 (-1+x36V1),0]+If[x36V2>0,400+50 (-1+x36V2),0]+If[x36V3>0,200+50 (-1+x36V3),0]+If[x36V4>0,250+25 (-1+x36V4),0]+If[x36V6>0,50+5 (-1+x36V6),0]+If[x36V9>0,40+5 (-1+x36V9),0]) If[897-If[x36V1>0,500+50 (-1+x36V1),0]-If[x36V2>0,400+50 (-1+x36V2),0]-If[x36V3>0,200+50 (-1+x36V3),0]-If[x36V4>0,250+25 (-1+x36V4),0]-If[x36V6>0,50+5 (-1+x36V6),0]-If[x36V9>0,40+5 (-1+x36V9),0]>0,125,0]+(-2855+If[x37V2>0,20+5 (-1+x37V2),0]+If[x37V3>0,30+5 (-1+x37V3),0]+If[x37V9>0,60+10 (-1+x37V9),0]) If[2855-If[x37V2>0,20+5 (-1+x37V2),0]-If[x37V3>0,30+5 (-1+x37V3),0]-If[x37V9>0,60+10 (-1+x37V9),0]>0,150,0]+(-883+If[x38V8>0,40+5 (-1+x38V8),0]+If[x38V9>0,200+25 (-1+x38V9),0]) If[883-If[x38V8>0,40+5 (-1+x38V8),0]-If[x38V9>0,200+25 (-1+x38V9),0]>0,123,0]+(-1292+If[x39V2>0,100+25 (-1+x39V2),0]+If[x39V4>0,200+25 (-1+x39V4),0]+If[x39V8>0,200+25 (-1+x39V8),0]+If[x39V9>0,80+10 (-1+x39V9),0]) If[1292-If[x39V2>0,100+25 (-1+x39V2),0]-If[x39V4>0,200+25 (-1+x39V4),0]-If[x39V8>0,200+25 (-1+x39V8),0]-If[x39V9>0,80+10 (-1+x39V9),0]>0,101,0]+(-654+If[x3V1>0,20+5 (-1+x3V1),0]+If[x3V3>0,400+100 (-1+x3V3),0]) If[654-If[x3V1>0,20+5 (-1+x3V1),0]-If[x3V3>0,400+100 (-1+x3V3),0]>0,150,0]+(-1910+If[x40V1>0,30+5 (-1+x40V1),0]+If[x40V9>0,1000+100 (-1+x40V9),0]) If[1910-If[x40V1>0,30+5 (-1+x40V1),0]-If[x40V9>0,1000+100 (-1+x40V9),0]>0,114,0]+(-2920+If[x41V1>0,200+25 (-1+x41V1),0]+If[x41V10>0,20+5 (-1+x41V10),0]+If[x41V3>0,300+50 (-1+x41V3),0]+If[x41V5>0,100+10 (-1+x41V5),0]+If[x41V6>0,800+100 (-1+x41V6),0]+If[x41V7>0,600+100 (-1+x41V7),0]+If[x41V8>0,60+10 (-1+x41V8),0]+If[x41V9>0,40+10 (-1+x41V9),0]) If[2920-If[x41V1>0,200+25 (-1+x41V1),0]-If[x41V10>0,20+5 (-1+x41V10),0]-If[x41V3>0,300+50 (-1+x41V3),0]-If[x41V5>0,100+10 (-1+x41V5),0]-If[x41V6>0,800+100 (-1+x41V6),0]-If[x41V7>0,600+100 (-1+x41V7),0]-If[x41V8>0,60+10 (-1+x41V8),0]-If[x41V9>0,40+10 (-1+x41V9),0]>0,169,0]+(-2758+If[x42V1>0,1000+100 (-1+x42V1),0]+If[x42V10>0,100+25 (-1+x42V10),0]+If[x42V3>0,500+50 (-1+x42V3),0]+If[x42V4>0,1000+100 (-1+x42V4),0]+If[x42V6>0,800+100 (-1+x42V6),0]+If[x42V7>0,80+10 (-1+x42V7),0]+If[x42V9>0,200+25 (-1+x42V9),0]) If[2758-If[x42V1>0,1000+100 (-1+x42V1),0]-If[x42V10>0,100+25 (-1+x42V10),0]-If[x42V3>0,500+50 (-1+x42V3),0]-If[x42V4>0,1000+100 (-1+x42V4),0]-If[x42V6>0,800+100 (-1+x42V6),0]-If[x42V7>0,80+10 (-1+x42V7),0]-If[x42V9>0,200+25 (-1+x42V9),0]>0,181,0]+(-906+If[x43V1>0,100+25 (-1+x43V1),0]+If[x43V10>0,300+50 (-1+x43V10),0]+If[x43V3>0,150+25 (-1+x43V3),0]+If[x43V4>0,800+100 (-1+x43V4),0]+If[x43V5>0,40+10 (-1+x43V5),0]+If[x43V8>0,200+50 (-1+x43V8),0]) If[906-If[x43V1>0,100+25 (-1+x43V1),0]-If[x43V10>0,300+50 (-1+x43V10),0]-If[x43V3>0,150+25 (-1+x43V3),0]-If[x43V4>0,800+100 (-1+x43V4),0]-If[x43V5>0,40+10 (-1+x43V5),0]-If[x43V8>0,200+50 (-1+x43V8),0]>0,145,0]+(-2681+If[x44V10>0,500+50 (-1+x44V10),0]+If[x44V2>0,400+50 (-1+x44V2),0]+If[x44V4>0,40+10 (-1+x44V4),0]+If[x44V6>0,80+10 (-1+x44V6),0]+If[x44V7>0,300+50 (-1+x44V7),0]) If[2681-If[x44V10>0,500+50 (-1+x44V10),0]-If[x44V2>0,400+50 (-1+x44V2),0]-If[x44V4>0,40+10 (-1+x44V4),0]-If[x44V6>0,80+10 (-1+x44V6),0]-If[x44V7>0,300+50 (-1+x44V7),0]>0,128,0]+(-1270+If[x45V1>0,600+100 (-1+x45V1),0]+If[x45V10>0,400+50 (-1+x45V10),0]+If[x45V3>0,100+10 (-1+x45V3),0]+If[x45V8>0,40+10 (-1+x45V8),0]) If[1270-If[x45V1>0,600+100 (-1+x45V1),0]-If[x45V10>0,400+50 (-1+x45V10),0]-If[x45V3>0,100+10 (-1+x45V3),0]-If[x45V8>0,40+10 (-1+x45V8),0]>0,167,0]+(-1445+If[x46V1>0,250+25 (-1+x46V1),0]+If[x46V2>0,500+50 (-1+x46V2),0]+If[x46V3>0,60+10 (-1+x46V3),0]+If[x46V4>0,250+25 (-1+x46V4),0]+If[x46V5>0,200+50 (-1+x46V5),0]+If[x46V6>0,600+100 (-1+x46V6),0]) If[1445-If[x46V1>0,250+25 (-1+x46V1),0]-If[x46V2>0,500+50 (-1+x46V2),0]-If[x46V3>0,60+10 (-1+x46V3),0]-If[x46V4>0,250+25 (-1+x46V4),0]-If[x46V5>0,200+50 (-1+x46V5),0]-If[x46V6>0,600+100 (-1+x46V6),0]>0,146,0]+(-2150+If[x47V1>0,1000+100 (-1+x47V1),0]+If[x47V10>0,200+50 (-1+x47V10),0]+If[x47V3>0,50+5 (-1+x47V3),0]+If[x47V5>0,500+50 (-1+x47V5),0]+If[x47V6>0,30+5 (-1+x47V6),0]+If[x47V8>0,200+50 (-1+x47V8),0]) If[2150-If[x47V1>0,1000+100 (-1+x47V1),0]-If[x47V10>0,200+50 (-1+x47V10),0]-If[x47V3>0,50+5 (-1+x47V3),0]-If[x47V5>0,500+50 (-1+x47V5),0]-If[x47V6>0,30+5 (-1+x47V6),0]-If[x47V8>0,200+50 (-1+x47V8),0]>0,190,0]+(-88+If[x48V2>0,300+50 (-1+x48V2),0]+If[x48V3>0,200+50 (-1+x48V3),0]+If[x48V4>0,30+5 (-1+x48V4),0]+If[x48V6>0,100+10 (-1+x48V6),0]+If[x48V7>0,200+25 (-1+x48V7),0]+If[x48V8>0,500+50 (-1+x48V8),0]) If[88-If[x48V2>0,300+50 (-1+x48V2),0]-If[x48V3>0,200+50 (-1+x48V3),0]-If[x48V4>0,30+5 (-1+x48V4),0]-If[x48V6>0,100+10 (-1+x48V6),0]-If[x48V7>0,200+25 (-1+x48V7),0]-If[x48V8>0,500+50 (-1+x48V8),0]>0,177,0]+(-1356+If[x49V2>0,40+10 (-1+x49V2),0]+If[x49V3>0,800+100 (-1+x49V3),0]+If[x49V4>0,20+5 (-1+x49V4),0]+If[x49V5>0,300+50 (-1+x49V5),0]+If[x49V9>0,100+25 (-1+x49V9),0]) If[1356-If[x49V2>0,40+10 (-1+x49V2),0]-If[x49V3>0,800+100 (-1+x49V3),0]-If[x49V4>0,20+5 (-1+x49V4),0]-If[x49V5>0,300+50 (-1+x49V5),0]-If[x49V9>0,100+25 (-1+x49V9),0]>0,163,0]+(-1685+If[x4V1>0,500+50 (-1+x4V1),0]+If[x4V10>0,50+5 (-1+x4V10),0]+If[x4V2>0,400+50 (-1+x4V2),0]+If[x4V4>0,40+5 (-1+x4V4),0]+If[x4V8>0,20+5 (-1+x4V8),0]+If[x4V9>0,30+5 (-1+x4V9),0]) If[1685-If[x4V1>0,500+50 (-1+x4V1),0]-If[x4V10>0,50+5 (-1+x4V10),0]-If[x4V2>0,400+50 (-1+x4V2),0]-If[x4V4>0,40+5 (-1+x4V4),0]-If[x4V8>0,20+5 (-1+x4V8),0]-If[x4V9>0,30+5 (-1+x4V9),0]>0,157,0]+(-666+If[x50V1>0,150+25 (-1+x50V1),0]+If[x50V3>0,20+5 (-1+x50V3),0]+If[x50V6>0,500+50 (-1+x50V6),0]+If[x50V8>0,500+50 (-1+x50V8),0]+If[x50V9>0,80+10 (-1+x50V9),0]) If[666-If[x50V1>0,150+25 (-1+x50V1),0]-If[x50V3>0,20+5 (-1+x50V3),0]-If[x50V6>0,500+50 (-1+x50V6),0]-If[x50V8>0,500+50 (-1+x50V8),0]-If[x50V9>0,80+10 (-1+x50V9),0]>0,100,0]+(-794+If[x51V1>0,100+10 (-1+x51V1),0]+If[x51V4>0,800+100 (-1+x51V4),0]+If[x51V6>0,200+50 (-1+x51V6),0]+If[x51V8>0,300+50 (-1+x51V8),0]+If[x51V9>0,400+100 (-1+x51V9),0]) If[794-If[x51V1>0,100+10 (-1+x51V1),0]-If[x51V4>0,800+100 (-1+x51V4),0]-If[x51V6>0,200+50 (-1+x51V6),0]-If[x51V8>0,300+50 (-1+x51V8),0]-If[x51V9>0,400+100 (-1+x51V9),0]>0,194,0]+(-2629+If[x52V3>0,50+5 (-1+x52V3),0]+If[x52V8>0,300+50 (-1+x52V8),0]) If[2629-If[x52V3>0,50+5 (-1+x52V3),0]-If[x52V8>0,300+50 (-1+x52V8),0]>0,118,0]+(-2710+If[x53V1>0,500+50 (-1+x53V1),0]+If[x53V10>0,40+10 (-1+x53V10),0]+If[x53V2>0,600+100 (-1+x53V2),0]+If[x53V7>0,400+100 (-1+x53V7),0]+If[x53V8>0,60+10 (-1+x53V8),0]+If[x53V9>0,150+25 (-1+x53V9),0]) If[2710-If[x53V1>0,500+50 (-1+x53V1),0]-If[x53V10>0,40+10 (-1+x53V10),0]-If[x53V2>0,600+100 (-1+x53V2),0]-If[x53V7>0,400+100 (-1+x53V7),0]-If[x53V8>0,60+10 (-1+x53V8),0]-If[x53V9>0,150+25 (-1+x53V9),0]>0,131,0]+(-2957+If[x54V10>0,80+10 (-1+x54V10),0]+If[x54V4>0,500+50 (-1+x54V4),0]) If[2957-If[x54V10>0,80+10 (-1+x54V10),0]-If[x54V4>0,500+50 (-1+x54V4),0]>0,147,0]+(-353+If[x55V1>0,40+10 (-1+x55V1),0]+If[x55V10>0,400+50 (-1+x55V10),0]+If[x55V2>0,40+10 (-1+x55V2),0]+If[x55V4>0,20+5 (-1+x55V4),0]+If[x55V6>0,1000+100 (-1+x55V6),0]+If[x55V7>0,500+50 (-1+x55V7),0]+If[x55V8>0,100+10 (-1+x55V8),0]+If[x55V9>0,80+10 (-1+x55V9),0]) If[353-If[x55V1>0,40+10 (-1+x55V1),0]-If[x55V10>0,400+50 (-1+x55V10),0]-If[x55V2>0,40+10 (-1+x55V2),0]-If[x55V4>0,20+5 (-1+x55V4),0]-If[x55V6>0,1000+100 (-1+x55V6),0]-If[x55V7>0,500+50 (-1+x55V7),0]-If[x55V8>0,100+10 (-1+x55V8),0]-If[x55V9>0,80+10 (-1+x55V9),0]>0,117,0]+(-2623+If[x56V2>0,20+5 (-1+x56V2),0]+If[x56V3>0,250+25 (-1+x56V3),0]+If[x56V8>0,1000+100 (-1+x56V8),0]) If[2623-If[x56V2>0,20+5 (-1+x56V2),0]-If[x56V3>0,250+25 (-1+x56V3),0]-If[x56V8>0,1000+100 (-1+x56V8),0]>0,130,0]+(-470+If[x57V1>0,500+50 (-1+x57V1),0]+If[x57V10>0,800+100 (-1+x57V10),0]+If[x57V4>0,1000+100 (-1+x57V4),0]+If[x57V5>0,150+25 (-1+x57V5),0]+If[x57V6>0,60+10 (-1+x57V6),0]+If[x57V8>0,30+5 (-1+x57V8),0]+If[x57V9>0,800+100 (-1+x57V9),0]) If[470-If[x57V1>0,500+50 (-1+x57V1),0]-If[x57V10>0,800+100 (-1+x57V10),0]-If[x57V4>0,1000+100 (-1+x57V4),0]-If[x57V5>0,150+25 (-1+x57V5),0]-If[x57V6>0,60+10 (-1+x57V6),0]-If[x57V8>0,30+5 (-1+x57V8),0]-If[x57V9>0,800+100 (-1+x57V9),0]>0,165,0]+(-1980+If[x58V1>0,250+25 (-1+x58V1),0]+If[x58V10>0,1000+100 (-1+x58V10),0]+If[x58V2>0,800+100 (-1+x58V2),0]+If[x58V3>0,500+50 (-1+x58V3),0]+If[x58V4>0,100+25 (-1+x58V4),0]+If[x58V5>0,400+50 (-1+x58V5),0]+If[x58V6>0,20+5 (-1+x58V6),0]+If[x58V7>0,100+10 (-1+x58V7),0]+If[x58V8>0,60+10 (-1+x58V8),0]+If[x58V9>0,100+10 (-1+x58V9),0]) If[1980-If[x58V1>0,250+25 (-1+x58V1),0]-If[x58V10>0,1000+100 (-1+x58V10),0]-If[x58V2>0,800+100 (-1+x58V2),0]-If[x58V3>0,500+50 (-1+x58V3),0]-If[x58V4>0,100+25 (-1+x58V4),0]-If[x58V5>0,400+50 (-1+x58V5),0]-If[x58V6>0,20+5 (-1+x58V6),0]-If[x58V7>0,100+10 (-1+x58V7),0]-If[x58V8>0,60+10 (-1+x58V8),0]-If[x58V9>0,100+10 (-1+x58V9),0]>0,155,0]+(-2941+If[x59V5>0,20+5 (-1+x59V5),0]+If[x59V7>0,100+25 (-1+x59V7),0]) If[2941-If[x59V5>0,20+5 (-1+x59V5),0]-If[x59V7>0,100+25 (-1+x59V7),0]>0,174,0]+(-2639+If[x5V1>0,600+100 (-1+x5V1),0]+If[x5V2>0,800+100 (-1+x5V2),0]+If[x5V4>0,60+10 (-1+x5V4),0]+If[x5V6>0,150+25 (-1+x5V6),0]+If[x5V7>0,150+25 (-1+x5V7),0]+If[x5V8>0,600+100 (-1+x5V8),0]+If[x5V9>0,80+10 (-1+x5V9),0]) If[2639-If[x5V1>0,600+100 (-1+x5V1),0]-If[x5V2>0,800+100 (-1+x5V2),0]-If[x5V4>0,60+10 (-1+x5V4),0]-If[x5V6>0,150+25 (-1+x5V6),0]-If[x5V7>0,150+25 (-1+x5V7),0]-If[x5V8>0,600+100 (-1+x5V8),0]-If[x5V9>0,80+10 (-1+x5V9),0]>0,157,0]+(-2928+If[x60V10>0,40+5 (-1+x60V10),0]+If[x60V2>0,250+25 (-1+x60V2),0]+If[x60V4>0,1000+100 (-1+x60V4),0]+If[x60V7>0,20+5 (-1+x60V7),0]+If[x60V9>0,40+10 (-1+x60V9),0]) If[2928-If[x60V10>0,40+5 (-1+x60V10),0]-If[x60V2>0,250+25 (-1+x60V2),0]-If[x60V4>0,1000+100 (-1+x60V4),0]-If[x60V7>0,20+5 (-1+x60V7),0]-If[x60V9>0,40+10 (-1+x60V9),0]>0,150,0]+(-2274+If[x61V2>0,250+25 (-1+x61V2),0]+If[x61V5>0,800+100 (-1+x61V5),0]+If[x61V7>0,100+10 (-1+x61V7),0]+If[x61V9>0,250+25 (-1+x61V9),0]) If[2274-If[x61V2>0,250+25 (-1+x61V2),0]-If[x61V5>0,800+100 (-1+x61V5),0]-If[x61V7>0,100+10 (-1+x61V7),0]-If[x61V9>0,250+25 (-1+x61V9),0]>0,102,0]+(-2666+If[x62V2>0,300+50 (-1+x62V2),0]+If[x62V7>0,200+25 (-1+x62V7),0]) If[2666-If[x62V2>0,300+50 (-1+x62V2),0]-If[x62V7>0,200+25 (-1+x62V7),0]>0,176,0]+(-1708+If[x63V3>0,100+25 (-1+x63V3),0]+If[x63V4>0,200+50 (-1+x63V4),0]+If[x63V5>0,150+25 (-1+x63V5),0]+If[x63V6>0,400+50 (-1+x63V6),0]+If[x63V7>0,40+10 (-1+x63V7),0]+If[x63V8>0,20+5 (-1+x63V8),0]+If[x63V9>0,30+5 (-1+x63V9),0]) If[1708-If[x63V3>0,100+25 (-1+x63V3),0]-If[x63V4>0,200+50 (-1+x63V4),0]-If[x63V5>0,150+25 (-1+x63V5),0]-If[x63V6>0,400+50 (-1+x63V6),0]-If[x63V7>0,40+10 (-1+x63V7),0]-If[x63V8>0,20+5 (-1+x63V8),0]-If[x63V9>0,30+5 (-1+x63V9),0]>0,147,0]+(-1826+If[x64V2>0,200+25 (-1+x64V2),0]+If[x64V4>0,80+10 (-1+x64V4),0]+If[x64V8>0,50+5 (-1+x64V8),0]) If[1826-If[x64V2>0,200+25 (-1+x64V2),0]-If[x64V4>0,80+10 (-1+x64V4),0]-If[x64V8>0,50+5 (-1+x64V8),0]>0,143,0]+(-665+If[x65V1>0,400+50 (-1+x65V1),0]+If[x65V10>0,800+100 (-1+x65V10),0]+If[x65V3>0,300+50 (-1+x65V3),0]+If[x65V4>0,100+25 (-1+x65V4),0]+If[x65V9>0,500+50 (-1+x65V9),0]) If[665-If[x65V1>0,400+50 (-1+x65V1),0]-If[x65V10>0,800+100 (-1+x65V10),0]-If[x65V3>0,300+50 (-1+x65V3),0]-If[x65V4>0,100+25 (-1+x65V4),0]-If[x65V9>0,500+50 (-1+x65V9),0]>0,112,0]+(-938+If[x66V2>0,600+100 (-1+x66V2),0]+If[x66V3>0,250+25 (-1+x66V3),0]+If[x66V4>0,30+5 (-1+x66V4),0]+If[x66V5>0,150+25 (-1+x66V5),0]+If[x66V6>0,50+5 (-1+x66V6),0]+If[x66V8>0,100+10 (-1+x66V8),0]+If[x66V9>0,500+50 (-1+x66V9),0]) If[938-If[x66V2>0,600+100 (-1+x66V2),0]-If[x66V3>0,250+25 (-1+x66V3),0]-If[x66V4>0,30+5 (-1+x66V4),0]-If[x66V5>0,150+25 (-1+x66V5),0]-If[x66V6>0,50+5 (-1+x66V6),0]-If[x66V8>0,100+10 (-1+x66V8),0]-If[x66V9>0,500+50 (-1+x66V9),0]>0,159,0]+(-2447+If[x67V4>0,60+10 (-1+x67V4),0]+If[x67V8>0,400+100 (-1+x67V8),0]) If[2447-If[x67V4>0,60+10 (-1+x67V4),0]-If[x67V8>0,400+100 (-1+x67V8),0]>0,169,0]+(-79+If[x68V1>0,150+25 (-1+x68V1),0]+If[x68V4>0,400+100 (-1+x68V4),0]+If[x68V5>0,30+5 (-1+x68V5),0]+If[x68V7>0,500+50 (-1+x68V7),0]+If[x68V8>0,50+5 (-1+x68V8),0]+If[x68V9>0,150+25 (-1+x68V9),0]) If[79-If[x68V1>0,150+25 (-1+x68V1),0]-If[x68V4>0,400+100 (-1+x68V4),0]-If[x68V5>0,30+5 (-1+x68V5),0]-If[x68V7>0,500+50 (-1+x68V7),0]-If[x68V8>0,50+5 (-1+x68V8),0]-If[x68V9>0,150+25 (-1+x68V9),0]>0,181,0]+(-1253+If[x69V1>0,50+5 (-1+x69V1),0]+If[x69V2>0,60+10 (-1+x69V2),0]+If[x69V4>0,100+10 (-1+x69V4),0]+If[x69V5>0,250+25 (-1+x69V5),0]+If[x69V7>0,300+50 (-1+x69V7),0]+If[x69V8>0,150+25 (-1+x69V8),0]+If[x69V9>0,40+5 (-1+x69V9),0]) If[1253-If[x69V1>0,50+5 (-1+x69V1),0]-If[x69V2>0,60+10 (-1+x69V2),0]-If[x69V4>0,100+10 (-1+x69V4),0]-If[x69V5>0,250+25 (-1+x69V5),0]-If[x69V7>0,300+50 (-1+x69V7),0]-If[x69V8>0,150+25 (-1+x69V8),0]-If[x69V9>0,40+5 (-1+x69V9),0]>0,110,0]+(-1994+If[x6V1>0,50+5 (-1+x6V1),0]+If[x6V10>0,300+50 (-1+x6V10),0]+If[x6V4>0,250+25 (-1+x6V4),0]+If[x6V8>0,50+5 (-1+x6V8),0]) If[1994-If[x6V1>0,50+5 (-1+x6V1),0]-If[x6V10>0,300+50 (-1+x6V10),0]-If[x6V4>0,250+25 (-1+x6V4),0]-If[x6V8>0,50+5 (-1+x6V8),0]>0,121,0]+(-2762+If[x70V10>0,50+5 (-1+x70V10),0]+If[x70V2>0,40+5 (-1+x70V2),0]+If[x70V3>0,60+10 (-1+x70V3),0]+If[x70V4>0,60+10 (-1+x70V4),0]+If[x70V6>0,150+25 (-1+x70V6),0]+If[x70V7>0,50+5 (-1+x70V7),0]) If[2762-If[x70V10>0,50+5 (-1+x70V10),0]-If[x70V2>0,40+5 (-1+x70V2),0]-If[x70V3>0,60+10 (-1+x70V3),0]-If[x70V4>0,60+10 (-1+x70V4),0]-If[x70V6>0,150+25 (-1+x70V6),0]-If[x70V7>0,50+5 (-1+x70V7),0]>0,153,0]+(-2504+If[x71V1>0,30+5 (-1+x71V1),0]+If[x71V3>0,250+25 (-1+x71V3),0]+If[x71V7>0,1000+100 (-1+x71V7),0]+If[x71V8>0,40+10 (-1+x71V8),0]+If[x71V9>0,40+5 (-1+x71V9),0]) If[2504-If[x71V1>0,30+5 (-1+x71V1),0]-If[x71V3>0,250+25 (-1+x71V3),0]-If[x71V7>0,1000+100 (-1+x71V7),0]-If[x71V8>0,40+10 (-1+x71V8),0]-If[x71V9>0,40+5 (-1+x71V9),0]>0,119,0]+(-357+If[x72V1>0,800+100 (-1+x72V1),0]+If[x72V7>0,400+100 (-1+x72V7),0]) If[357-If[x72V1>0,800+100 (-1+x72V1),0]-If[x72V7>0,400+100 (-1+x72V7),0]>0,178,0]+(-1374+If[x73V1>0,400+50 (-1+x73V1),0]+If[x73V2>0,50+5 (-1+x73V2),0]+If[x73V6>0,20+5 (-1+x73V6),0]+If[x73V8>0,80+10 (-1+x73V8),0]+If[x73V9>0,300+50 (-1+x73V9),0]) If[1374-If[x73V1>0,400+50 (-1+x73V1),0]-If[x73V2>0,50+5 (-1+x73V2),0]-If[x73V6>0,20+5 (-1+x73V6),0]-If[x73V8>0,80+10 (-1+x73V8),0]-If[x73V9>0,300+50 (-1+x73V9),0]>0,122,0]+(-1906+If[x74V2>0,40+5 (-1+x74V2),0]+If[x74V3>0,500+50 (-1+x74V3),0]+If[x74V4>0,80+10 (-1+x74V4),0]+If[x74V6>0,20+5 (-1+x74V6),0]) If[1906-If[x74V2>0,40+5 (-1+x74V2),0]-If[x74V3>0,500+50 (-1+x74V3),0]-If[x74V4>0,80+10 (-1+x74V4),0]-If[x74V6>0,20+5 (-1+x74V6),0]>0,124,0]+(-484+If[x75V1>0,500+50 (-1+x75V1),0]+If[x75V10>0,20+5 (-1+x75V10),0]+If[x75V4>0,30+5 (-1+x75V4),0]+If[x75V5>0,400+50 (-1+x75V5),0]+If[x75V6>0,100+10 (-1+x75V6),0]+If[x75V7>0,400+50 (-1+x75V7),0]+If[x75V9>0,20+5 (-1+x75V9),0]) If[484-If[x75V1>0,500+50 (-1+x75V1),0]-If[x75V10>0,20+5 (-1+x75V10),0]-If[x75V4>0,30+5 (-1+x75V4),0]-If[x75V5>0,400+50 (-1+x75V5),0]-If[x75V6>0,100+10 (-1+x75V6),0]-If[x75V7>0,400+50 (-1+x75V7),0]-If[x75V9>0,20+5 (-1+x75V9),0]>0,196,0]+(-2003+If[x76V10>0,250+25 (-1+x76V10),0]+If[x76V2>0,600+100 (-1+x76V2),0]+If[x76V3>0,100+25 (-1+x76V3),0]+If[x76V4>0,500+50 (-1+x76V4),0]+If[x76V7>0,40+5 (-1+x76V7),0]+If[x76V8>0,80+10 (-1+x76V8),0]+If[x76V9>0,1000+100 (-1+x76V9),0]) If[2003-If[x76V10>0,250+25 (-1+x76V10),0]-If[x76V2>0,600+100 (-1+x76V2),0]-If[x76V3>0,100+25 (-1+x76V3),0]-If[x76V4>0,500+50 (-1+x76V4),0]-If[x76V7>0,40+5 (-1+x76V7),0]-If[x76V8>0,80+10 (-1+x76V8),0]-If[x76V9>0,1000+100 (-1+x76V9),0]>0,131,0]+(-103+If[x77V10>0,30+5 (-1+x77V10),0]+If[x77V3>0,1000+100 (-1+x77V3),0]+If[x77V4>0,400+100 (-1+x77V4),0]+If[x77V6>0,400+100 (-1+x77V6),0]) If[103-If[x77V10>0,30+5 (-1+x77V10),0]-If[x77V3>0,1000+100 (-1+x77V3),0]-If[x77V4>0,400+100 (-1+x77V4),0]-If[x77V6>0,400+100 (-1+x77V6),0]>0,132,0]+(-2059+If[x78V1>0,20+5 (-1+x78V1),0]+If[x78V2>0,80+10 (-1+x78V2),0]+If[x78V3>0,80+10 (-1+x78V3),0]+If[x78V4>0,20+5 (-1+x78V4),0]+If[x78V5>0,40+10 (-1+x78V5),0]+If[x78V6>0,40+10 (-1+x78V6),0]) If[2059-If[x78V1>0,20+5 (-1+x78V1),0]-If[x78V2>0,80+10 (-1+x78V2),0]-If[x78V3>0,80+10 (-1+x78V3),0]-If[x78V4>0,20+5 (-1+x78V4),0]-If[x78V5>0,40+10 (-1+x78V5),0]-If[x78V6>0,40+10 (-1+x78V6),0]>0,103,0]+(-2189+If[x79V2>0,100+10 (-1+x79V2),0]+If[x79V6>0,800+100 (-1+x79V6),0]+If[x79V7>0,150+25 (-1+x79V7),0]) If[2189-If[x79V2>0,100+10 (-1+x79V2),0]-If[x79V6>0,800+100 (-1+x79V6),0]-If[x79V7>0,150+25 (-1+x79V7),0]>0,137,0]+(-1523+If[x7V10>0,40+10 (-1+x7V10),0]+If[x7V2>0,100+10 (-1+x7V2),0]+If[x7V4>0,150+25 (-1+x7V4),0]+If[x7V6>0,100+10 (-1+x7V6),0]) If[1523-If[x7V10>0,40+10 (-1+x7V10),0]-If[x7V2>0,100+10 (-1+x7V2),0]-If[x7V4>0,150+25 (-1+x7V4),0]-If[x7V6>0,100+10 (-1+x7V6),0]>0,122,0]+(-2860+If[x80V1>0,60+10 (-1+x80V1),0]+If[x80V5>0,800+100 (-1+x80V5),0]+If[x80V9>0,100+10 (-1+x80V9),0]) If[2860-If[x80V1>0,60+10 (-1+x80V1),0]-If[x80V5>0,800+100 (-1+x80V5),0]-If[x80V9>0,100+10 (-1+x80V9),0]>0,175,0]+(-1010+If[x81V1>0,20+5 (-1+x81V1),0]+If[x81V3>0,100+25 (-1+x81V3),0]+If[x81V8>0,150+25 (-1+x81V8),0]+If[x81V9>0,150+25 (-1+x81V9),0]) If[1010-If[x81V1>0,20+5 (-1+x81V1),0]-If[x81V3>0,100+25 (-1+x81V3),0]-If[x81V8>0,150+25 (-1+x81V8),0]-If[x81V9>0,150+25 (-1+x81V9),0]>0,101,0]+(-1599+If[x82V1>0,200+50 (-1+x82V1),0]+If[x82V10>0,80+10 (-1+x82V10),0]+If[x82V2>0,400+100 (-1+x82V2),0]+If[x82V4>0,150+25 (-1+x82V4),0]+If[x82V6>0,400+50 (-1+x82V6),0]+If[x82V7>0,40+10 (-1+x82V7),0]+If[x82V9>0,250+25 (-1+x82V9),0]) If[1599-If[x82V1>0,200+50 (-1+x82V1),0]-If[x82V10>0,80+10 (-1+x82V10),0]-If[x82V2>0,400+100 (-1+x82V2),0]-If[x82V4>0,150+25 (-1+x82V4),0]-If[x82V6>0,400+50 (-1+x82V6),0]-If[x82V7>0,40+10 (-1+x82V7),0]-If[x82V9>0,250+25 (-1+x82V9),0]>0,194,0]+(-1520+If[x83V1>0,200+50 (-1+x83V1),0]+If[x83V2>0,50+5 (-1+x83V2),0]+If[x83V9>0,300+50 (-1+x83V9),0]) If[1520-If[x83V1>0,200+50 (-1+x83V1),0]-If[x83V2>0,50+5 (-1+x83V2),0]-If[x83V9>0,300+50 (-1+x83V9),0]>0,136,0]+(-2337+If[x84V10>0,400+50 (-1+x84V10),0]+If[x84V2>0,60+10 (-1+x84V2),0]+If[x84V4>0,300+50 (-1+x84V4),0]+If[x84V5>0,200+50 (-1+x84V5),0]+If[x84V6>0,60+10 (-1+x84V6),0]+If[x84V7>0,60+10 (-1+x84V7),0]+If[x84V8>0,60+10 (-1+x84V8),0]) If[2337-If[x84V10>0,400+50 (-1+x84V10),0]-If[x84V2>0,60+10 (-1+x84V2),0]-If[x84V4>0,300+50 (-1+x84V4),0]-If[x84V5>0,200+50 (-1+x84V5),0]-If[x84V6>0,60+10 (-1+x84V6),0]-If[x84V7>0,60+10 (-1+x84V7),0]-If[x84V8>0,60+10 (-1+x84V8),0]>0,130,0]+(-1720+If[x85V1>0,200+50 (-1+x85V1),0]+If[x85V3>0,200+50 (-1+x85V3),0]+If[x85V4>0,40+10 (-1+x85V4),0]+If[x85V6>0,800+100 (-1+x85V6),0]+If[x85V7>0,30+5 (-1+x85V7),0]+If[x85V8>0,100+25 (-1+x85V8),0]+If[x85V9>0,400+100 (-1+x85V9),0]) If[1720-If[x85V1>0,200+50 (-1+x85V1),0]-If[x85V3>0,200+50 (-1+x85V3),0]-If[x85V4>0,40+10 (-1+x85V4),0]-If[x85V6>0,800+100 (-1+x85V6),0]-If[x85V7>0,30+5 (-1+x85V7),0]-If[x85V8>0,100+25 (-1+x85V8),0]-If[x85V9>0,400+100 (-1+x85V9),0]>0,110,0]+(-1283+If[x86V1>0,50+5 (-1+x86V1),0]+If[x86V10>0,100+25 (-1+x86V10),0]+If[x86V2>0,300+50 (-1+x86V2),0]+If[x86V4>0,100+10 (-1+x86V4),0]+If[x86V6>0,400+100 (-1+x86V6),0]+If[x86V7>0,100+10 (-1+x86V7),0]+If[x86V8>0,40+10 (-1+x86V8),0]+If[x86V9>0,30+5 (-1+x86V9),0]) If[1283-If[x86V1>0,50+5 (-1+x86V1),0]-If[x86V10>0,100+25 (-1+x86V10),0]-If[x86V2>0,300+50 (-1+x86V2),0]-If[x86V4>0,100+10 (-1+x86V4),0]-If[x86V6>0,400+100 (-1+x86V6),0]-If[x86V7>0,100+10 (-1+x86V7),0]-If[x86V8>0,40+10 (-1+x86V8),0]-If[x86V9>0,30+5 (-1+x86V9),0]>0,138,0]+(-784+If[x87V1>0,300+50 (-1+x87V1),0]+If[x87V6>0,30+5 (-1+x87V6),0]+If[x87V7>0,200+25 (-1+x87V7),0]+If[x87V8>0,100+10 (-1+x87V8),0]+If[x87V9>0,200+25 (-1+x87V9),0]) If[784-If[x87V1>0,300+50 (-1+x87V1),0]-If[x87V6>0,30+5 (-1+x87V6),0]-If[x87V7>0,200+25 (-1+x87V7),0]-If[x87V8>0,100+10 (-1+x87V8),0]-If[x87V9>0,200+25 (-1+x87V9),0]>0,109,0]+(-724+If[x88V1>0,100+10 (-1+x88V1),0]+If[x88V10>0,600+100 (-1+x88V10),0]+If[x88V2>0,250+25 (-1+x88V2),0]+If[x88V4>0,100+25 (-1+x88V4),0]+If[x88V5>0,20+5 (-1+x88V5),0]+If[x88V6>0,200+25 (-1+x88V6),0]+If[x88V7>0,200+25 (-1+x88V7),0]+If[x88V8>0,400+100 (-1+x88V8),0]+If[x88V9>0,100+25 (-1+x88V9),0]) If[724-If[x88V1>0,100+10 (-1+x88V1),0]-If[x88V10>0,600+100 (-1+x88V10),0]-If[x88V2>0,250+25 (-1+x88V2),0]-If[x88V4>0,100+25 (-1+x88V4),0]-If[x88V5>0,20+5 (-1+x88V5),0]-If[x88V6>0,200+25 (-1+x88V6),0]-If[x88V7>0,200+25 (-1+x88V7),0]-If[x88V8>0,400+100 (-1+x88V8),0]-If[x88V9>0,100+25 (-1+x88V9),0]>0,140,0]+(-1164+If[x89V2>0,800+100 (-1+x89V2),0]+If[x89V6>0,400+100 (-1+x89V6),0]+If[x89V8>0,400+50 (-1+x89V8),0]) If[1164-If[x89V2>0,800+100 (-1+x89V2),0]-If[x89V6>0,400+100 (-1+x89V6),0]-If[x89V8>0,400+50 (-1+x89V8),0]>0,101,0]+(-2702+If[x8V10>0,50+5 (-1+x8V10),0]+If[x8V3>0,50+5 (-1+x8V3),0]+If[x8V7>0,150+25 (-1+x8V7),0]+If[x8V8>0,20+5 (-1+x8V8),0]) If[2702-If[x8V10>0,50+5 (-1+x8V10),0]-If[x8V3>0,50+5 (-1+x8V3),0]-If[x8V7>0,150+25 (-1+x8V7),0]-If[x8V8>0,20+5 (-1+x8V8),0]>0,153,0]+(-2307+If[x90V1>0,60+10 (-1+x90V1),0]+If[x90V3>0,150+25 (-1+x90V3),0]+If[x90V4>0,100+10 (-1+x90V4),0]+If[x90V5>0,150+25 (-1+x90V5),0]+If[x90V7>0,500+50 (-1+x90V7),0]+If[x90V8>0,250+25 (-1+x90V8),0]+If[x90V9>0,400+100 (-1+x90V9),0]) If[2307-If[x90V1>0,60+10 (-1+x90V1),0]-If[x90V3>0,150+25 (-1+x90V3),0]-If[x90V4>0,100+10 (-1+x90V4),0]-If[x90V5>0,150+25 (-1+x90V5),0]-If[x90V7>0,500+50 (-1+x90V7),0]-If[x90V8>0,250+25 (-1+x90V8),0]-If[x90V9>0,400+100 (-1+x90V9),0]>0,170,0]+(-2023+If[x91V6>0,800+100 (-1+x91V6),0]+If[x91V8>0,100+10 (-1+x91V8),0]) If[2023-If[x91V6>0,800+100 (-1+x91V6),0]-If[x91V8>0,100+10 (-1+x91V8),0]>0,182,0]+(-419+If[x92V10>0,200+25 (-1+x92V10),0]+If[x92V2>0,800+100 (-1+x92V2),0]+If[x92V4>0,20+5 (-1+x92V4),0]+If[x92V7>0,250+25 (-1+x92V7),0]) If[419-If[x92V10>0,200+25 (-1+x92V10),0]-If[x92V2>0,800+100 (-1+x92V2),0]-If[x92V4>0,20+5 (-1+x92V4),0]-If[x92V7>0,250+25 (-1+x92V7),0]>0,186,0]+(-198+If[x93V1>0,200+25 (-1+x93V1),0]+If[x93V2>0,300+50 (-1+x93V2),0]+If[x93V4>0,30+5 (-1+x93V4),0]+If[x93V5>0,1000+100 (-1+x93V5),0]+If[x93V7>0,40+10 (-1+x93V7),0]+If[x93V8>0,20+5 (-1+x93V8),0]) If[198-If[x93V1>0,200+25 (-1+x93V1),0]-If[x93V2>0,300+50 (-1+x93V2),0]-If[x93V4>0,30+5 (-1+x93V4),0]-If[x93V5>0,1000+100 (-1+x93V5),0]-If[x93V7>0,40+10 (-1+x93V7),0]-If[x93V8>0,20+5 (-1+x93V8),0]>0,131,0]+(-1140+If[x94V10>0,30+5 (-1+x94V10),0]+If[x94V2>0,60+10 (-1+x94V2),0]+If[x94V3>0,250+25 (-1+x94V3),0]+If[x94V5>0,200+25 (-1+x94V5),0]+If[x94V7>0,500+50 (-1+x94V7),0]+If[x94V8>0,600+100 (-1+x94V8),0]+If[x94V9>0,400+50 (-1+x94V9),0]) If[1140-If[x94V10>0,30+5 (-1+x94V10),0]-If[x94V2>0,60+10 (-1+x94V2),0]-If[x94V3>0,250+25 (-1+x94V3),0]-If[x94V5>0,200+25 (-1+x94V5),0]-If[x94V7>0,500+50 (-1+x94V7),0]-If[x94V8>0,600+100 (-1+x94V8),0]-If[x94V9>0,400+50 (-1+x94V9),0]>0,127,0]+(-871+If[x95V1>0,200+25 (-1+x95V1),0]+If[x95V10>0,30+5 (-1+x95V10),0]+If[x95V3>0,300+50 (-1+x95V3),0]+If[x95V5>0,400+100 (-1+x95V5),0]+If[x95V8>0,300+50 (-1+x95V8),0]+If[x95V9>0,200+25 (-1+x95V9),0]) If[871-If[x95V1>0,200+25 (-1+x95V1),0]-If[x95V10>0,30+5 (-1+x95V10),0]-If[x95V3>0,300+50 (-1+x95V3),0]-If[x95V5>0,400+100 (-1+x95V5),0]-If[x95V8>0,300+50 (-1+x95V8),0]-If[x95V9>0,200+25 (-1+x95V9),0]>0,198,0]+(-1789+If[x96V10>0,200+25 (-1+x96V10),0]+If[x96V3>0,300+50 (-1+x96V3),0]+If[x96V4>0,200+50 (-1+x96V4),0]+If[x96V5>0,40+10 (-1+x96V5),0]+If[x96V6>0,50+5 (-1+x96V6),0]+If[x96V7>0,250+25 (-1+x96V7),0]+If[x96V9>0,20+5 (-1+x96V9),0]) If[1789-If[x96V10>0,200+25 (-1+x96V10),0]-If[x96V3>0,300+50 (-1+x96V3),0]-If[x96V4>0,200+50 (-1+x96V4),0]-If[x96V5>0,40+10 (-1+x96V5),0]-If[x96V6>0,50+5 (-1+x96V6),0]-If[x96V7>0,250+25 (-1+x96V7),0]-If[x96V9>0,20+5 (-1+x96V9),0]>0,143,0]+(-2116+If[x97V1>0,30+5 (-1+x97V1),0]+If[x97V2>0,50+5 (-1+x97V2),0]+If[x97V3>0,200+25 (-1+x97V3),0]+If[x97V6>0,800+100 (-1+x97V6),0]+If[x97V9>0,100+25 (-1+x97V9),0]) If[2116-If[x97V1>0,30+5 (-1+x97V1),0]-If[x97V2>0,50+5 (-1+x97V2),0]-If[x97V3>0,200+25 (-1+x97V3),0]-If[x97V6>0,800+100 (-1+x97V6),0]-If[x97V9>0,100+25 (-1+x97V9),0]>0,183,0]+(-242+If[x98V1>0,400+100 (-1+x98V1),0]+If[x98V10>0,250+25 (-1+x98V10),0]+If[x98V2>0,1000+100 (-1+x98V2),0]+If[x98V3>0,60+10 (-1+x98V3),0]+If[x98V5>0,40+10 (-1+x98V5),0]+If[x98V6>0,30+5 (-1+x98V6),0]+If[x98V7>0,150+25 (-1+x98V7),0]+If[x98V9>0,50+5 (-1+x98V9),0]) If[242-If[x98V1>0,400+100 (-1+x98V1),0]-If[x98V10>0,250+25 (-1+x98V10),0]-If[x98V2>0,1000+100 (-1+x98V2),0]-If[x98V3>0,60+10 (-1+x98V3),0]-If[x98V5>0,40+10 (-1+x98V5),0]-If[x98V6>0,30+5 (-1+x98V6),0]-If[x98V7>0,150+25 (-1+x98V7),0]-If[x98V9>0,50+5 (-1+x98V9),0]>0,161,0]+(-116+If[x99V3>0,40+10 (-1+x99V3),0]+If[x99V4>0,40+5 (-1+x99V4),0]+If[x99V9>0,600+100 (-1+x99V9),0]) If[116-If[x99V3>0,40+10 (-1+x99V3),0]-If[x99V4>0,40+5 (-1+x99V4),0]-If[x99V9>0,600+100 (-1+x99V9),0]>0,176,0]+(-2685+If[x9V1>0,400+100 (-1+x9V1),0]+If[x9V10>0,600+100 (-1+x9V10),0]+If[x9V2>0,1000+100 (-1+x9V2),0]+If[x9V4>0,150+25 (-1+x9V4),0]+If[x9V6>0,100+25 (-1+x9V6),0]+If[x9V8>0,600+100 (-1+x9V8),0]+If[x9V9>0,200+50 (-1+x9V9),0]) If[2685-If[x9V1>0,400+100 (-1+x9V1),0]-If[x9V10>0,600+100 (-1+x9V10),0]-If[x9V2>0,1000+100 (-1+x9V2),0]-If[x9V4>0,150+25 (-1+x9V4),0]-If[x9V6>0,100+25 (-1+x9V6),0]-If[x9V8>0,600+100 (-1+x9V8),0]-If[x9V9>0,200+50 (-1+x9V9),0]>0,194,0]
Fri 24 Nov 2017 04:34:02, time used: 0.572, total time used: 1.
---------------------------------------------
conf = 

    let conf = 
        {   
            ConfigData.defaultValue with
                noOfContracts = 100
                noOfAssets = 10
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
        |]

positions = 

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

Calling NMaximize...
Sat 25 Nov 2017 09:17:48, time used: 103426, total time used: 103427.
---------------------------------------------
sol = {-3.28524*10^6,{x1V5->1,x1V6->0,x1V7->0,x2V2->1,x2V5->0,x2V6->0,x2V7->2,x2V9->0,x2V10->0,x3V1->0,x3V3->0,x4V1->0,x4V2->0,x4V4->0,x4V8->1,x4V9->0,x4V10->0,x5V1->0,x5V2->1,x5V4->0,x5V6->0,x5V7->0,x5V8->0,x5V9->0,x6V1->0,x6V4->0,x6V8->0,x6V10->0,x7V2->0,x7V4->0,x7V6->0,x7V10->0,x8V3->0,x8V7->0,x8V8->1,x8V10->0,x9V1->0,x9V2->0,x9V4->1,x9V6->0,x9V8->0,x9V9->0,x9V10->0,x10V5->0,x10V7->0,x10V8->0,x10V10->0,x11V1->0,x11V2->0,x11V3->0,x11V4->0,x11V9->0,x11V10->0,x12V1->0,x12V3->0,x12V4->0,x12V5->0,x12V8->1,x12V9->1,x12V10->0,x13V1->0,x13V3->0,x13V4->1,x13V7->0,x13V10->0,x14V1->0,x14V2->0,x14V3->0,x14V4->0,x14V5->0,x14V6->1,x14V7->0,x14V10->0,x15V1->0,x15V2->0,x15V5->0,x16V1->0,x16V3->0,x17V2->0,x17V3->0,x17V4->0,x17V5->0,x17V6->0,x17V8->0,x17V9->0,x17V10->0,x18V1->0,x18V2->0,x18V3->0,x18V4->0,x18V5->0,x18V6->0,x18V7->1,x18V10->0,x19V3->0,x19V5->1,x19V8->0,x19V9->0,x19V10->0,x20V1->0,x20V2->0,x20V3->0,x20V6->0,x20V8->0,x20V10->0,x21V3->0,x21V5->0,x21V6->0,x21V7->1,x22V1->0,x22V3->0,x22V4->0,x22V6->0,x22V10->0,x23V4->0,x23V5->0,x24V1->0,x24V3->0,x24V4->1,x24V5->0,x24V7->0,x24V9->0,x24V10->1,x25V2->0,x25V4->1,x25V7->0,x25V9->1,x26V1->0,x26V6->0,x26V7->0,x26V9->0,x27V2->0,x27V4->0,x27V5->0,x28V1->0,x28V6->0,x28V7->0,x28V8->0,x28V9->0,x29V1->0,x29V2->0,x29V3->0,x29V4->0,x29V5->0,x29V8->0,x29V9->0,x30V1->1,x30V4->0,x31V1->1,x31V3->0,x31V5->0,x31V6->0,x31V7->0,x31V10->0,x32V1->0,x32V3->0,x32V8->0,x32V9->1,x32V10->0,x33V7->1,x33V10->0,x34V4->0,x34V6->0,x34V7->0,x34V8->0,x34V10->0,x35V3->0,x35V4->0,x35V8->0,x36V1->0,x36V2->0,x36V3->0,x36V4->0,x36V6->0,x36V9->0,x37V2->0,x37V3->0,x37V9->0,x38V8->0,x38V9->0,x39V2->1,x39V4->0,x39V8->0,x39V9->0,x40V1->0,x40V9->0,x41V1->0,x41V3->0,x41V5->0,x41V6->0,x41V7->0,x41V8->0,x41V9->0,x41V10->0,x42V1->0,x42V3->1,x42V4->0,x42V6->0,x42V7->0,x42V9->0,x42V10->0,x43V1->0,x43V3->0,x43V4->0,x43V5->0,x43V8->0,x43V10->0,x44V2->0,x44V4->0,x44V6->0,x44V7->0,x44V10->0,x45V1->0,x45V3->1,x45V8->1,x45V10->0,x46V1->0,x46V2->0,x46V3->0,x46V4->0,x46V5->0,x46V6->0,x47V1->1,x47V3->0,x47V5->0,x47V6->1,x47V8->0,x47V10->0,x48V2->0,x48V3->1,x48V4->0,x48V6->0,x48V7->1,x48V8->0,x49V2->0,x49V3->0,x49V4->0,x49V5->1,x49V9->0,x50V1->0,x50V3->0,x50V6->0,x50V8->0,x50V9->0,x51V1->0,x51V4->0,x51V6->0,x51V8->0,x51V9->0,x52V3->0,x52V8->0,x53V1->1,x53V2->0,x53V7->0,x53V8->0,x53V9->0,x53V10->0,x54V4->0,x54V10->0,x55V1->0,x55V2->0,x55V4->0,x55V6->0,x55V7->1,x55V8->0,x55V9->0,x55V10->1,x56V2->0,x56V3->0,x56V8->0,x57V1->1,x57V4->0,x57V5->0,x57V6->1,x57V8->0,x57V9->1,x57V10->1,x58V1->0,x58V2->0,x58V3->1,x58V4->0,x58V5->0,x58V6->0,x58V7->0,x58V8->0,x58V9->1,x58V10->0,x59V5->0,x59V7->0,x60V2->0,x60V4->0,x60V7->0,x60V9->0,x60V10->1,x61V2->0,x61V5->0,x61V7->0,x61V9->0,x62V2->0,x62V7->0,x63V3->0,x63V4->0,x63V5->0,x63V6->0,x63V7->0,x63V8->0,x63V9->0,x64V2->0,x64V4->0,x64V8->0,x65V1->0,x65V3->0,x65V4->0,x65V9->0,x65V10->0,x66V2->1,x66V3->0,x66V4->1,x66V5->0,x66V6->0,x66V8->0,x66V9->0,x67V4->0,x67V8->0,x68V1->1,x68V4->1,x68V5->0,x68V7->1,x68V8->0,x68V9->0,x69V1->0,x69V2->0,x69V4->0,x69V5->0,x69V7->0,x69V8->1,x69V9->0,x70V2->0,x70V3->0,x70V4->0,x70V6->0,x70V7->0,x70V10->0,x71V1->1,x71V3->0,x71V7->0,x71V8->0,x71V9->0,x72V1->0,x72V7->1,x73V1->0,x73V2->0,x73V6->0,x73V8->0,x73V9->0,x74V2->0,x74V3->0,x74V4->0,x74V6->0,x75V1->1,x75V4->0,x75V5->0,x75V6->0,x75V7->0,x75V9->0,x75V10->0,x76V2->0,x76V3->0,x76V4->0,x76V7->0,x76V8->0,x76V9->0,x76V10->0,x77V3->1,x77V4->0,x77V6->1,x77V10->0,x78V1->0,x78V2->0,x78V3->1,x78V4->0,x78V5->0,x78V6->0,x79V2->0,x79V6->0,x79V7->0,x80V1->1,x80V5->0,x80V9->0,x81V1->0,x81V3->0,x81V8->0,x81V9->0,x82V1->0,x82V2->0,x82V4->0,x82V6->0,x82V7->0,x82V9->0,x82V10->0,x83V1->0,x83V2->0,x83V9->0,x84V2->0,x84V4->0,x84V5->0,x84V6->0,x84V7->0,x84V8->0,x84V10->0,x85V1->0,x85V3->0,x85V4->0,x85V6->0,x85V7->0,x85V8->0,x85V9->0,x86V1->0,x86V2->0,x86V4->0,x86V6->0,x86V7->1,x86V8->0,x86V9->0,x86V10->1,x87V1->0,x87V6->0,x87V7->0,x87V8->0,x87V9->0,x88V1->0,x88V2->0,x88V4->0,x88V5->0,x88V6->0,x88V7->0,x88V8->0,x88V9->0,x88V10->0,x89V2->0,x89V6->0,x89V8->0,x90V1->0,x90V3->0,x90V4->0,x90V5->0,x90V7->0,x90V8->0,x90V9->0,x91V6->0,x91V8->0,x92V2->1,x92V4->0,x92V7->1,x92V10->0,x93V1->0,x93V2->1,x93V4->0,x93V5->0,x93V7->0,x93V8->0,x94V2->0,x94V3->0,x94V5->0,x94V7->0,x94V8->0,x94V9->0,x94V10->1,x95V1->0,x95V3->0,x95V5->0,x95V8->0,x95V9->0,x95V10->0,x96V3->0,x96V4->0,x96V5->0,x96V6->0,x96V7->0,x96V9->0,x96V10->0,x97V1->0,x97V2->0,x97V3->0,x97V6->0,x97V9->0,x98V1->0,x98V2->0,x98V3->0,x98V5->0,x98V6->0,x98V7->0,x98V9->0,x98V10->1,x99V3->1,x99V4->0,x99V9->1,x100V1->0,x100V3->0,x100V4->0,x100V6->1,x100V8->0,x100V9->0,x100V10->1}}
Initial balances
(7237
6700
50497
40180
37400
43915
58895
2335
68369
14162

)
=============================================

Updated balances:
(2497
3100
48077
38420
36100
43135
55855
2025
65069
11042

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

)
=============================================

Updated contract amounts.
(-7
-389
654
1665
1839
1994
1523
2682
2535
1641
1207
1493
391
792
2805
2438
2209
2238
1215
1391
57
573
1992
-572
-594
1145
508
2013
1858
95
-282
2122
1611
1424
2618
897
2855
883
1192
1910
2920
2258
906
2681
1130
1445
1120
-312
1056
666
794
2629
2210
2957
-547
2623
-1690
1380
2941
2888
2274
2666
1708
1826
665
308
2447
-971
1103
2762
2474
-43
1374
1906
-16
2003
-1297
1979
2189
2800
1010
1599
1520
2337
1720
1083
784
724
1164
2307
2023
-631
-102
1110
871
1789
2116
-8
-524
-427

)
=============================================
