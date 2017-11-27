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

noOfContracts =20;
noOfAssets = 6;

(*
noOfContracts =3;
noOfAssets = 2;
*)
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

(* ==============================================*)
Print["Variables"];
allVars=Table[ToExpression["x" <> ToString[ii] <> "V" <>  ToString[rdGetAsset[contrGetDescr[allContracts[[ii]]][[jj]]]]],{ii,1,noOfContracts}, {jj,1,Length[contrGetDescr[allContracts[[ii]]]]}];

allVarsLinear  = Flatten[allVars];
zeroRule=Table[allVarsLinear[[ii]] -> 0, {ii, 1, Length[allVarsLinear]}];

nonNegativeCond = "";
Do[nonNegativeCond = nonNegativeCond <> If[ii > 1, " && ", ""] <>ToString[allVarsLinear[[ii]]] <> " >= 0" ,{ii,1,Length[allVarsLinear]}];
nonNegativeCond=ToExpression[nonNegativeCond];

Print["allVars"];
Print[allVars // MatrixForm];
Print["allVarsLinear"];
Print[allVarsLinear];
Print["nonNegativeCond"];
Print[nonNegativeCond];

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
(* ==============================================*)

Print["generateContractCost"];
Print["generateContractCost[1] = ", generateContractCost[1]]

Print["generatePositionChange"];
Print[generatePositionChange[1]]

Print["generateMaxFunc"];
maxFunc=generateMaxFunc[];
Print[maxFunc];

Print["nonNegativeCond"];
Print[nonNegativeCond];

Print["nonNegBalanceCond"];
Print[nonNegBalanceCond];


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


(* 
contrGetBaseCurrency[c_?contractQ]:=c[[1]];
contrGetAmount[c_?contractQ]:=c[[2]];
contrGetDescr[c_?contractQ]:=c[[3]];
contrGetNonPayingRate[c_?contractQ]:=c[[4]];
contrGetOverPayingRate[c_?contractQ]:=c[[5]];

rdGetAsset[rd_?resourceDescriptorQ]:=rd[[1]];
rdGetMinVal[rd_?resourceDescriptorQ]:=rd[[2]];
rdGetIncr[rd_?resourceDescriptorQ]:=rd[[3]];

intGetIncomeRate[d_?interestRateQ]:=d[[1]];
intGetBorrowingRate[d_?interestRateQ]:=d[[2]];
*)

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

Print["Calling NMaximize..."];
sol = NMaximize[{maxFunc, nonNegativeCond && nonNegBalanceCond && Element[allVarsLinear, Integers]}, allVarsLinear, MaxIterations->100000];
printTimeUsed[];
Print["sol = ", sol];

Print["Initial balances"];
zeroBal=generateBalance[] /. zeroRule;
 Print[zeroBal // MatrixForm, strSeparator];

Print["Updated balances:"];
newBal=generateBalance[] /. sol[[2]];
Print[newBal // MatrixForm, strSeparator];

Print["Initial contract amounts"];
initContrAmt=Table[contrGetAmount[allContracts[[ii]]]-getContractAmt[ii] /.zeroRule, {ii,1 noOfContracts}];
 Print[initContrAmt // MatrixForm, strSeparator];

Print ["Updated contract amounts."];
newContrAmt = Table[contrGetAmount[allContracts[[ii]]]-getContractAmt[ii] /. sol[[2]], {ii,1 noOfContracts}];
Print[newContrAmt // MatrixForm, strSeparator];

(* ==============================================*)



seedRandomValue = 597543585369
allContracts
(6	1699	{{4,30,5},{6,20,5}}	138	0
3	2249	{{1,300,50},{3,400,50},{4,800,100},{5,200,25},{6,60,10}}	101	0
3	360	{{2,100,10},{3,500,50}}	162	0
1	263	{{1,50,5},{2,80,10},{3,60,10}}	130	0
1	2616	{{1,40,5},{2,200,25},{3,400,50},{5,50,5}}	131	0
5	62	{{1,400,100},{3,250,25},{4,300,50},{5,500,50},{6,400,100}}	190	0
5	2506	{{1,600,100},{2,200,25},{4,600,100},{5,200,50},{6,200,25}}	139	0
6	742	{{1,250,25},{2,250,25},{4,400,100},{6,400,50}}	184	0
3	1716	{{1,50,5},{2,300,50},{3,300,50},{4,200,25},{6,200,50}}	150	0
5	233	{{1,300,50},{2,20,5},{3,80,10},{4,200,25},{5,300,50}}	153	0
5	2940	{{2,40,5},{3,20,5},{4,100,25},{5,500,50}}	155	0
4	2011	{{1,1000,100},{2,150,25},{3,100,25},{4,400,100}}	146	0
4	2367	{{1,400,100},{4,60,10},{5,600,100},{6,1000,100}}	159	0
3	893	{{1,200,50},{2,100,25},{3,30,5},{5,200,25}}	199	0
2	651	{{1,200,25},{2,80,10},{4,60,10}}	149	0
4	2360	{{1,40,10},{2,300,50},{4,80,10},{5,100,25},{6,20,5}}	113	0
5	1958	{{3,20,5},{5,40,10}}	181	0
3	1419	{{3,200,50}}	158	0
5	762	{{1,150,25},{2,250,25},{4,250,25},{5,400,50},{6,200,50}}	200	0
1	821	{{1,100,25},{3,250,25},{4,40,10},{6,600,100}}	147	0

)
allExchangeRates
(1	1	1	1	1	1
1	1	1	1	1	1
1	1	1	1	1	1
1	1	1	1	1	1
1	1	1	1	1	1
1	1	1	1	1	1

)
allInterestRates
(90	1000000
97	1000000
56	1000000
7	1000000
35	1000000
44	1000000

)
allAssets
(12500
1088
6940
11222
8048
8645

)
Contract Cost
contr = {6,1699,{{4,30,5},{6,20,5}},138,0}
descr = {{4,30,5},{6,20,5}}
payVal = {2,2}
cost = -226182
change = {0,0,0,-35,0,-25}
Variables
allVars
({x1V4,x1V6}
{x2V1,x2V3,x2V4,x2V5,x2V6}
{x3V2,x3V3}
{x4V1,x4V2,x4V3}
{x5V1,x5V2,x5V3,x5V5}
{x6V1,x6V3,x6V4,x6V5,x6V6}
{x7V1,x7V2,x7V4,x7V5,x7V6}
{x8V1,x8V2,x8V4,x8V6}
{x9V1,x9V2,x9V3,x9V4,x9V6}
{x10V1,x10V2,x10V3,x10V4,x10V5}
{x11V2,x11V3,x11V4,x11V5}
{x12V1,x12V2,x12V3,x12V4}
{x13V1,x13V4,x13V5,x13V6}
{x14V1,x14V2,x14V3,x14V5}
{x15V1,x15V2,x15V4}
{x16V1,x16V2,x16V4,x16V5,x16V6}
{x17V3,x17V5}
{x18V3}
{x19V1,x19V2,x19V4,x19V5,x19V6}
{x20V1,x20V3,x20V4,x20V6}

)
allVarsLinear
{x1V4,x1V6,x2V1,x2V3,x2V4,x2V5,x2V6,x3V2,x3V3,x4V1,x4V2,x4V3,x5V1,x5V2,x5V3,x5V5,x6V1,x6V3,x6V4,x6V5,x6V6,x7V1,x7V2,x7V4,x7V5,x7V6,x8V1,x8V2,x8V4,x8V6,x9V1,x9V2,x9V3,x9V4,x9V6,x10V1,x10V2,x10V3,x10V4,x10V5,x11V2,x11V3,x11V4,x11V5,x12V1,x12V2,x12V3,x12V4,x13V1,x13V4,x13V5,x13V6,x14V1,x14V2,x14V3,x14V5,x15V1,x15V2,x15V4,x16V1,x16V2,x16V4,x16V5,x16V6,x17V3,x17V5,x18V3,x19V1,x19V2,x19V4,x19V5,x19V6,x20V1,x20V3,x20V4,x20V6}
nonNegativeCond
x1V4>=0&&x1V6>=0&&x2V1>=0&&x2V3>=0&&x2V4>=0&&x2V5>=0&&x2V6>=0&&x3V2>=0&&x3V3>=0&&x4V1>=0&&x4V2>=0&&x4V3>=0&&x5V1>=0&&x5V2>=0&&x5V3>=0&&x5V5>=0&&x6V1>=0&&x6V3>=0&&x6V4>=0&&x6V5>=0&&x6V6>=0&&x7V1>=0&&x7V2>=0&&x7V4>=0&&x7V5>=0&&x7V6>=0&&x8V1>=0&&x8V2>=0&&x8V4>=0&&x8V6>=0&&x9V1>=0&&x9V2>=0&&x9V3>=0&&x9V4>=0&&x9V6>=0&&x10V1>=0&&x10V2>=0&&x10V3>=0&&x10V4>=0&&x10V5>=0&&x11V2>=0&&x11V3>=0&&x11V4>=0&&x11V5>=0&&x12V1>=0&&x12V2>=0&&x12V3>=0&&x12V4>=0&&x13V1>=0&&x13V4>=0&&x13V5>=0&&x13V6>=0&&x14V1>=0&&x14V2>=0&&x14V3>=0&&x14V5>=0&&x15V1>=0&&x15V2>=0&&x15V4>=0&&x16V1>=0&&x16V2>=0&&x16V4>=0&&x16V5>=0&&x16V6>=0&&x17V3>=0&&x17V5>=0&&x18V3>=0&&x19V1>=0&&x19V2>=0&&x19V4>=0&&x19V5>=0&&x19V6>=0&&x20V1>=0&&x20V3>=0&&x20V4>=0&&x20V6>=0
generateContractCost
generateContractCost[1] = (-1699+If[x1V4>0,30+5 (-1+x1V4),0]+If[x1V6>0,20+5 (-1+x1V6),0]) If[1699-If[x1V4>0,30+5 (-1+x1V4),0]-If[x1V6>0,20+5 (-1+x1V6),0]>0,138,0]
generatePositionChange
{0,0,0,-If[x1V4>0,30+5 (-1+x1V4),0],0,-If[x1V6>0,20+5 (-1+x1V6),0]}
generateMaxFunc
35 (8048-If[x10V5>0,300+50 (-1+x10V5),0]-If[x11V5>0,500+50 (-1+x11V5),0]-If[x13V5>0,600+100 (-1+x13V5),0]-If[x14V5>0,200+25 (-1+x14V5),0]-If[x16V5>0,100+25 (-1+x16V5),0]-If[x17V5>0,40+10 (-1+x17V5),0]-If[x19V5>0,400+50 (-1+x19V5),0]-If[x2V5>0,200+25 (-1+x2V5),0]-If[x5V5>0,50+5 (-1+x5V5),0]-If[x6V5>0,500+50 (-1+x6V5),0]-If[x7V5>0,200+50 (-1+x7V5),0])+90 (12500-If[x10V1>0,300+50 (-1+x10V1),0]-If[x12V1>0,1000+100 (-1+x12V1),0]-If[x13V1>0,400+100 (-1+x13V1),0]-If[x14V1>0,200+50 (-1+x14V1),0]-If[x15V1>0,200+25 (-1+x15V1),0]-If[x16V1>0,40+10 (-1+x16V1),0]-If[x19V1>0,150+25 (-1+x19V1),0]-If[x20V1>0,100+25 (-1+x20V1),0]-If[x2V1>0,300+50 (-1+x2V1),0]-If[x4V1>0,50+5 (-1+x4V1),0]-If[x5V1>0,40+5 (-1+x5V1),0]-If[x6V1>0,400+100 (-1+x6V1),0]-If[x7V1>0,600+100 (-1+x7V1),0]-If[x8V1>0,250+25 (-1+x8V1),0]-If[x9V1>0,50+5 (-1+x9V1),0])+97 (1088-If[x10V2>0,20+5 (-1+x10V2),0]-If[x11V2>0,40+5 (-1+x11V2),0]-If[x12V2>0,150+25 (-1+x12V2),0]-If[x14V2>0,100+25 (-1+x14V2),0]-If[x15V2>0,80+10 (-1+x15V2),0]-If[x16V2>0,300+50 (-1+x16V2),0]-If[x19V2>0,250+25 (-1+x19V2),0]-If[x3V2>0,100+10 (-1+x3V2),0]-If[x4V2>0,80+10 (-1+x4V2),0]-If[x5V2>0,200+25 (-1+x5V2),0]-If[x7V2>0,200+25 (-1+x7V2),0]-If[x8V2>0,250+25 (-1+x8V2),0]-If[x9V2>0,300+50 (-1+x9V2),0])+56 (6940-If[x10V3>0,80+10 (-1+x10V3),0]-If[x11V3>0,20+5 (-1+x11V3),0]-If[x12V3>0,100+25 (-1+x12V3),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x17V3>0,20+5 (-1+x17V3),0]-If[x18V3>0,200+50 (-1+x18V3),0]-If[x20V3>0,250+25 (-1+x20V3),0]-If[x2V3>0,400+50 (-1+x2V3),0]-If[x3V3>0,500+50 (-1+x3V3),0]-If[x4V3>0,60+10 (-1+x4V3),0]-If[x5V3>0,400+50 (-1+x5V3),0]-If[x6V3>0,250+25 (-1+x6V3),0]-If[x9V3>0,300+50 (-1+x9V3),0])+7 (11222-If[x10V4>0,200+25 (-1+x10V4),0]-If[x11V4>0,100+25 (-1+x11V4),0]-If[x12V4>0,400+100 (-1+x12V4),0]-If[x13V4>0,60+10 (-1+x13V4),0]-If[x15V4>0,60+10 (-1+x15V4),0]-If[x16V4>0,80+10 (-1+x16V4),0]-If[x19V4>0,250+25 (-1+x19V4),0]-If[x1V4>0,30+5 (-1+x1V4),0]-If[x20V4>0,40+10 (-1+x20V4),0]-If[x2V4>0,800+100 (-1+x2V4),0]-If[x6V4>0,300+50 (-1+x6V4),0]-If[x7V4>0,600+100 (-1+x7V4),0]-If[x8V4>0,400+100 (-1+x8V4),0]-If[x9V4>0,200+25 (-1+x9V4),0])+44 (8645-If[x13V6>0,1000+100 (-1+x13V6),0]-If[x16V6>0,20+5 (-1+x16V6),0]-If[x19V6>0,200+50 (-1+x19V6),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x20V6>0,600+100 (-1+x20V6),0]-If[x2V6>0,60+10 (-1+x2V6),0]-If[x6V6>0,400+100 (-1+x6V6),0]-If[x7V6>0,200+25 (-1+x7V6),0]-If[x8V6>0,400+50 (-1+x8V6),0]-If[x9V6>0,200+50 (-1+x9V6),0])+(-233+If[x10V1>0,300+50 (-1+x10V1),0]+If[x10V2>0,20+5 (-1+x10V2),0]+If[x10V3>0,80+10 (-1+x10V3),0]+If[x10V4>0,200+25 (-1+x10V4),0]+If[x10V5>0,300+50 (-1+x10V5),0]) If[233-If[x10V1>0,300+50 (-1+x10V1),0]-If[x10V2>0,20+5 (-1+x10V2),0]-If[x10V3>0,80+10 (-1+x10V3),0]-If[x10V4>0,200+25 (-1+x10V4),0]-If[x10V5>0,300+50 (-1+x10V5),0]>0,153,0]+(-2940+If[x11V2>0,40+5 (-1+x11V2),0]+If[x11V3>0,20+5 (-1+x11V3),0]+If[x11V4>0,100+25 (-1+x11V4),0]+If[x11V5>0,500+50 (-1+x11V5),0]) If[2940-If[x11V2>0,40+5 (-1+x11V2),0]-If[x11V3>0,20+5 (-1+x11V3),0]-If[x11V4>0,100+25 (-1+x11V4),0]-If[x11V5>0,500+50 (-1+x11V5),0]>0,155,0]+(-2011+If[x12V1>0,1000+100 (-1+x12V1),0]+If[x12V2>0,150+25 (-1+x12V2),0]+If[x12V3>0,100+25 (-1+x12V3),0]+If[x12V4>0,400+100 (-1+x12V4),0]) If[2011-If[x12V1>0,1000+100 (-1+x12V1),0]-If[x12V2>0,150+25 (-1+x12V2),0]-If[x12V3>0,100+25 (-1+x12V3),0]-If[x12V4>0,400+100 (-1+x12V4),0]>0,146,0]+(-2367+If[x13V1>0,400+100 (-1+x13V1),0]+If[x13V4>0,60+10 (-1+x13V4),0]+If[x13V5>0,600+100 (-1+x13V5),0]+If[x13V6>0,1000+100 (-1+x13V6),0]) If[2367-If[x13V1>0,400+100 (-1+x13V1),0]-If[x13V4>0,60+10 (-1+x13V4),0]-If[x13V5>0,600+100 (-1+x13V5),0]-If[x13V6>0,1000+100 (-1+x13V6),0]>0,159,0]+(-893+If[x14V1>0,200+50 (-1+x14V1),0]+If[x14V2>0,100+25 (-1+x14V2),0]+If[x14V3>0,30+5 (-1+x14V3),0]+If[x14V5>0,200+25 (-1+x14V5),0]) If[893-If[x14V1>0,200+50 (-1+x14V1),0]-If[x14V2>0,100+25 (-1+x14V2),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x14V5>0,200+25 (-1+x14V5),0]>0,199,0]+(-651+If[x15V1>0,200+25 (-1+x15V1),0]+If[x15V2>0,80+10 (-1+x15V2),0]+If[x15V4>0,60+10 (-1+x15V4),0]) If[651-If[x15V1>0,200+25 (-1+x15V1),0]-If[x15V2>0,80+10 (-1+x15V2),0]-If[x15V4>0,60+10 (-1+x15V4),0]>0,149,0]+(-2360+If[x16V1>0,40+10 (-1+x16V1),0]+If[x16V2>0,300+50 (-1+x16V2),0]+If[x16V4>0,80+10 (-1+x16V4),0]+If[x16V5>0,100+25 (-1+x16V5),0]+If[x16V6>0,20+5 (-1+x16V6),0]) If[2360-If[x16V1>0,40+10 (-1+x16V1),0]-If[x16V2>0,300+50 (-1+x16V2),0]-If[x16V4>0,80+10 (-1+x16V4),0]-If[x16V5>0,100+25 (-1+x16V5),0]-If[x16V6>0,20+5 (-1+x16V6),0]>0,113,0]+(-1958+If[x17V3>0,20+5 (-1+x17V3),0]+If[x17V5>0,40+10 (-1+x17V5),0]) If[1958-If[x17V3>0,20+5 (-1+x17V3),0]-If[x17V5>0,40+10 (-1+x17V5),0]>0,181,0]+(-1419+If[x18V3>0,200+50 (-1+x18V3),0]) If[1419-If[x18V3>0,200+50 (-1+x18V3),0]>0,158,0]+(-762+If[x19V1>0,150+25 (-1+x19V1),0]+If[x19V2>0,250+25 (-1+x19V2),0]+If[x19V4>0,250+25 (-1+x19V4),0]+If[x19V5>0,400+50 (-1+x19V5),0]+If[x19V6>0,200+50 (-1+x19V6),0]) If[762-If[x19V1>0,150+25 (-1+x19V1),0]-If[x19V2>0,250+25 (-1+x19V2),0]-If[x19V4>0,250+25 (-1+x19V4),0]-If[x19V5>0,400+50 (-1+x19V5),0]-If[x19V6>0,200+50 (-1+x19V6),0]>0,200,0]+(-1699+If[x1V4>0,30+5 (-1+x1V4),0]+If[x1V6>0,20+5 (-1+x1V6),0]) If[1699-If[x1V4>0,30+5 (-1+x1V4),0]-If[x1V6>0,20+5 (-1+x1V6),0]>0,138,0]+(-821+If[x20V1>0,100+25 (-1+x20V1),0]+If[x20V3>0,250+25 (-1+x20V3),0]+If[x20V4>0,40+10 (-1+x20V4),0]+If[x20V6>0,600+100 (-1+x20V6),0]) If[821-If[x20V1>0,100+25 (-1+x20V1),0]-If[x20V3>0,250+25 (-1+x20V3),0]-If[x20V4>0,40+10 (-1+x20V4),0]-If[x20V6>0,600+100 (-1+x20V6),0]>0,147,0]+(-2249+If[x2V1>0,300+50 (-1+x2V1),0]+If[x2V3>0,400+50 (-1+x2V3),0]+If[x2V4>0,800+100 (-1+x2V4),0]+If[x2V5>0,200+25 (-1+x2V5),0]+If[x2V6>0,60+10 (-1+x2V6),0]) If[2249-If[x2V1>0,300+50 (-1+x2V1),0]-If[x2V3>0,400+50 (-1+x2V3),0]-If[x2V4>0,800+100 (-1+x2V4),0]-If[x2V5>0,200+25 (-1+x2V5),0]-If[x2V6>0,60+10 (-1+x2V6),0]>0,101,0]+(-360+If[x3V2>0,100+10 (-1+x3V2),0]+If[x3V3>0,500+50 (-1+x3V3),0]) If[360-If[x3V2>0,100+10 (-1+x3V2),0]-If[x3V3>0,500+50 (-1+x3V3),0]>0,162,0]+(-263+If[x4V1>0,50+5 (-1+x4V1),0]+If[x4V2>0,80+10 (-1+x4V2),0]+If[x4V3>0,60+10 (-1+x4V3),0]) If[263-If[x4V1>0,50+5 (-1+x4V1),0]-If[x4V2>0,80+10 (-1+x4V2),0]-If[x4V3>0,60+10 (-1+x4V3),0]>0,130,0]+(-2616+If[x5V1>0,40+5 (-1+x5V1),0]+If[x5V2>0,200+25 (-1+x5V2),0]+If[x5V3>0,400+50 (-1+x5V3),0]+If[x5V5>0,50+5 (-1+x5V5),0]) If[2616-If[x5V1>0,40+5 (-1+x5V1),0]-If[x5V2>0,200+25 (-1+x5V2),0]-If[x5V3>0,400+50 (-1+x5V3),0]-If[x5V5>0,50+5 (-1+x5V5),0]>0,131,0]+(-62+If[x6V1>0,400+100 (-1+x6V1),0]+If[x6V3>0,250+25 (-1+x6V3),0]+If[x6V4>0,300+50 (-1+x6V4),0]+If[x6V5>0,500+50 (-1+x6V5),0]+If[x6V6>0,400+100 (-1+x6V6),0]) If[62-If[x6V1>0,400+100 (-1+x6V1),0]-If[x6V3>0,250+25 (-1+x6V3),0]-If[x6V4>0,300+50 (-1+x6V4),0]-If[x6V5>0,500+50 (-1+x6V5),0]-If[x6V6>0,400+100 (-1+x6V6),0]>0,190,0]+(-2506+If[x7V1>0,600+100 (-1+x7V1),0]+If[x7V2>0,200+25 (-1+x7V2),0]+If[x7V4>0,600+100 (-1+x7V4),0]+If[x7V5>0,200+50 (-1+x7V5),0]+If[x7V6>0,200+25 (-1+x7V6),0]) If[2506-If[x7V1>0,600+100 (-1+x7V1),0]-If[x7V2>0,200+25 (-1+x7V2),0]-If[x7V4>0,600+100 (-1+x7V4),0]-If[x7V5>0,200+50 (-1+x7V5),0]-If[x7V6>0,200+25 (-1+x7V6),0]>0,139,0]+(-742+If[x8V1>0,250+25 (-1+x8V1),0]+If[x8V2>0,250+25 (-1+x8V2),0]+If[x8V4>0,400+100 (-1+x8V4),0]+If[x8V6>0,400+50 (-1+x8V6),0]) If[742-If[x8V1>0,250+25 (-1+x8V1),0]-If[x8V2>0,250+25 (-1+x8V2),0]-If[x8V4>0,400+100 (-1+x8V4),0]-If[x8V6>0,400+50 (-1+x8V6),0]>0,184,0]+(-1716+If[x9V1>0,50+5 (-1+x9V1),0]+If[x9V2>0,300+50 (-1+x9V2),0]+If[x9V3>0,300+50 (-1+x9V3),0]+If[x9V4>0,200+25 (-1+x9V4),0]+If[x9V6>0,200+50 (-1+x9V6),0]) If[1716-If[x9V1>0,50+5 (-1+x9V1),0]-If[x9V2>0,300+50 (-1+x9V2),0]-If[x9V3>0,300+50 (-1+x9V3),0]-If[x9V4>0,200+25 (-1+x9V4),0]-If[x9V6>0,200+50 (-1+x9V6),0]>0,150,0]
nonNegativeCond
x1V4>=0&&x1V6>=0&&x2V1>=0&&x2V3>=0&&x2V4>=0&&x2V5>=0&&x2V6>=0&&x3V2>=0&&x3V3>=0&&x4V1>=0&&x4V2>=0&&x4V3>=0&&x5V1>=0&&x5V2>=0&&x5V3>=0&&x5V5>=0&&x6V1>=0&&x6V3>=0&&x6V4>=0&&x6V5>=0&&x6V6>=0&&x7V1>=0&&x7V2>=0&&x7V4>=0&&x7V5>=0&&x7V6>=0&&x8V1>=0&&x8V2>=0&&x8V4>=0&&x8V6>=0&&x9V1>=0&&x9V2>=0&&x9V3>=0&&x9V4>=0&&x9V6>=0&&x10V1>=0&&x10V2>=0&&x10V3>=0&&x10V4>=0&&x10V5>=0&&x11V2>=0&&x11V3>=0&&x11V4>=0&&x11V5>=0&&x12V1>=0&&x12V2>=0&&x12V3>=0&&x12V4>=0&&x13V1>=0&&x13V4>=0&&x13V5>=0&&x13V6>=0&&x14V1>=0&&x14V2>=0&&x14V3>=0&&x14V5>=0&&x15V1>=0&&x15V2>=0&&x15V4>=0&&x16V1>=0&&x16V2>=0&&x16V4>=0&&x16V5>=0&&x16V6>=0&&x17V3>=0&&x17V5>=0&&x18V3>=0&&x19V1>=0&&x19V2>=0&&x19V4>=0&&x19V5>=0&&x19V6>=0&&x20V1>=0&&x20V3>=0&&x20V4>=0&&x20V6>=0
nonNegBalanceCond
12500-If[x10V1>0,300+50 (-1+x10V1),0]-If[x12V1>0,1000+100 (-1+x12V1),0]-If[x13V1>0,400+100 (-1+x13V1),0]-If[x14V1>0,200+50 (-1+x14V1),0]-If[x15V1>0,200+25 (-1+x15V1),0]-If[x16V1>0,40+10 (-1+x16V1),0]-If[x19V1>0,150+25 (-1+x19V1),0]-If[x20V1>0,100+25 (-1+x20V1),0]-If[x2V1>0,300+50 (-1+x2V1),0]-If[x4V1>0,50+5 (-1+x4V1),0]-If[x5V1>0,40+5 (-1+x5V1),0]-If[x6V1>0,400+100 (-1+x6V1),0]-If[x7V1>0,600+100 (-1+x7V1),0]-If[x8V1>0,250+25 (-1+x8V1),0]-If[x9V1>0,50+5 (-1+x9V1),0]>=0&&1088-If[x10V2>0,20+5 (-1+x10V2),0]-If[x11V2>0,40+5 (-1+x11V2),0]-If[x12V2>0,150+25 (-1+x12V2),0]-If[x14V2>0,100+25 (-1+x14V2),0]-If[x15V2>0,80+10 (-1+x15V2),0]-If[x16V2>0,300+50 (-1+x16V2),0]-If[x19V2>0,250+25 (-1+x19V2),0]-If[x3V2>0,100+10 (-1+x3V2),0]-If[x4V2>0,80+10 (-1+x4V2),0]-If[x5V2>0,200+25 (-1+x5V2),0]-If[x7V2>0,200+25 (-1+x7V2),0]-If[x8V2>0,250+25 (-1+x8V2),0]-If[x9V2>0,300+50 (-1+x9V2),0]>=0&&6940-If[x10V3>0,80+10 (-1+x10V3),0]-If[x11V3>0,20+5 (-1+x11V3),0]-If[x12V3>0,100+25 (-1+x12V3),0]-If[x14V3>0,30+5 (-1+x14V3),0]-If[x17V3>0,20+5 (-1+x17V3),0]-If[x18V3>0,200+50 (-1+x18V3),0]-If[x20V3>0,250+25 (-1+x20V3),0]-If[x2V3>0,400+50 (-1+x2V3),0]-If[x3V3>0,500+50 (-1+x3V3),0]-If[x4V3>0,60+10 (-1+x4V3),0]-If[x5V3>0,400+50 (-1+x5V3),0]-If[x6V3>0,250+25 (-1+x6V3),0]-If[x9V3>0,300+50 (-1+x9V3),0]>=0&&11222-If[x10V4>0,200+25 (-1+x10V4),0]-If[x11V4>0,100+25 (-1+x11V4),0]-If[x12V4>0,400+100 (-1+x12V4),0]-If[x13V4>0,60+10 (-1+x13V4),0]-If[x15V4>0,60+10 (-1+x15V4),0]-If[x16V4>0,80+10 (-1+x16V4),0]-If[x19V4>0,250+25 (-1+x19V4),0]-If[x1V4>0,30+5 (-1+x1V4),0]-If[x20V4>0,40+10 (-1+x20V4),0]-If[x2V4>0,800+100 (-1+x2V4),0]-If[x6V4>0,300+50 (-1+x6V4),0]-If[x7V4>0,600+100 (-1+x7V4),0]-If[x8V4>0,400+100 (-1+x8V4),0]-If[x9V4>0,200+25 (-1+x9V4),0]>=0&&8048-If[x10V5>0,300+50 (-1+x10V5),0]-If[x11V5>0,500+50 (-1+x11V5),0]-If[x13V5>0,600+100 (-1+x13V5),0]-If[x14V5>0,200+25 (-1+x14V5),0]-If[x16V5>0,100+25 (-1+x16V5),0]-If[x17V5>0,40+10 (-1+x17V5),0]-If[x19V5>0,400+50 (-1+x19V5),0]-If[x2V5>0,200+25 (-1+x2V5),0]-If[x5V5>0,50+5 (-1+x5V5),0]-If[x6V5>0,500+50 (-1+x6V5),0]-If[x7V5>0,200+50 (-1+x7V5),0]>=0&&8645-If[x13V6>0,1000+100 (-1+x13V6),0]-If[x16V6>0,20+5 (-1+x16V6),0]-If[x19V6>0,200+50 (-1+x19V6),0]-If[x1V6>0,20+5 (-1+x1V6),0]-If[x20V6>0,600+100 (-1+x20V6),0]-If[x2V6>0,60+10 (-1+x2V6),0]-If[x6V6>0,400+100 (-1+x6V6),0]-If[x7V6>0,200+25 (-1+x7V6),0]-If[x8V6>0,400+50 (-1+x8V6),0]-If[x9V6>0,200+50 (-1+x9V6),0]>=0
Wed 22 Nov 2017 19:22:47, time used: 0.114, total time used: 0.
---------------------------------------------
conf = 

    let conf = 
        {   
            ConfigData.defaultValue with
                noOfContracts = 20
                noOfAssets = 6
                rescaleRates = true
        }


contracts = 

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

positions = 

    let positions : PositionData[] = 
        [|
            { asset = 0; balance = 12500.; incomeRate = 90. }
            { asset = 1; balance = 1088.; incomeRate = 97. }
            { asset = 2; balance = 6940.; incomeRate = 56. }
            { asset = 3; balance = 11222.; incomeRate = 7. }
            { asset = 4; balance = 8048.; incomeRate = 35. }
            { asset = 5; balance = 8645.; incomeRate = 44. }
        |]

Calling NMaximize...
Wed 22 Nov 2017 23:05:45, time used: 13379, total time used: 13379.
---------------------------------------------
sol = {1.50739*10^6,{x1V4->324,x1V6->8,x2V1->0,x2V3->0,x2V4->10,x2V5->9,x2V6->10,x3V2->0,x3V3->1,x4V1->0,x4V2->0,x4V3->21,x5V1->0,x5V2->0,x5V3->43,x5V5->14,x6V1->0,x6V3->0,x6V4->0,x6V5->0,x6V6->0,x7V1->0,x7V2->0,x7V4->13,x7V5->5,x7V6->5,x8V1->0,x8V2->0,x8V4->0,x8V6->8,x9V1->0,x9V2->0,x9V3->0,x9V4->16,x9V6->20,x10V1->0,x10V2->0,x10V3->0,x10V4->3,x10V5->0,x11V2->0,x11V3->1,x11V4->4,x11V5->46,x12V1->0,x12V2->0,x12V3->0,x12V4->17,x13V1->0,x13V4->2,x13V5->3,x13V6->6,x14V1->0,x14V2->0,x14V3->14,x14V5->25,x15V1->0,x15V2->0,x15V4->60,x16V1->0,x16V2->0,x16V4->147,x16V5->29,x16V6->1,x17V3->0,x17V5->193,x18V3->26,x19V1->0,x19V2->0,x19V4->22,x19V5->0,x19V6->0,x20V1->0,x20V3->0,x20V4->1,x20V6->3}}
Initial balances
(12500
1088
6940
11222
8048
8645

)=============================================

Updated balances:
(12500
1088
2115
2
23
3920

)=============================================

Initial contract amounts
(1699
2249
360
263
2616
62
2506
742
1716
233
2940
2011
2367
893
651
2360
1958
1419
762
821

)=============================================

Updated contract amounts.
(-1
-1
-140
3
1
62
6
-8
-9
-17
-5
11
-3
-2
1
0
-2
-31
-13
-19

)=============================================
