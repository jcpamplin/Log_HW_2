/*--------------------------------*/
/*  MSA 2019: Logistic Regression */
/*           Homework 2           */
/*                                */
/*         Homework Team 9        */
/*--------------------------------*/

libname logistic 'C:\Users\14lmu\OneDrive\Documents\LogisticRegressionData\MSA2019LogisticData\data';
run;

*1. Revise the model that you built in your previous report. 
As before, you are free to do so in any way you’d like—this includes 
transforming/combining predictors in some sensible way if you can think of one. 
This can also include making a completely new model if you hate your current one.;

data logistic.insurance_modified;
set logistic.insurance_t;
if savbal <> 0 then modified_savbal = log(savbal);
else modified_savbal = savbal;
if atmamt <> 0 then modified_atmamt = log(atmamt);
else modified_atmamt = atmamt;
run; 

/*Old Model*/
proc logistic data=logistic.insurance_t;
	model INS(event='1') = CC CD DDA DEP INV IRA MM MTG TELLER SAVBAL PHONE ATMAMT;
run;
*original AIC: 8527.198
 
/*First Final Model*/
*AIC with transformed log variables;
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = CC CD DDA DEP INV IRA MM MTG TELLER modified_savbal PHONE modified_atmamt;
run;
*AIC 8346.084;

*Two variables right on the verge of being 
insignificant: but the fact is, the AIC doesn't change. 
Model with less predictors should be better;

/*Second Final Model*/
%let FinalModVar =  CC CD DDA DEP INV IRA MM TELLER modified_savbal PHONE; 
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar;
run;
*AIC: 8349.947;

*Testing for multicollinearity; 
proc reg data=logistic.insurance_modified; 
model INS = &FinalModVar / tol vif ;
run;


*2. Make sure to check for separation and adjust accordingly. 
This may or may not change your model.;  

*Checking the model for separation requires looking at the pvalues for the estimates
*Are they close to 1? No
*Are the errors very high? No 
*page 47-49 provide a more nontechnical description;

*3. Check the linearity assumption for any continuous variables in your model
that you suspect might have a nonlinear effect. Make any corrections necessary. 
Don’t forget model hierarchy.;  

*partial residuals of all of our continuous predictors - modified_savbal modified_atmamt; 
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar;
	output out=predicted reschi=respearson pred=phat predprobs=x;
run;

data predicted;
set predicted;
 working = (INS - phat)/(phat*(1 - phat));
 respart_SAVBAL = 0.1476*modified_savbal + working;
 run;

 *partial residual plot for SAVBAL; 
 ODS GRAPHICS on/LOESSMAXOBS=10000;/* for getting CI for data points > 5000*/
 proc sgplot data=predicted;
 scatter x=modified_savbal y=respart_SAVBAL;
 loess x=modified_savbal y=respart_SAVBAL / clm;
 reg x=modified_savbal y=respart_SAVBAL / nomarkers;
 run;  

*Fitting the additive model;  
*Higher order terms probably not necessary because partial residual plot has loess/regression aligned;
 proc gam data=logistic.insurance_modified plots=components(clm additive commonaxes);
	model INS(event='1') = param(&FinalModVar) / dist=binomial link=logit;
run; 
*this confirms this hypothesis; 


*Calibration curve; 
proc sgplot data=predicted;
loess x=phat y=INS / smooth=0.75 interpolation=cubic clm;
lineparm x=0 y=0 slope=1 / lineattrs=(color=grey pattern=dash);
run;

*4. Check interactions between some of your most important main effects or any 
that you suspect would be plausible and see how useful they are according to either 
AIC or the LRT. Or, if your model already has interaction terms, use AIC or the LRT 
to simultaneously test all terms involving the variable which appears most frequently 
in your model. In either case, don’t go beyond two-way interactions.; 

*Were there any plausible interactions (dependencies) that were seen in initial investigation?;

*5. Check for influential observations to understand why they 
are influential (if any are). Whether or not to change your model 
for them is up to you. 

*leverage is how much they influence; 
*cooks distance is the lower right hand;
proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=influence;
	model INS(event='1') = &FinalModVar;
run;  

*handle outliers before we run the model; 
*there are a lot of leverage points and outliers in cook's distance; 

*cook's d; 
proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=dpc;
	model INS(event='1') = &FinalModVar;
run;

*dfbetas;
proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=dfbetas;
	model INS(event='1') = &FinalModVar;
run;


