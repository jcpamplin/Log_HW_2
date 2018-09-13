/*--------------------------------*/
/*  MSA 2019: Logistic Regression */
/*           Homework 2           */
/*                                */
/*         Homework Team 9        */
/*--------------------------------*/

libname logistic 'C:\Users\14lmu\OneDrive\Documents\LogisticRegressionData\MSA2019LogisticData\data';
run;

*1. Revise the model that you built in your previous report. 
As before, you are free to do so in any way youÅfd like?this includes 
transforming/combining predictors in some sensible way if you can think of one. 
This can also include making a completely new model if you hate your current one.;

%let FinalModVar =  CC CD DDA DEP INV IRA MM MTG TELLER SAVBAl PHONE ATMAMT;
/*Final Model*/
proc logistic data=logistic.insurance_t;
	model INS(event='1') = &FinalModVar;
run;

*Please review:
*What is the reasoning behind choosing each variable that was included? 
*Was multicollinearity tested? We tested on the model, but we recommend testing all the
pairs of categorical/continous variables; 
proc reg data=logistic.insurance_t; 
model INS = &FinalModVar / tol vif ;
run;
*Did you test transformations? (Check page 2 comments from Logistic Regression HW 1); 

*Notes for final report (from Deb and Lauren):
*Remember to mention in report  - sample size 
*Reviewing Matt's comments in general and application;

*2. Make sure to check for separation and adjust accordingly. 
This may or may not change your model.;  

*Checking the model for separation requires looking at the pvalues for the estimates
*Are they close to 1? No
*Are the errors very high? No 
*page 47-49 provide a more nontechnical description;

*3. Check the linearity assumption for any continuous variables in your model
that you suspect might have a nonlinear effect. Make any corrections necessary. 
DonÅft forget model hierarchy.;  

*partial residuals of all of our continuous predictors - SAVBAL ATMAMT; 

proc logistic data=logistic.insurance_t;
	model INS(event='1') = &FinalModVar;
	output out=predicted reschi=respearson pred=phat predprobs=x;
run;

data predicted;
set predicted;
 working = (INS - phat)/(phat*(1 - phat));
 respart_SAVBAL = 0.000045*SAVBAL + working;
 respart_ATMAMT = 0.000084*ATMAMT + working;
 run;

 *partial residual plot for SAVBAL; 
 proc sgplot data=predicted;
 scatter x=SAVBAL y=respart_SAVBAL;
 loess x=SAVBAL y=respart_SAVBAL / clm;
 reg x=SAVBAL y=respart_SAVBAL / nomarkers;
 run;  

*partial residual plot for ATMAMT; 
 proc sgplot data=predicted;
 scatter x=ATMAMT y=respart_ATMAMT;
 loess x=ATMAMT y=respart_ATMAMT / clm;
 reg x=ATMAMT y=respart_ATMAMT / nomarkers;
 run; 

 *why is this not working for either? 
 *We hypothesize that it has to do with the estimates, which are almost zero for both, 
 because so much of the data for variables is zero;

*fitting the additive model;  
*Not sure if we need this with these continuous variables, because higher order terms 
 are not necessary; 
*Again, there are issues with parameter estimates close to zero; 

 proc gam data=logistic.insurance_t plots=components(clm additive commonaxes);
	model INS(event='1') = param(&FinalModVar) / dist=binomial link=logit;
run; 

*calibration curve; 

proc sgplot data=predicted;
loess x=phat y=INS / smooth=0.75 interpolation=cubic clm;
lineparm x=0 y=0 slope=1 / lineattrs=(color=grey pattern=dash);
run;

*4. Check interactions between some of your most important main effects or any 
that you suspect would be plausible and see how useful they are according to either 
AIC or the LRT. Or, if your model already has interaction terms, use AIC or the LRT 
to simultaneously test all terms involving the variable which appears most frequently 
in your model. In either case, donÅft go beyond two-way interactions.; 

*Did you check for this? After revising the model again, 
it might be a good idea to do this again.;

*5. Check for influential observations to understand why they 
are influential (if any are). Whether or not to change your model 
for them is up to you. 

*leverage is how much they influence; 
*cooks distance is the lower right hand;
proc logistic data=logistic.insurance_t plots(MAXPOINTS=NONE only label)=influence;
	model INS(event='1') = &FinalModVar;
run;  

*handle outliers before we run the model; 
*there are a lot of leverage points and outliers in cook's distance; 

*cook's d; 
proc logistic data=logistic.insurance_t plots(MAXPOINTS=NONE only label)=dpc;
	model INS(event='1') = &FinalModVar;
run;

*dfbetas;
proc logistic data=logistic.insurance_t plots(MAXPOINTS=NONE only label)=dfbetas;
	model INS(event='1') = &FinalModVar;
run;

*Something that we are noticing with all of these plots is that standardization of the data
is necessary. This is especially evident in dfbetas, where all of the variables are 
clustered around 0.
