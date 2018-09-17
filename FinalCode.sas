/* Imputing missing variables*/ 
*creates new dataset;
data work.develop;
	set logistic.insurance_t;
run;
*finds missing variables;
proc means data=develop n nmiss mean median min max;
	var CC CD DDA DEP INV IRA MM MTG TELLER SAVBAl PHONE ATMAMT;
run;
*creates new table with only variables of intrest;
proc sql;
	create table work.selected_var as 
	select CC, CD, DDA, DEP, INV, IRA, MM, MTG, TELLER, SAVBAl, PHONE, ATMAMT, INS
	from work.develop;
quit;
*prints sample of dataset;
proc print data = selected_var (obs = 15);
run;
*finds all missing values and creates column to tell if missing;
data work.train_mi(drop=i);
	set work.selected_var;
	array mi{*} MICC MIINV MIPHONE;
	array x{*} CC INV PHONE;
	do i=1 to dim(mi);
		mi{i} = (x{i}=.);
		nummiss+mi{1};
	end;
run;
*imputes missing values with the median of that value;
proc stdize data=work.train_mi reponly method=median out=train_imputed;
	var CC CD DDA DEP INV IRA MM MTG TELLER SAVBAl PHONE ATMAMT;
run; 
*prints new dataset;
proc print data = train_imputed (obs = 15);
run;
*checks new dataset for missing values;
proc means data=train_imputed n nmiss mean median min max;
	var _ALL_;
run;

data logistic.train_imputed(drop=nummiss);
set logistic.train_imputed;
run;

/*******************************************************************************************/
proc univariate data=logistic.train_imputed;
	var savbal;
	id ins;
run;/*Skewness 18.4175272 Kurtosis 616.603326 */

proc means data=logistic.train_imputed nmiss;
run;/*0 miss*/

proc sql;
select count(*) from logistic.train_imputed where savbal = 0;
quit;/* > 4000 having zero values */

proc sgplot data=logistic.train_imputed;
histogram savbal;
density savbal;
run; /*shows skewness in diagram */

proc sgplot data=logistic.train_imputed;
reg x= savbal y=ins;
run;/* lineplot to check relation */

/* transforming the savbal column by log */
data logistic.insurance_modified;
set logistic.train_imputed;
if savbal <> 0 then modified_savbal = log(savbal);
else modified_savbal = savbal;
run;

/* skewness is negligible, but there are a lot of zeroes, hence a spike in the plot*/
proc sgplot data=logistic.insurance_modified;
histogram modified_savbal;
density modified_savbal;
run;

proc univariate data=logistic.insurance_modified;
	var modified_savbal;
	id ins;
run;

/**************ATMAMT************/

proc univariate data=logistic.train_imputed;
	var atmamt;
	id ins;
run;/*Skewness 13.8475405 Kurtosis 301.309234*/

proc means data=logistic.train_imputed nmiss;
run;/*0 miss*/

proc sql;
select count(*) from logistic.train_imputed where atmamt = 0;
quit;/* > 3629 having zero values */

proc sgplot data=logistic.train_imputed;
histogram atmamt;
density atmamt;
run; /*shows skewness in diagram */

proc sgplot data=logistic.train_imputed;
reg x= atmamt y=ins;
run;/* lineplot to check relation */

/* transforming the savbal column by log */
data logistic.insurance_modified;
set logistic.insurance_modified;
if atmamt <> 0 then modified_atmamt = log(atmamt);
else modified_atmamt = atmamt;
run;

/* skewness is negligible, but there are a lot of zeroes, hence a spike in the plot*/
proc sgplot data=logistic.insurance_modified;
histogram modified_atmamt;
density modified_atmamt;
run;

proc univariate data=logistic.insurance_modified;
	var modified_atmamt;
	id ins;
run; /*Skewness 0.00285701 Kurtosis -1.7069307 */

%let FinalModVar =  CC CD DDA DEP INV IRA MM MTG TELLER modified_savbal PHONE modified_atmamt;
/*Final Model*/
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar;
run;

/* old model stats
AIC 9670.535 8527.198 
SC 9677.447 8617.053 
-2 Log L 9668.535 8501.198 

new model stats
AIC 9670.535 8346.084 
SC 9677.447 8435.939 
-2 Log L 9668.535 8320.084

after considering missing values

new variables
Model Fit Statistics 
Criterion Intercept Only Intercept and
Covariates 
AIC 10932.130 9482.438 
SC 10939.178 9574.052 
-2 Log L 10930.130 9456.438 

old model
Model Fit Statistics 
Criterion Intercept Only Intercept and
Covariates 
AIC 10932.130 9665.877 
SC 10939.178 9757.491 
-2 Log L 10930.130 9639.877 

*/



%let FinalModVar =  CC CD DDA DEP INV IRA MM MTG TELLER savbal PHONE atmamt;
/*Final Model*/
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar;
run;

/* on running this model the p-value of the following 2 variables became close to non-significant boundary:
modified_atmamt 0.0485
MTG 0.0423 */

/* let's see what we get without these 2 variables now*/
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = CC CD DDA DEP INV IRA MM MTG TELLER modified_savbal PHONE;
run;
/*
output
---------
Model Fit Statistics 
Criterion Intercept Only Intercept and
Covariates 
AIC 10932.130 9490.133 
SC 10939.178 9567.652 
-2 Log L 10930.130 9468.133 

*/

 /* checking partial residual*/


%let FinalModVar =  CC CD DDA DEP INV IRA MM MTG TELLER modified_savbal PHONE;
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar;
	output out=predicted reschi=respearson pred=phat predprobs=x;
run;

/* coefficients improved than before --> modified_savbal = 0.1476*/
data predicted;
set predicted;
 working = (INS - phat)/(phat*(1 - phat));
 respart_SAVBAL = 0.1427*MODIFIED_SAVBAL + working;
 respart_DEP = -0.1059*DEP + working;
 respart_TELLER = 0.0886*TELLER + working;
 respart_PHONE = -0.0882*PHONE + working;
 run;

 /*this will take a lot of time*/
ODS GRAPHICS on/LOESSMAXOBS=10000;/* for getting CI for data points > 5000*/
 proc sgplot data=predicted;
 scatter x=MODIFIED_SAVBAL y=respart_SAVBAL;
 loess x=MODIFIED_SAVBAL y=respart_SAVBAL / clm;
 reg x=MODIFIED_SAVBAL y=respart_SAVBAL / nomarkers;
 run;

proc sgplot data=predicted;
 scatter x=DEP y=respart_DEP;
 loess x=DEP y=respart_DEP / clm;
 reg x=DEP y=respart_DEP / nomarkers;
 run;
/**/
/*proc sgplot data=predicted;*/
/* scatter x=TELLER y=respart_TELLER;*/
/* loess x=TELLER y=respart_TELLER / clm;*/
/* reg x=TELLER y=respart_TELLER / nomarkers;*/
/* run;*/

proc sgplot data=predicted;
 scatter x=PHONE y=respart_PHONE;
 loess x=PHONE y=respart_PHONE / clm;
 reg x=PHONE y=respart_PHONE / nomarkers;
 run; 

 /*fitting additive model - cannot interpret it - coeffs look good*/
proc gam data=logistic.insurance_modified plots=components(clm additive commonaxes);
	model INS(event='1') = param(&FinalModVar) / dist=binomial link=logit;
run; 


/*proc gam data=logistic.insurance_modified plots=components(clm additive commonaxes);
	model INS(event='1') = param(&FinalModVar) spline(phone, df=4)/ dist=binomial link=logit;
run; */
/*corrected code for fitting additive models*/
%let SplineVar =  CC CD DDA DEP INV IRA MM MTG TELLER modified_savbal;
proc gam data=logistic.insurance_modified plots=components(clm additive commonaxes);
	model INS(event='1') = param(&SplineVar) spline(phone, df=4)/ dist=binomial link=logit;
run; 

%let SplineVar =  CC CD DDA INV IRA MM MTG TELLER modified_savbal PHONE;
proc gam data=logistic.insurance_modified plots=components(clm additive commonaxes);
	model INS(event='1') = param(&SplineVar) spline(dep, df=4)/ dist=binomial link=logit;
run; 
/* calibration curve - looking great, but again it will take time to run due to the loess thing*/
proc sgplot data=predicted;
loess x=phat y=INS / smooth=0.75 interpolation=cubic clm;
lineparm x=0 y=0 slope=1 / lineattrs=(color=grey pattern=dash);
run;

/*leverage points check */
proc sort data=logistic.insurance_modified;
by INS;
run;
proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=influence;
	model INS(event='1') = &FinalModVar;
run;

proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=dpc;
	model INS(event='1') = &FinalModVar;
run;


proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=dfbetas;
	model INS(event='1') = &FinalModVar;
run;



data logistic.insurance_v_mod;
set logistic.insurance_v;
if savbal <> 0 then modified_savbal = log(savbal);
else modified_savbal = savbal;
run;


/* fitting */
/* roc curve brier score c stat */
proc logistic data=logistic.insurance_modified plots(only)=ROC(id = prob);
	model INS(event='1') = &FinalModVar / rocci;
	score data=logistic.insurance_v_mod fitstat;
run;


/* distribution of predicted probabilities */
proc logistic data=logistic.insurance_modified noprint;
	model INS(event='1') = &FinalModVar;
	/* output predicted probabilities */
	output out=predprobs p=phat;
run;
/* graphics -- sorting by low to get mean(1s) - mean(0s) in the next step.
that's the coefficient of discrimination */
proc sort data=predprobs;
by descending low;
run;

/* proc ttest will give the coefficient of discrimination.
also gives a nice histogram (with density overlaid)
and boxplots for each group */
proc ttest data=predprobs order=data;
ods select statistics summarypanel;
class ins;
var phat;
title 'coefficient of discrimination and plots';
run;

/* classification table */
/* NOTE that SAS does leave-one-out when computing predicted probabilities,
so the table results (and youden index) are different than what I have in R */
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar / ctable pprob = 0 to 0.98 by 0.02;
	/* output table */
	ods output classification=classtable;
	title 'classification table';
	/* in this table:
	true positives = "number of correct events"
	true negatives = "number of correct nonevents"
	false positive = "number of incorrect events"
	false negative = "number of incorrect nonevents" */
run;



/* Youden's J statistic */
data classtable;
set classtable;
/* using 100 because sas gives these in percentages */
youden = sensitivity + specificity - 100;
/* weighted youden would be 2*(w*sens + (1-w)*spec) - 100, where w is between 0 and 100 */
run;
proc sort data=classtable;
by descending youden;
run;
proc print data=classtable;
run;

