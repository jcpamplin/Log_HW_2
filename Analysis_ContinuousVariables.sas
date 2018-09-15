proc univariate data=logistic.insurance_t;
	var savbal;
	id ins;
run;/*Skewness 18.4175272 Kurtosis 616.603326 */

proc means data=logistic.insurance_t nmiss;
run;/*0 miss*/

proc sql;
select count(*) from logistic.insurance_t where savbal = 0;
quit;/* > 4000 having zero values */

proc sgplot data=logistic.insurance_t;
histogram savbal;
density savbal;
run; /*shows skewness in diagram */

proc sgplot data=logistic.insurance_t;
reg x= savbal y=ins;
run;/* lineplot to check relation */

/* transforming the savbal column by log */
data logistic.insurance_modified;
set logistic.insurance_t;
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

proc univariate data=logistic.insurance_t;
	var atmamt;
	id ins;
run;/*Skewness 13.8475405 Kurtosis 301.309234*/

proc means data=logistic.insurance_t nmiss;
run;/*0 miss*/

proc sql;
select count(*) from logistic.insurance_t where atmamt = 0;
quit;/* > 3629 having zero values */

proc sgplot data=logistic.insurance_t;
histogram atmamt;
density atmamt;
run; /*shows skewness in diagram */

proc sgplot data=logistic.insurance_t;
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
-2 Log L 9668.535 8320.084*/


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
	model INS(event='1') = CC CD DDA DEP INV IRA MM TELLER modified_savbal PHONE;
run;
/*
output
---------
Model Fit Statistics 
Criterion Intercept Only Intercept and Covariates 
AIC 		9670.535 		8349.947  <--------- AIC increased by 3 only
SC 			9677.447 		8425.978 
-2 Log L 	9668.535 		8327.947 

*/

/* testing if coefficients improve*/

%let FinalModVar =  CC CD DDA DEP INV IRA MM TELLER modified_savbal PHONE;
proc logistic data=logistic.insurance_modified;
	model INS(event='1') = &FinalModVar;
	output out=predicted reschi=respearson pred=phat predprobs=x;
run;

/* coefficients improved than before --> modified_savbal = 0.1476*/
data predicted;
set predicted;
 working = (INS - phat)/(phat*(1 - phat));
 respart_SAVBAL = 0.1476*MODIFIED_SAVBAL + working;
 run;

 /* checking partial residual*/

 /*this whill take a lot of time*/
ODS GRAPHICS on/LOESSMAXOBS=10000;/* for getting CI for data points > 5000*/
 proc sgplot data=predicted;
 scatter x=MODIFIED_SAVBAL y=respart_SAVBAL;
 loess x=MODIFIED_SAVBAL y=respart_SAVBAL / clm;
 reg x=MODIFIED_SAVBAL y=respart_SAVBAL / nomarkers;
 run;  

 /*fitting additive model - cannot interpret it - coeffs look good*/
proc gam data=logistic.insurance_modified plots=components(clm additive commonaxes);
	model INS(event='1') = param(&FinalModVar) / dist=binomial link=logit;
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

