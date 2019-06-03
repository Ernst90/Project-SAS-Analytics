
libname mylib "P:\SCIENG\MATHS\DATA\SAS Course\Analytics\Assessment";
libname outlib "H:\SAS_Assessment"; 

proc copy in = outlib out = work;
run;



/* EXPLANATORY ANALYSIS */

proc corr data=weather plots=all;
	var MinTemp MaxTemp;
run;

proc sgplot data=weather noautolegend;
  title "Relationship between MaxTemp and MinTemp variables";
  scatter x=MinTemp y=MaxTemp;
  lineparm x=0 y=0 slope=1; 
  xaxis grid; yaxis grid;
run;

ods noproctitle;
title "Summary Statistics for the variable WindGustDir";
Proc freq data=weather nlevels;
	table WindGustDir;
run;
ods proctitle;

%let categories = Location NewEquipment WindGustDir WindDir9am WindDir3pm Status RainToday Cloud3pm Cloud9am;

/* Distribution of missing data */
*https://stats.idre.ucla.edu/wp-content/uploads/2017/01/Missing-Data-Techniques_UCLA.pdf;

ods trace on;
ods graphics on;

title "Missing Values for MaxTemp and MinTemp variables";
proc means data = weather N Nmiss;
	var MaxTemp MinTemp;
run;
ods trace off;

proc sort data=weather out=weather;
	by WindGustDir;
Run;

proc freq data=weather order=data;
   by WindGustDir;
   tables Evaporation;
   title 'Frequences of Evaporation/Wind Gust Direction';
run;

*Checking the number of levels for each categorical variable;
ods noproctitle;
title "Categorical Variables";
proc freq data = weather nlevels;
 	tables &categories;
run;
ods proctitle;

proc univariate data=weather;
run;


/*Performing listwise deletion to see total of complete cases*/
*https://blogs.sas.com/content/iml/2015/02/23/complete-cases.html;

data weather_complete;
  set weather;
  if cmiss(of _ALL_)=0;  /* complete cases for all vars */
run;

proc means data = weather_complete N min max mean std maxdec=2;
	var _numeric_ ;
run;

proc contents data = weather;
run;

proc univariate data = weather plots mu0 = 23;
	histogram MinTemp / nmidpoints = .35;
run;

proc sgplot data=weather;
	histogram MaxTemp;
	yaxis label = 'Frequency' grid values = (0 TO 12 BY 2); 
	xaxis label = 'Maximum Temperature (Degrees Celcius)';
	title bold "Maximum Temperature by Degrees Celcius"; 
run;

%let continuos = MinTemp MaxTemp Pressure9am  Pressure3pm Temp9am Temp3pm;
proc sgscatter data=weather;
  title "Scatter Plot Matrix for continuos variables";
run;
title;

%let variables of interest = MinTemp MaxTemp Evaporation;
proc sgscatter data=weather;
  title "Scatter Plot Matrix for continuos variables";
  matrix &variables of interest;
run;
title;


/* FORMAL ANALYSIS 

Question A 
A temperature for this country of 25.C or above can alert
officials to the possibility of a drought. Is the mean maximum temperature
different to this? 
*/

*plot more statistics;
ods proctitle;
proc univariate data = weather plots all mu0 = 25;
	var MinTemp;
run;

*check for normality;
proc univariate data = weather normal; 
	qqplot MaxTemp / Normal(mu=est sigma=est color=red l=1);
run;

proc univariate data = weather normal; 
	qqplot MinTemp / Normal(mu=est sigma=est color=red l=1);
run;

*t-tests;
proc ttest data = weather H0 = 25 plots(shownull) = interval;
	var MaxTemp;
run;

proc ttest data = weather H0 = 25 plots(shownull) = all;
	var MaxTemp;
run;

proc ttest data = weather H0 = 25 plots(shownull) = interval;
	var MaxTemp;
run;


/* 
Question B 
Investigate whether there is a difference between the mean maximum
temperature and the mean minimum temperature. 
*/

proc ttest data = weather plots(shownull) = all;
	paired MaxTemp * MinTemp;
run;

/* ASSMUPTIONS (check them also and justify in each step of Exploratory Data Analysis)
- The dependent variable must be continuous (interval/ratio).
- The observations are independent of one another.
- The dependent variable should be approximately normally distributed.
- The dependent variable should not contain any outliers.s
*/


/*Question C
It is quite possible that evaporation is associated with the
strongest wind gust direction for a day. Check whether there is a difference in
the mean evaporation between the different strongest wind gust directions using
an appropriate multiple comparison adjustment. 
*/

* We first produce diagnostic plots to check normality assumption;
proc glm data = weather plots = all;
	class WindGustDir;
	model Evaporation = WindGustDir;
	means WindGustDir;
run;

* Then we perform a Levene's test to check equal variances assumption;
proc glm data = weather plots = diagnostics;
	class WindGustDir;
	model Evaporation = WindGustDir;
	means WindGustDir / hovtest=levene;
run;

* We perform post-hoc analysis without asjusted p-values first;
proc glm data = weather plots(only)=(diffplot(center)); 
	class WindGustDir;
	model Evaporation = WindGustDir;
	lsmeans WindGustDir / pdiff = all adjust = T;
run; 

* Post-hoc using Tukey method;
proc glm data = weather plots(only)=(diffplot(center)); 
	class WindGustDir;
	model Evaporation = WindGustDir;
	lsmeans WindGustDir / pdiff = all adjust = tukey;
run;

proc anova data = weather;
	class WindGustDir;
	model Evaporation = WindGustDir;
	means WindGustDir / tukey;
run;


/* Question D 
Find the “best” model (as determined by an automated model
selection procedure), using evaporation as the response and all other variables
in the dataset as potential predictors (do not consider any interaction terms,
transformations or higher order  terms, and do not include the variable
“RainTomorrow” in the modelling process).
When you come to fit a model for this question of interest, use two different
directional automated procedures (fit two different models considering the same
proposed predictors, using different directional selection approaches) and
examine whether they result in the same or different final models. If they are
different, which do you prefer? Justify your answer. */

%let variables = MinTemp MaxTemp Rainfall Sunshine WindGustSpeed WindSpeed9am WindSpeed3pm Humidity9am Humidity3pm Pressure9am  Pressure3pm Temp9am Temp3pm;
%let categories = Location NewEquipment WindGustDir WindDir9am WindDir3pm Status RainToday Cloud3pm Cloud9am;

proc corr data = weather;
	var &variables;
run;

%let variables = MinTemp MaxTemp Rainfall Sunshine WindGustSpeed WindSpeed9am WindSpeed3pm Humidity9am Humidity3pm Pressure9am Pressure3pm;
%let categories = Location NewEquipment WindGustDir WindDir9am WindDir3pm Status RainToday Cloud3pm Cloud9am;

proc glmselect data = weather plots=all;
	class &categories;
	model Evaporation = &variables &categories / selection = stepwise select=SBC showpvalues details=all stats=all stop=sbc;
run;

proc glmselect data = weather plots=all;
	class &categories;
	model Evaporation = &variables &categories / selection = forward select=SBC showpvalues details=all stats=all stop=sbc;
run;


/* Question E 
Since the variables “WindGustDir”, “WindDir9am” and “WindDir3pm”
have many levels, it is of interest to see whether some levels could be
combined. Perform Greenacre’s method, with the variable “RainTomorrow” as the
response, for each of these variables in turn. If, for any of the above
variables, Greenacre’s method suggests combining levels is reasonable, combine
the appropriate levels and use this(these) new variable(s) in any subsequent
analysis (give any new variables new names). 
*
* https://vimeo.com/301775681
* http://www.econ.upf.edu/~michael/stanford/maeb7.pdf */

%macro cluster_data(var);
 	data weather1;
		set weather;
		if RainTomorrow = "No" then RainTomorrowBin = 0;
		else RainTomorrowBin = 1;
	run;

	proc means data=weather1 noprint nway;
		class &var;
		var RainTomorrowBin;
		output out=greenacre01 mean=prop;
	run;

	ods output clusterhistory=weather1;

	proc cluster data=greenacre01 method=ward 
		plots=(dendrogram(vertical height=rsq));
		freq _freq_;
		var prop;
		id &var;
	run;
%mend;

%cluster_data(WindGustDir);
%cluster_data(WindDir9am);
%cluster_data(WindDir3pm);

data work.weather_combined;
	set weather;
	
	if WindGustDir in ("E" "NNE" "ESE" "SW" "NE" "ENE")
		then WindGustDirCombined = "Cluster1";
	else if WindGustDir in ("S" "SE" "SSE")
		then WindGustDirCombined = "Cluster2";
	else if WindGustDir ne ' ' then WindGustDirCombined = "Cluster3"; 
	
	if WindDir9am in ("E" "ENE" "SE" "ESE")
		then WindDir9amCombined = "Cluster1";
	else if WindDir9am in ("N" "NNW" "WNW")
		then WindDir9amCombined = "Cluster2";
	else if WindDir9am ne ' ' then WindDir9amCombined = "Cluster3";
	
	if WindDir3pm in ("E" "ENE" "NE" "SE" "S" "SSW" "WSW")
		then WindDir3pmCombined = "Cluster1";
	else if WindDir3pm in ("ESE" "NNE" "SSE" "SW")
		then WindDir3pmCombined = "Cluster2";
	else if WindDir3pm ne ' ' then WindDir3pmCombined = "Cluster3";
run;

proc freq data = weather_combined;
	table WindGustDirCombined;
run;


/* Question F 
Split your data for model assessment into training data (75% of
original dataset), and test data (25% of original dataset), ensuring you have a
roughly equal proportion of Yes to No responses for the variable “RainTomorrow”
in each dataset. In addition to output and discussion, include your code in your
report for this question of Interest. 
*/
data weather1;
	set work.weather_combined;
run;

proc sort data=weather1 out=work.weather_sort;
	by RainTomorrow;
run;

proc surveyselect noprint data=weather_sort samprate=.75 outall out=weather_sampling;
	strata RainTomorrow;
run;

data weather_training(drop=selected SelectionProb SamplingWeight) 
	 weather_test(drop=selected SelectionProb SamplingWeight);
	set work.weather_sampling;
	if selected then output weather_training;
	else output weather_test;
run;

*76.90 percentage of NO;
proc freq data=work.weather_training;
   tables RainTomorrow;
run;

*76.96 percentage of NO;
proc freq data=work.weather_test;
   tables RainTomorrow;
run;


/* Question G 
Using Hoeffding’s and Spearman’s statistics, check whether there
is any evidence of non-linearity and/or irrelevant variables for the continuous
variables in your dataset (use the training data).*/

%let variables = MinTemp MaxTemp Rainfall Sunshine WindGustSpeed WindSpeed9am WindSpeed3pm Humidity9am Humidity3pm Pressure9am Pressure3pm Temp9am Temp3pm;

ods output spearmancorr=work.spearman hoeffdingcorr=work.hoeffding;

proc corr data=work.weather_training spearman hoeffding;
	var Evaporation;
	with &variables;
run;

proc sort data=work.spearman;
	by variable;
run;

proc sort data=work.hoeffding;
	by variable;
run;

data work.coefficients;
	merge work.spearman(rename=(Evaporation=scoef pEvaporation=spvalue))
		work.hoeffding(rename=(Evaporation=hcoef pEvaporation=hpvalue));
	by variable;
	scoef_abs=abs(scoef);
	hcoef_abs=abs(hcoef);
run;

proc rank data=work.coefficients out= work.coefficients_rank;
	var scoef_abs hcoef_abs;
	ranks ranksp rankho;
run;

proc print data=work.coefficients_rank;
	var variable ranksp rankho scoef spvalue hcoef hpvalue;
run;

proc sgplot data=work.coefficients_rank;
	scatter y=ranksp x=rankho / datalabel=variable;
run;


/* Question H 
Find the “best” model (as determined by an automated model selection procedure), 
using the variable “RainTomorrow” as the response and all other variables in the 
dataset as potential predictors. Use the training data and do not consider any 
interaction terms, transformations or higher order terms. */

%let variables = MinTemp MaxTemp Rainfall Sunshine WindGustSpeed WindSpeed9am WindSpeed3pm Humidity9am Humidity3pm Pressure9am  Pressure3pm Evaporation;
%let categories = Location NewEquipment WindGustDirCombined WindDir9amCombined WindDir3pmCombined Status RainToday Cloud3pm Cloud9am;

data work.weather_training;
	set work.weather_training;
	if RainTomorrow = "No" then RainTomorrowBin = 0;
	else RainTomorrowBin = 1;
run;

*0.1 for backwards, 0.5 for forward, 0.15  for stepwise model selection;
*slstay, slentry;

*0.1 for backwards, 0.5 for forward, 0.15  for stepwise model selection;
*slstay, slentry;

ods select all;
proc logistic data=work.weather_training plots=all;
	class &categories;
	model RainTomorrowBin(event='1') = &variables &categories / clodds=pl  selection=forward SLENTRY=0.05;
run;
ods select all;


/* Question I 
It is of interest to see whether a specific subset of the
variables can be used to make accurate predictions about whether it will rain
tomorrow. Consider the model that only uses the variables “RainToday” and
“MaxTemp” as predictors. Investigate how the level of predictive performance to
the test data differs using this model to that of your final model from part h).
In addition to output and discussion, include your code in your report for this
question of interest. */

data work.weather_training;
	set work.weather_training;
	if RainTomorrow = "No" then RainTomorrowBin = 0;   
	else RainTomorrowBin = 1;
run;

data work.weather_test;
	set work.weather_test;
	if RainTomorrow = "No" then RainTomorrowBin = 0;
	else RainTomorrowBin = 1;
run;

proc logistic data=work.weather_training;
	class RainToday  ;
	model RainTomorrowBin(event='1')=Sunshine WindGustSpeed WindSpeed3pm Humidity3pm Pressure9am Pressure3pm;
	score data=work.weather_test out=testAssess(rename=(p_1=p_complex)) outroc=work.roc;
run;

proc logistic data=work.weather_training;
	class RainToday;
	model RainTomorrowBin(event='1')=RainToday MaxTemp;
	score data=work.testAssess out=testAssess(rename=(p_1=p_simple)) outroc=work.roc;
run;

proc logistic data=work.testAssess;
	model RainTomorrowBin(event='1')=p_complex p_simple/nofit;
	roc "Optimal Model (8 pred.)" p_complex;
	roc "Simple Model (2 pred.)" p_simple;
	roccontrast "Model Comparison";
run;

