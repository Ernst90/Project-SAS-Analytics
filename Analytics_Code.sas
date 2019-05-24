libname mylib "P:\SCIENG\MATHS\DATA\SAS Course\Analytics\Assessment";
libname outlib "H:\SAS_Assessment"; 

proc copy in=outlib out=work;
run;

*PART ONE: Power Lifting *;

* a) *;
data Powerlifting;
set Powerlifting; 
TotalKg = BestSquatKg + BestBenchKg + BestDeadliftKg;
run;

proc univariate data=Powerlifting plots;
var TotalKg;
histogram;
run;

* b) *;
proc ttest data = Powerlifting H0 = 400 plots(shownull)= interval;
	var TotalKg;
run;

* c) *;
proc ttest data = Powerlifting plots(shownull) = interval;
	class sex;
	var TotalKg;
run;

* d) *;
* Outcome of two sample t-test: Since, the 95% CI for the mean difference in total lift weight between females and males is wholly negative 
  and additionally we have p-value = 0.0001 < 0.05, we reject H0.;

* e) *;
* Assumptions - Carefully explain*;

* f) *;
proc glm data = Powerlifting plots=diagnostics;
class Equipment;
model age = Equipment;
run;

proc glm data = Powerlifting plots=diagnostics;
class Equipment;
model age = Equipment;
means Equipment / hovtest = levene;
run;

proc glm data = Powerlifting plots=diagnostics;
class Equipment;
model age = Equipment;
means Equipment / welch;
run;
 
* g) *;
proc glm data=Powerlifting plots = diagnostics;
class Equipment;
model age = Equipment;
lsmeans Equipment / pdiff=all adjust = tukey;
run;

* h* model does not seem to fit since R-squared and Adjusted is very low, evidence of faning, non constant variance;
proc reg data = Powerlifting;
model Wilks = age;
run;

*i*;
data Powerlifting;
set Powerlifting;
if Place not in (1 2 3) then
Place2 = "Other";
else
Place2 = Place;
run;

*j*;
proc glm data = Powerlifting plots(only)= diagnostics;
class Sex Equipment Schedule Place2;
model Wilks = Sex Equipment LiquidConsumed Schedule Place2 GymCost Age BestSquatKg BestBenchKg BestDeadLiftKg AverageTime / solution clparm;
run;

*k* ;
proc glmselect data = Powerlifting;
class Sex Equipment Schedule Place2;
model Wilks = Sex Equipment LiquidConsumed Schedule Place2 GymCost Age BestSquatKg BestBenchKg BestDeadLiftKg AverageTime / selection = backward select=SBC showpvalues;
run;

*l*;
proc glmselect data = Powerlifting plots=all;
class Sex Equipment Schedule Place2;
model Wilks = Sex Equipment LiquidConsumed Schedule Place2 GymCost Age BestSquatKg BestBenchKg BestDeadLiftKg AverageTime / selection = backward select=SBC showpvalues;
run;

* PART TWO: Orthopaedics *;

*m)*;
proc glm data = outlib.Orthopaedics plots=diagnostics;
class class;
model degree_spondylolisthesis = class;
run;

proc ttest data = outlib.Orthopaedics plots(shownull) = interval;
	class class;
	var degree_spondylolisthesis;
run;

*n)*;
proc surveyselect noprint data=outlib.Orthopaedics samprate=0.75 outall out = Orthopaedics;
strata class;
run;

data train(drop = selected SelectionProb SamplingWeight) test(drop=selected SelectionProb SamplingWeight);
set Orthopaedics;
if selected then output train;
else output test;
run;

*o)*;
proc logistic data = Train plots(only)=(effect oddsratio) descending;
	model class = degree_spondylolisthesis pelvic_incidence pelvic_tilt_numeric lumbar_lordosis_angle sacral_slope pelvic_radius  ;
run;

proc logistic data = Train plots(only)=(effect oddsratio);
class class(ref='Normal');
model class = pelvic_incidence pelvic_tilt_numeric lumbar_lordosis_angle sacral_slope pelvic_radius / clodds = pl;
run;

*p)*;
proc logistic data = train;
	class class (ref = "Normal");
	model class = pelvic_incidence pelvic_tilt_numeric lumbar_lordosis_angle sacral_slope pelvic_radius degree_spondylolisthesis;
	score data = test out = testAssess (rename = (P_Normal = p_complex)) outroc = roc;
run;

proc logistic data = train;
	class class (ref = "Normal");
	model class = pelvic_incidence pelvic_tilt_numeric lumbar_lordosis_angle sacral_slope pelvic_radius;
	score data = testAssess out = testAssess (rename = (P_Normal = p_simple)) outroc = roc;
run;

proc logistic data = testAssess;
	model class = p_complex p_simple / nofit;
	roc "complex Model" p_complex;
	roc "simple Model" p_simple;
	roccontrast "comparing Models";
run;



