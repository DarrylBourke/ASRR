
************************************
* Preliminaries - Continuous 1 of 2*
************************************

* 1. Opening the dataset and preparing set more off command
cd n:/asrr_assignment
use exam_data_2018.dta
set more off


* 2. Installation of partpred for plotting purposes (Please install this before running code)
findit partpred


* 3. Desciption and summary of dataset
describe
summ
tabstat glucose*, by(mi_fchd) s(count mean sd)




***************************************************************
* Recoding variables with clinical significance and labelling *
***************************************************************

* 4. Calculating the age variable
gen age = (baseline_visit - dob)/365.25


* 5. Recoding age in to a categorical variable called agea
gen agea = age
summ agea
recode agea min/45=1 45/50=2 50/55=3 55/60=4 60/max=5
label define agea 1 "<45yrs" 2 "45/50yrs" 3 "50/55yrs" 4 "55/60yrs" 5 ">60yrs"
label value agea agea
tab agea


* 6. Recoding bmi in to a categorical variable called bmia
gen bmia = bmi
summ bmia
recode bmia min/18.5=1 18.5/24.9=2 24.9/29.9=3 29.9/39.9=4 39.9/max=5 . = 9999 
label define bmia 1 "underweight" 2 "healthy weight" 3 "overweight" 4 "obese" 5 "very obese" 9999 "missing"
label value bmia bmia
tab bmia


* 7. Recoding glucose in to a categorical variable called glucosea and glucosea1(to assess missing values)
gen glucosea = glucose
summ glucosea
recode glucosea min/70=1 70/100=2 100/125=3 125/max=4
label define glucosea 1 "<70mg/dl (low blood sugar (hypoglycemia))" 2 "70/100mg/dl (normal blood sugar)" 3 "101/125mg/dl (pre-diabetic blood sugar)" 4 ">126mg/dl (diabetic blood sugar)"
label value glucosea glucosea
tab glucosea

gen glucosea1 = glucose
recode glucosea1 min/70=1 70/100=2 100/125=3 125/max=4 . = 99999
label define glucosea1 1 "<70mg/dl (low blood sugar (hypoglycemia))" 2 "70/100mg/dl (normal blood sugar)" 3 "101/125mg/dl (pre-diabetic blood sugar)" 4 ">126mg/dl (diabetic blood sugar)" 99999 "missing"
label value glucosea1 glucosea1


* 8. Recoding glucose in to a categorical variable with two levels called glucosea2
gen glucosea2 = glucose
summ glucosea2
recode glucosea2 min/100=1 100/max=2 
label define glucosea2 1 "<100mg/dl (normal and low blood sugar)" 2 ">100mg/dl (pre-diabetic and diabetic blood sugar)" 
label value glucosea2 glucosea2
tab glucosea2


* 9. Recoding cigpday in to a categorical variable called cigpdaya
gen cigpdaya = cigpday
summ cigpdaya
recode cigpdaya min/10=1 10/20=2 20/40=3 40/max=4
label define cigpdaya 1 "<10 perday" 2 "10/20 per day" 3 "20/40 per day" 4 ">40 per day"
label value cigpdaya cigpdaya
tab cigpdaya


* 10. Labeling educ
label define educ 1 "0-11yrs" 2 "high school" 3 "some college" 4 "college grad or higher qual"
label value educ educ
tab educ

* 11. Recode last_followup, date_dth and date_mifchd to a new variable called enddate
gen enddate = last_followup
format enddate %td
replace enddate = date_dth if date_dth <= last_followup
replace enddate = date_mifchd if date_mifchd <= last_followup




*****************************
* Unadjusted model analysis *
*****************************

* 12. Unadjusted investigation of each variable for statistical significance and their ORs
logistic mi_fchd glucose
logistic mi_fchd sex
logistic mi_fchd bmi
logistic mi_fchd cursmoke
logistic mi_fchd cigpday
logistic mi_fchd educ
logistic mi_fchd prior_hyp
logistic mi_fchd death
logistic mi_fchd age

tab sex mi_fchd, row chi
tab cursmoke mi_fchd, row chi
tab prior_hyp mi_fchd, row chi
tab death mi_fchd, row chi


* 13. Unadjusted investigation of each categorical variable for statistical significance and their ORs
logistic mi_fchd i.glucosea1
logistic mi_fchd i.bmia
logistic mi_fchd i.cigpdaya
logistic mi_fchd i.educ
logistic mi_fchd i.agea

tab glucosea1 mi_fchd, row chi
tab bmia mi_fchd, row chi
tab cigpdaya mi_fchd, row chi
tab educ mi_fchd, row chi
tab agea mi_fchd, row chi


// Cox regression //

* 14. Cox regression of categorical glucose (four levels)
stset enddate, enter(baseline_visit) origin(dob) fail(mi_fchd) scale(365.25)
stcox ib2.glucosea, base


* 15. Checking proportional hazards assumptions (categorical (glucose))
stphplot, by(glucosea) legend(cols(1))
estat phtest, detail


* 16. Checking proportional hazards assumptions for the alternative categorical glucose variable (two levels)
stset enddate, enter(baseline_visit) origin(dob) fail(mi_fchd) scale(365.25)
stcox i.glucosea2, base
stphplot, by(glucosea2) legend(cols(1))
estat phtest, detail


// Quadratic (continuous) //

* 17. Plotting a quadratic expression of the assiocation between continuous glucose and adverse CVD outcomes
logistic mi_fchd c.glucose##c.glucose, base
predict yhat_quad
sort glucose


// Fractional polynomial (continuous) //

* 18. Fractional polynomial for averaging
fp <glucose> : logistic mi_fchd <glucose>
predict yhat_fp
gen temp1 = glucose^2
gen temp2 = glucose^3
logistic mi_fchd temp1 temp2


// Combined plots (continuous) //

* 19. Combined plot of quadratic expression, fractional polynomial, spline and lowess smoothing
twoway (scatter mi_fchd glucose)(line yhat_fp glucose)(line yhat_quad glucose)(lowess mi_fchd glucose, bwidth(0.1))




*******************************************
* Complete case analysis (adjusted model) *
*******************************************


// Continuous //

* 20. Complete case analysis of covariates (decided upon from the unadjusted model) using logistic regression and associated plot
logistic mi_fchd glucose i.agea sex i.bmia cursmoke i.cigpdaya i.educ prior_hyp, base

partpred or, for (glucose) ref(glucose 100) ci(or_cl or_cu) eform
twoway (rarea or_cl or_cu glucose, sort pstyle(ci)) (line or glucose, sort), legend(off) xtitle("glucose") ytitle("odds ratio") title(Prediction of the association between glucose and adverse CVD outcomes)


// Categorical //

* 21. Complete case analysis of categorical covariates using logistic regression and the associated plot
logistic mi_fchd ib2.glucosea i.agea sex i.bmia cursmoke i.cigpdaya i.educ prior_hyp, base

partpred or_cat, for (ib2.glucosea) ref(i.glucosea 2) ci(or_cat_cl or_cat_cu) eform
twoway (rcap or_cat_cl or_cat_cu glucosea, sort pstyle(ci)) (scatter or_cat glucosea, sort), legend(off) xtitle("glucose") ytitle("odds ratio") title(Prediction of the association between glucose and adverse CVD outcomes)

twoway (line or_cat glucose, sort) (line or glucose, sort), legend(off) xtitle("glucose") ytitle("odds ratio")

drop or or_cat or_cl or_cu or_cat_cl or_cat_cu




***********************************************************************************
* Multiple imputation of missing glucose and bmi values (continous adjusted model)*
***********************************************************************************

* 22. Generating a new glucose (exposure) variable for multiple imputation called glucoseb
gen glucoseb = glucose


* 23. Generating a new bmi variable for multiple imputation called bmi_mi
gen bmi_mi = bmi


* 24. Creation of imputations
mi set wide
mi register imputed glucoseb 
mi register imputed bmi_mi
mi register regular age sex cursmoke cigpday educ prior_hyp mi_fchd
mi impute chained (regress) glucoseb bmi_mi = age sex cursmoke cigpday educ prior_hyp mi_fchd, add(10)


// Continuous, MI //

* 25. Multiple imputation analysis of covariates using logistic regression
mi estimate, or: logistic mi_fchd glucoseb bmi_mi age sex cursmoke cigpday educ prior_hyp, base




******************************************************************
* Cox regression and Weibull survival analysis (adjusted model) - Continous *
******************************************************************

* 26. Multiple imputation analysis - cox regression of continuous glucose
mi stset enddate, enter(baseline_visit) origin(dob) fail(mi_fchd) scale(365.25)
mi estimate, eform base: stcox glucoseb i.agea sex bmi_mi cursmoke i.cigpdaya i.educ prior_hyp


* 27.  Multiple imputation analysis - Weibull analysis of continuous glucose
mi estimate, eform base: streg glucoseb i.agea sex bmi_mi cursmoke i.cigpdaya i.educ prior_hyp, d(weibull)

