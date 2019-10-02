
**************************************
* Preliminaries - Categorical 2 of 2 *
**************************************

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


* 8. Recoding cigpday in to a categorical variable called cigpdaya
gen cigpdaya = cigpday
summ cigpdaya
recode cigpdaya min/10=1 10/20=2 20/40=3 40/max=4
label define cigpdaya 1 "<10 perday" 2 "10/20 per day" 3 "20/40 per day" 4 ">40 per day"
label value cigpdaya cigpdaya
tab cigpdaya


* 9. Labeling educ
label define educ 1 "0-11yrs" 2 "high school" 3 "some college" 4 "college grad or higher qual"
label value educ educ
tab educ

* 10. Recode last_followup, date_dth and date_mifchd to a new variable called enddate
gen enddate = last_followup
format enddate %td
replace enddate = date_dth if date_dth <= last_followup
replace enddate = date_mifchd if date_mifchd <= last_followup




*********************************************************************
* Unadjusted model analysis - presented in continous do.file 1 of 2 *
*********************************************************************




***********************************************************************************
* Complete case analysis (adjusted model) - presented in continous do.file 1 of 2 *
***********************************************************************************




***********************************************************************************
* Multiple imputation of missing glucose and bmi values (continous adjusted model)*
***********************************************************************************

* 11. Generating a new glucose (exposure) variable for multiple imputation called glucosec
gen glucosec = glucose


* 12. Generating a new bmi variable for multiple imputation called bmi_mi_c
gen bmi_mi_c = bmi


* 13. Relabelling of glucosec and bmi_mi_c variables
summ glucosec
recode glucosec min/70=1 70/100=2 100/125=3 125/max=4
label define glucosec 1 "<70mg/dl (low blood sugar (hypoglycemia))" 2 "70/100mg/dl (normal blood sugar)" 3 "101/125mg/dl (pre-diabetic blood sugar)" 4 ">126mg/dl (diabetic blood sugar)"
label value glucosec glucosec
tab glucosec

summ bmi_mi_c
recode bmi_mi_c min/18.5=1 18.5/24.9=2 24.9/29.9=3 29.9/39.9=4 39.9/max=5
label define bmi_mi_c 1 "underweight" 2 "healthy weight" 3 "overweight" 4 "obese" 5 "very obese"
label value bmi_mi_c bmi_mi_c
tab bmi_mi_c


* 14. Creation of imputations and associated multiple imputation analysis of covariates using logistic regression
mi set wide
set seed 12345
mi register imputed glucosec 
mi register imputed bmi_mi_c
mi register regular age sex cursmoke cigpday educ prior_hyp mi_fchd
mi impute chained (mlogit) glucosec bmi_mi_c = age sex cursmoke cigpday educ prior_hyp mi_fchd, add(10)
mi estimate, or: logistic mi_fchd ib2.glucosec i.agea sex i.bmi_mi_c cursmoke i.cigpdaya i.educ prior_hyp, base




********************************************************************
* Cox and Weibull survival analysis (adjusted model) - Categorical *
********************************************************************

* 15. Multiple imputation analysis - cox regression of categorical glucose
mi stset enddate, enter(baseline_visit) origin(dob) fail(mi_fchd) scale(365.25)
mi estimate, eform base: stcox ib2.glucosec i.agea sex i.bmi_mi_c cursmoke i.cigpdaya i.educ prior_hyp


* 16.  Multiple imputation analysis - Weibull analysis of categorical glucose
mi estimate, eform base: streg ib2.glucosec i.agea sex i.bmi_mi_c cursmoke i.cigpdaya i.educ prior_hyp, d(weibull)

