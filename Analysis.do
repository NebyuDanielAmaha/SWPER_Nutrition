
****************************************************************************************************
************ Code to estimate SWPER global individual scores for each domain *********************** 
****************************************************************************************************
			   *Written by Fernanda Ewerling*
				*Updated July 2020*
						
/*
READ ME

Before using this code, please make sure that your dataset have all the variables, with the same names presented below:
v744a - Beating not justified if wife goes out without telling husband
v744b - Beating not justified if wife neglects the children
v744c - Beating not justified if wife argues with husband
v744d - Beating not justified if wife refuses to have sex with husband
v744e - Beating not justified if wife burns the food
v157  - Frequency of reading newspaper or magazine
v133  - Woman education
v212  - Age of respondent at first birth 
v511  - Age at first cohabitation
v743a - Who usually decides on respondent's health care
v743b - Who usually decides on large household purchases
v743d - Who usually decides on visits to family or relatives
v715  - Husband's years of schooling
v730  - Husband's age
v501  - Current marital status (1=married, 2= in a union)
*/

tab v744a
tab v744d
tab v157
tab v133
tab v212
tab v511
tab v743a
tab v743b
tab v743d
tab v715
tab v730
tab v501
tab v501,nolabel

set more off
*log using "SWPER.log"

gen union = inlist(v501,1,2)				// Currently married or in union
  	  lab var union "Currently married or in union"
  	  lab val union yn
  	  
*** SWPER global - Survey-based women's empowerment index
// Recoding missings from 9, 99, 999 to . 

mvdecode v744a v744b v744c v744d v744e v743a v743b v743d v157, mv(9)
mvdecode v133 v715 v730, mv(99)
  		
// Beating NOT justified variables
recode v744a 0=1 1=-1 8=0, gen(beat1)
recode v744b 0=1 1=-1 8=0, gen(beat2)
recode v744c 0=1 1=-1 8=0, gen(beat3)
recode v744d 0=1 1=-1 8=0, gen(beat4)
recode v744e 0=1 1=-1 8=0, gen(beat5)
label define beat -1"Yes" 0" Don't know" 1"No"
label value (beat1 beat2 beat3 beat4 beat5) beat
  		
//Decision variables - new categories
recode v743a 4/7=-1 2/3=1 8/max=., gen(decide1a)
recode v743b 4/7=-1 2/3=1 8/max=., gen(decide2a)
recode v743d 4/7=-1 2/3=1 8/max=., gen(decide3a)
label define decide1 -1"Husband/partner or other alone" 1"Respondent alone or jointly with husband/partner" 
label value (decide1a decide2a decide3a) decide1
  		
//Wm education & work variables
recode v133 98=., gen(educ)
recode v157 3=2, gen(read) 		// Most countries do not have the cathegory "almost everyday"(3), so we recoded it to "At least once a week"(2).
  		
//Wm autonomy questions
clonevar age1cohab=v511
	*Imputing age1birth for those women that do not have children***
	recode age1cohab 33/max=33, gen (age1)
	hotdeck v212, store by(age1) keep(caseid) imp(1)
	sort age1 v212
	preserve
	use "imp1.dta", clear
	rename v212 v212_i
	drop age1
	save, replace
	restore
	cap drop _merge
	merge 1:1 caseid using "imp1.dta"
	erase "imp1.dta"
	
	clonevar age1birth=v212
  		
//Husband variables
recode v715 98=., gen(husb_educ)		//Recode dk as missing 
gen educ_diff= educ - husb_educ 		//education difference: woman-husband years of schooling
recode v730 98=., gen(husb_age)			//Recode dk as missing 
gen age_diff = v012 - husb_age 			// age difference: woman-husband
  		
//Generate continuous SWPER global scores		
gen swpatt=((-1.202)+(0.508*beat1)+(0.508*beat2)+(0.526*beat3)+(0.538*beat4)+(0.588*beat5)+(0.083*read)+(0.016*educ)+(-0.006*age1birth)+(-0.010*age1cohab)+(0.001*age_diff)+(0.002*educ_diff)+(0.001*decide1a)+(-0.017*decide2a)+(-0.002*decide3a))/1.811
gen swpsoc=((-5.661)+(-0.012*beat1)+(-0.026*beat2)+(0.001*beat3)+(0.001*beat4)+(-0.015*beat5)+(0.422*read)+(0.081*educ)+(0.133*age1birth)+(0.139*age1cohab)+(0.031*age_diff)+(0.054*educ_diff)+(-0.004*decide1a)+(-0.022*decide2a)+(-0.034*decide3a))/1.526
gen swpdec=((0.168)+(-0.003*beat1)+(-0.040*beat2)+(0.007*beat3)+(0.028*beat4)+(-0.020*beat5)+(0.121*read)+(0.022*educ)+(-0.012*age1birth)+(-0.016*age1cohab)+(0.013*age_diff)+(0.001*educ_diff)+(0.599*decide1a)+(0.601*decide2a)+(0.619*decide3a))/1.502
la var swpatt "SWPER global score - attitude to violence domain"
la var swpsoc "SWPER global score - social independence domain"
la var swpdec "SWPER global score - decision-making domain"

//Generate categorical SWPER global with 3 groups of empowerment (low, medium and high)
recode swpatt min/-0.700001=1 -0.7/0.400001=2 0.4/max=3, gen(swpatt3gr)
recode swpsoc min/-0.559=1 -0.558999/0.293=2 0.293001/max=3, gen(swpsoc3gr)
recode swpdec min/-1.000001=1 -1.0/0.600001=2 0.6/max=3, gen(swpdec3gr)
la def swp3gr 1"low empowerment" 2"medium empowerment" 3"high empowerment"
la value (swpatt3gr swpsoc3gr swpdec3gr) swp3gr
la var swpatt3gr "SWPER global 3 groups - attitude to violence domain"
la var swpsoc3gr "SWPER global 3 groups - social independence domain"
la var swpdec3gr "SWPER global 3 groups - decision-making domain"



*******************************************************************************
************ Code to estimate Anthropometry indicators  *********************** 
*******************************************************************************

cap label define yesno 0"No" 1"Yes"
gen wt=v005/1000000

* Calculate age
gen age = v008 - b3
		
	* to check if survey has b19, which should be used instead to compute age. 
	scalar b19_included=1
		capture confirm numeric variable b19, exact 
		if _rc>0 {
		* b19 is not present
		scalar b19_included=0
			}
		if _rc==0 {
		* b19 is present; check for values
		summarize b19
		if r(sd)==0 | r(sd)==. {
		scalar b19_included=0
			  }
		}

		if b19_included==1 {
		drop age
		gen age=b19
		}
		drop if age < 6


//Severely stunted
gen nt_ch_sev_stunt= 0 if v135==1	
replace nt_ch_sev_stunt=. if hw70>=9996
replace nt_ch_sev_stunt=1 if hw70<-300 & v135==1 
label values nt_ch_sev_stunt yesno
label var nt_ch_sev_stunt "Severely stunted child under 5 years"

//Stunted
gen nt_ch_stunt= 0 if v135==1
replace nt_ch_stunt=. if hw70>=9996
replace nt_ch_stunt=1 if hw70<-200 & v135==1 
label values nt_ch_stunt yesno
label var nt_ch_stunt "Stunted child under 5 years"

//Mean haz
gen haz=hw70/100 if hw70<996
summarize haz if v135==1 [iw=wt]
gen nt_ch_mean_haz=round(r(mean),0.01)
label var nt_ch_mean_haz "Mean z-score for height-for-age for children under 5 years"

//Severely wasted 
gen nt_ch_sev_wast= 0 if v135==1
replace nt_ch_sev_wast=. if hw72>=9996
replace nt_ch_sev_wast=1 if hw72<-300 & v135==1 
label values nt_ch_sev_wast yesno
label var nt_ch_sev_wast "Severely wasted child under 5 years"

//Wasted
gen nt_ch_wast= 0 if v135==1
replace nt_ch_wast=. if hw72>=9996
replace nt_ch_wast=1 if hw72<-200 & v135==1 
label values nt_ch_wast yesno
label var nt_ch_wast "Wasted child under 5 years"

//Overweight 
gen nt_ch_ovwt= 0 if v135==1
replace nt_ch_ovwt=. if hw72>=9996
replace nt_ch_ovwt=1 if hw72>200 & hw72<9996 & v135==1 
label values nt_ch_ovwt yesno
label var nt_ch_ovwt "Overweight child under 5 years"

//Mean whz
gen whz=hw72/100 if hw72<996
summarize whz if v135==1 [iw=wt]
gen nt_ch_mean_whz=round(r(mean),0.01)
label var nt_ch_mean_whz "Mean z-score for weight-for-height for children under 5 years"

//Severely underweight
gen nt_ch_sev_underwt= 0 if v135==1
replace nt_ch_sev_underwt=. if hw71>=9996
replace nt_ch_sev_underwt=1 if hw71<-300 & v135==1 
label values nt_ch_sev_underwt yesno
label var nt_ch_sev_underwt	"Severely underweight child under 5 years"

//Underweight
gen nt_ch_underwt= 0 if v135==1
replace nt_ch_underwt=. if hw71>=9996
replace nt_ch_underwt=1 if hw71<-200 & v135==1 
label values nt_ch_underwt yesno
label var nt_ch_underwt "Underweight child under 5 years"

//Mean waz
gen waz=hw71/100 if hw71<996
summarize waz if v135==1 [iw=wt]
gen nt_ch_mean_waz=round(r(mean),0.01)
label var nt_ch_mean_waz "Mean weight-for-age for children under 5 years"

***********************************************************
************ Code to create Tables  *********************** 
***********************************************************

/* TABLE 1*/
svyset v021 [pw=wt], strata(v022) singleunit(centered)

svy: mean haz
svy: mean waz
svy: mean whz


/* TABLE 2*/

set more off
svyset v021 [pw=wt], strata(v022) singleunit(centered)
*birth order
*recode bord (1=1 "1st") (2/3=2 "2nd-3rd") (4/20=3 "4th+"), gen(bord_cat)

* Model 1: Adjusted PR
* Stunting
* attitude towards violence
svy: glm nt_ch_stunt swpatt i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform
* social independence domain
svy: glm nt_ch_stunt swpsoc i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform		
* decision making domain
svy: glm nt_ch_stunt swpdec i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform

* Wasting
* attitude towards violence
svy: glm nt_ch_wast swpatt i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform
* social independence domain
svy: glm nt_ch_wast swpsoc i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform		
* decision making domain
svy: glm nt_ch_wast swpdec i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform

* Underweight
* attitude towards violence
svy: glm nt_ch_underwt swpatt i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform
* social independence domain
svy: glm nt_ch_underwt swpsoc i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform		
* decision making domain
svy: glm nt_ch_underwt swpdec i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform

* Overweight
* attitude towards violence
svy: glm nt_ch_ovwt swpatt i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform
* social independence domain
svy: glm nt_ch_ovwt swpsoc i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform		
* decision making domain
svy: glm nt_ch_ovwt swpdec i.v024 i.v025 age i.b4 i.bord_cat, family(poisson) link(log) eform


/* TABLE 3*/
* Sensitivity Analysis

* Linear regression
* Stunting
* attitude towards violence
svy: regress haz swpatt i.v024 i.v025 age i.b4 i.bord_cat
* social independence domain
svy: regress haz swpsoc i.v024 i.v025 age i.b4 i.bord_cat		
* decision making domain
svy: regress haz swpdec i.v024 i.v025 age i.b4 i.bord_cat

* Underweight
* attitude towards violence
svy: regress waz swpatt i.v024 i.v025 age i.b4 i.bord_cat
* social independence domain
svy: regress waz swpsoc i.v024 i.v025 age i.b4 i.bord_cat	
* decision making domain
svy: regress waz swpdec i.v024 i.v025 age i.b4 i.bord_cat

* Wasting/Overweight
* attitude towards violence
svy: regress whz swpatt i.v024 i.v025 age i.b4 i.bord_cat
* social independence domain
svy: regress whz swpsoc i.v024 i.v025  age i.b4 i.bord_cat	
* decision making domain
svy: regress whz swpdec i.v024 i.v025 age i.b4 i.bord_cat

