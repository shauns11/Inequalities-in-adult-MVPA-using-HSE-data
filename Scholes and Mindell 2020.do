*********************************************************************************************
*Inequalities in participation and time spent in moderate-to-vigorous physical activity: a
*pooled analysis of the cross-sectional health surveys for England 2008, 2012, and 2016
*Scholes and Mindell BMC Public Health (2020) 20:361
**********************************************************************************************

use "N:\Analysis_Dataset.dta", clear
renvars, lower
gen male = sex==1
gen female = sex==2

gen agegroup2=0
replace agegroup2=1 if inrange(ag16g10,1,2)
replace agegroup2=2 if inrange(ag16g10,3,4)
replace agegroup2=3 if inrange(ag16g10,5,6)
replace agegroup2=4 if ag16g10==7
label define agegrplbl 1 "16-34" 2 "35-54" 3 "55-74" 4 "75+"
label values agegroup2 agegrplbl 

**********.
*Table 1.
**********.

foreach var of varlist agegroup2 cigsta3 genhelf2 bmivg5 {
tab `var' eqv3 if sex==1
tab `var' eqv3 if sex==2
}

* % Work.

foreach var of varlist work {
tab `var' sex,col
tab `var' eqv3 if sex==1,col
tab `var' eqv3 if sex==2,col
}

* % Work (% active).

preserve
keep if work==1
foreach var of varlist active {
tab `var' eqv3 if sex==1,col
tab `var' eqv3 if sex==2,col
}
restore

svyset [pweight=wt_int],psu(point1)

foreach var of varlist agegroup2 cigsta3 genhelf2 bmivg5 {
svy,subpop(male): tab `var' eqv3, col
svy,subpop(female): tab `var' eqv3, col
}

*P-values excluding the missing.

mvdecode cigsta3,mv(-9 -8 -1)
svy,subpop(male): tab cigsta3 eqv3, col
svy,subpop(female): tab cigsta3 eqv3, col

mvdecode bmivg5,mv(-9 -8 -1)
svy,subpop(male): tab bmivg5 eqv3, col
svy,subpop(female): tab bmivg5 eqv3, col

**************
*PA variables.
**************.

summ mins10tot0812 mins10hwk mins10man mins10wlk mins10spta mins10wrk08

*truncate all variables to 40 hours a week.
*truncate walking at 2400.
*truncate manual at 2400.
*truncate walking at 2400.
*truncate housework at 2400.

replace mins10tot0812 = 2400 if mins10tot0812>=2400
replace mins10wlk = 2400 if mins10wlk>=2400
replace mins10hwk = 2400 if mins10hwk>=2400
replace mins10man = 2400 if mins10man>=2400
summ mins10tot0812 mins10hwk mins10man mins10wlk mins10spta mins10wrk08,sep(6)

*After truncation.
generate hours = mins10tot0812/60
generate hours_walking = mins10wlk/60
generate hours_work = mins10wrk08/60
generate hours_sport = mins10spta/60
generate hours_manual = mins10man/60
generate hours_housewrk = mins10hwk/60
summ hours hours_walking hours_work hours_sport hours_manual hours_housewrk,sep(6)


*Graph the distributions.
preserve
replace mins10tot0812 = (mins10tot0812/60)
summ mins10tot0812
summ mins10tot0812 if mins10tot0812>0
histogram mins10tot0812,ylabel(0 0.05 0.1 0.15 0.2,ang(hor) nogrid) fraction discrete  color(black)  ///
xtitle("Hours per week MVPA") ytitle("% of participants") ///
note("") by(sex,graphregion(color(white)) bgcolor(white) note(""))
graph export "N:\Hurdle Model\Submission\Figure1.tif", width(2820) height(1710) replace
restore

*Active.
gen active0 = (mins10tot0812>0)
bysort active0: summ mins10tot0812

*Recommendations.
generate MeetsRecs = (mins10tot0812>=150)
bysort MeetsRecs: summ mins10tot0812

*remember: 10 mins a month = 2.5 mins a week.

* use the pooled data for age-standardisation.
svy:tab ag16g10 sex, col format(%8.4f)     
generate std_weight=0
replace std_weight=0.1411 if ag16g10==1 & sex==1
replace std_weight=0.1656 if ag16g10==2 & sex==1
replace std_weight=0.1927 if ag16g10==3 & sex==1
replace std_weight=0.1774 if ag16g10==4 & sex==1
replace std_weight=0.1501 if ag16g10==5 & sex==1
replace std_weight=0.1023 if ag16g10==6 & sex==1
replace std_weight=0.0708 if ag16g10==7 & sex==1
replace std_weight=0.1343 if ag16g10==1 & sex==2
replace std_weight=0.1677 if ag16g10==2 & sex==2
replace std_weight=0.1880 if ag16g10==3 & sex==2
replace std_weight=0.1718 if ag16g10==4 & sex==2
replace std_weight=0.1448 if ag16g10==5 & sex==2
replace std_weight=0.1061 if ag16g10==6 & sex==2
replace std_weight=0.0873 if ag16g10==7 & sex==2

*******************************.
*Tables 1 (men) and 2 (women).
*******************************.

*************************
*Total MVPA: any
*************************.

svy:mean active0,over(sex)                                                                                   
estat size                                                                             
svy:mean active0,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)     
estat size
lincom [active0 ]_subpop_2  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_3  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_5  - [active0 ]_ subpop_4
lincom [active0 ]_subpop_6  - [active0 ]_ subpop_4

********************************
*Total MVPA: recommendations
******************************.

svy:mean MeetsRecs,over(sex)                                                                                             
svy:mean MeetsRecs,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)          
lincom [MeetsRecs ]_subpop_2  - [MeetsRecs ]_ subpop_1
lincom [MeetsRecs ]_subpop_3  - [MeetsRecs ]_ subpop_1
lincom [MeetsRecs ]_subpop_5  - [MeetsRecs ]_ subpop_4
lincom [MeetsRecs ]_subpop_6  - [MeetsRecs ]_ subpop_4

********************************
*Total MVPA: mean (incl zeros)
******************************.

svy:mean hours,over(sex)      
svy:mean hours,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours]_subpop_2  - [hours]_ subpop_1
lincom [hours ]_subpop_3  - [hours]_ subpop_1
lincom [hours]_subpop_5  - [hours]_ subpop_4
lincom [hours]_subpop_6  - [hours]_ subpop_4

********************************
*Total MVPA: mean (excl zeros)
******************************.

svy,subpop(active0):mean hours,over(sex)      
svy,subpop(active0):mean hours,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours]_subpop_2  - [hours]_ subpop_1
lincom [hours ]_subpop_3  - [hours]_ subpop_1
lincom [hours]_subpop_5  - [hours]_ subpop_4
lincom [hours]_subpop_6  - [hours]_ subpop_4


*************************
*Sports
*************************.

drop active0 
generate active0 = (hours_sport>0)
generate active150 = (hours_sport>2.49999)

*************************
*Sports: any
*************************.
svy:mean active0,over(sex)                                                                                    
svy:mean active0,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active0 ]_subpop_2  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_3  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_5  - [active0 ]_ subpop_4
lincom [active0 ]_subpop_6  - [active0 ]_ subpop_4

*************************
*Sports: 150+
*************************.
svy:mean active150,over(sex)      
svy:mean active150,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active150 ]_subpop_2  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_3  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_5  - [active150 ]_ subpop_4
lincom [active150 ]_subpop_6  - [active150 ]_ subpop_4

*************************
*Sports: mean (incl zeros)
*************************.
svy:mean hours_sport,over(sex)      
svy:mean hours_sport,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours_sport]_subpop_2  - [hours_sport]_ subpop_1
lincom [hours_sport]_subpop_3  - [hours_sport]_ subpop_1
lincom [hours_sport]_subpop_5  - [hours_sport]_ subpop_4
lincom [hours_sport]_subpop_6  - [hours_sport]_ subpop_4

*************************
*Sports: mean (excl zeros)
*************************.
svy,subpop(active0):mean hours_sport,over(sex)     
svy,subpop(active0):mean hours_sport,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours_sport]_subpop_2  - [hours_sport]_ subpop_1
lincom [hours_sport]_subpop_3  - [hours_sport]_ subpop_1
lincom [hours_sport]_subpop_5  - [hours_sport]_ subpop_4
lincom [hours_sport]_subpop_6  - [hours_sport]_ subpop_4


****************************************
*Domestic (hours_housewrk).
****************************************

drop active0 active150
generate active0 = (hours_housewrk>0)
generate active150 = (hours_housewrk>2.49999)

****************
*Domestic: any
****************

svy:mean active0,over(sex)      
svy:mean active0,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active0 ]_subpop_2  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_3  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_5  - [active0 ]_ subpop_4
lincom [active0 ]_subpop_6  - [active0 ]_ subpop_4

****************
*Domestic: 150+
****************
svy:mean active150,over(sex)      
svy:mean active150,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active150 ]_subpop_2  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_3  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_5  - [active150 ]_ subpop_4
lincom [active150 ]_subpop_6  - [active150 ]_ subpop_4

******************************
*Domestic: mean (inc. zeros)
******************************

svy:mean hours_housewrk,over(sex)      
svy:mean hours_housewrk,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours_housewrk]_subpop_2  - [hours_housewrk]_ subpop_1
lincom [hours_housewrk]_subpop_3  - [hours_housewrk]_ subpop_1
lincom [hours_housewrk ]_subpop_5  - [hours_housewrk]_ subpop_4
lincom [hours_housewrk ]_subpop_6  - [hours_housewrk]_ subpop_4

******************************
*Domestic: mean (exc. zeros)
******************************

svy,subpop(active0):mean hours_housewrk,over(sex)      /* MVPA-active */
svy,subpop(active0):mean hours_housewrk,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      /* MVPA-active */
lincom [hours_housewrk]_subpop_2  - [hours_housewrk]_ subpop_1
lincom [hours_housewrk]_subpop_3  - [hours_housewrk]_ subpop_1
lincom [hours_housewrk ]_subpop_5  - [hours_housewrk]_ subpop_4
lincom [hours_housewrk ]_subpop_6  - [hours_housewrk]_ subpop_4

*************
*Walking.
*************
drop active0 active150
generate active0 = (hours_walking>0)
generate active150= (hours_walking>2.49999)

****************
*Walking: any
****************
svy:mean active0,over(sex)      
svy:mean active0,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)     
lincom [active0 ]_subpop_2  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_3  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_5  - [active0 ]_ subpop_4
lincom [active0 ]_subpop_6  - [active0 ]_ subpop_4

****************
*Walking: 150+
****************
svy:mean active150,over(sex)      
svy:mean active150,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active150 ]_subpop_2  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_3  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_5  - [active150 ]_ subpop_4
lincom [active150 ]_subpop_6  - [active150 ]_ subpop_4

****************************
*Walking: mean (incl zeros)
******************************
svy:mean hours_walking,over(sex)      
svy:mean hours_walking,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)     
lincom [hours_walking]_subpop_2  - [hours_walking]_ subpop_1
lincom [hours_walking]_subpop_3  - [hours_walking]_ subpop_1
lincom [hours_walking]_subpop_5  - [hours_walking]_ subpop_4
lincom [hours_walking]_subpop_6  - [hours_walking]_ subpop_4

****************************
*Walking: mean (excl zeros)
******************************

svy,subpop(active0):mean hours_walking,over(sex)     
svy,subpop(active0):mean hours_walking,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)     
lincom [hours_walking]_subpop_2  - [hours_walking]_ subpop_1
lincom [hours_walking ]_subpop_3  - [hours_walking]_ subpop_1
lincom [hours_walking]_subpop_5  - [hours_walking]_ subpop_4
lincom [hours_walking]_subpop_6  - [hours_walking]_ subpop_4

***********
*Manual.
***********
drop active0 active150
generate active0 = (hours_manual>0)
generate active150 = (hours_manual>2.49999)

***************
*Manual: any
***************
svy:mean active0,over(sex)      
svy:mean active0,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)     
lincom [active0 ]_subpop_2  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_3  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_5  - [active0 ]_ subpop_4
lincom [active0 ]_subpop_6  - [active0 ]_ subpop_4

***************
*Manual: 150+
***************
svy:mean active150,over(sex)      
svy:mean active150,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active150 ]_subpop_2  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_3  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_5  - [active150 ]_ subpop_4
lincom [active150 ]_subpop_6  - [active150 ]_ subpop_4

****************************
*Manual: mean (incl zeros)
****************************
svy:mean hours_manual,over(sex)      
svy:mean hours_manual,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours_manual]_subpop_2  - [hours_manual]_ subpop_1
lincom [hours_manual]_subpop_3  - [hours_manual]_ subpop_1
lincom [hours_manual]_subpop_5  - [hours_manual]_ subpop_4
lincom [hours_manual]_subpop_6  - [hours_manual]_ subpop_4

****************************
*Manual: mean (excl zeros)
****************************
svy,subpop(active0):mean hours_manual,over(sex)      /* Mean */
svy,subpop(active0):mean hours_manual,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      /* Mean */
lincom [hours_manual]_subpop_2  - [hours_manual]_ subpop_1
lincom [hours_manual]_subpop_3  - [hours_manual]_ subpop_1
lincom [hours_manual]_subpop_5  - [hours_manual]_ subpop_4
lincom [hours_manual]_subpop_6  - [hours_manual]_ subpop_4

************************************.
*Occupational: Hours_work.
************************************.
drop active0 active150
generate active0 = (hours_work>0)
generate active150 = (hours_work>2.49999)

************************************.
*Occupational: any
************************************.
svy:mean active0,over(sex)     
svy:mean active0,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [active0 ]_subpop_2  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_3  - [active0 ]_ subpop_1
lincom [active0 ]_subpop_5  - [active0 ]_ subpop_4
lincom [active0 ]_subpop_6  - [active0 ]_ subpop_4

************************************.
*Occupational: 150+
************************************.
svy:mean active150,over(sex)      
svy:mean active150,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)     
lincom [active150 ]_subpop_2  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_3  - [active150 ]_ subpop_1
lincom [active150 ]_subpop_5  - [active150 ]_ subpop_4
lincom [active150 ]_subpop_6  - [active150 ]_ subpop_4

************************************.
*Occupational: mean (incl zeros)
************************************.
svy:mean hours_work,over(sex)      
svy:mean hours_work,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours_work]_subpop_2  - [hours_work]_ subpop_1
lincom [hours_work]_subpop_3  - [hours_work]_ subpop_1
lincom [hours_work]_subpop_5  - [hours_work]_ subpop_4
lincom [hours_work]_subpop_6  - [hours_work]_ subpop_4

************************************.
*Occupational: mean (excl zeros)
************************************.
svy,subpop(active0):mean hours_work,over(sex)      
svy,subpop(active0):mean hours_work,stdize(ag16g10) stdweight(std_weight) over(sex eqv3)      
lincom [hours_work]_subpop_2  - [hours_work]_ subpop_1
lincom [hours_work]_subpop_3  - [hours_work]_ subpop_1
lincom [hours_work]_subpop_5  - [hours_work]_ subpop_4
lincom [hours_work]_subpop_6  - [hours_work]_ subpop_4


***********************************************************
********************* Hurdle models ***********************
***********************************************************.

*Include missing as separate category (compare with complete cases).

mvencode cigsta3 bmivg5,mv(9)
recode genhelf2 (-8=1)

* 0.041 = 10 minutes a month (the minimum for activity).

***************.
* Overall MVPA.
***************.

svy,subpop(male): churdle exponential hours i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV (Uncond: incl zeros)*/
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration (Cond: excl zeros) */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

svy,subpop(female): churdle exponential hours i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration (Cond) */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

***************.
*Sports
***************.

svy,subpop(male): churdle exponential hours_sport i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV (Uncond) */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration (cond) */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

svy,subpop(female): churdle exponential hours_sport i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                           
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

***************.
*Domestic
***************.

svy,subpop(male): churdle exponential hours_housewrk i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

svy,subpop(female): churdle exponential hours_housewrk i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                           
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

***************.
*Walking
***************.

svy,subpop(male): churdle exponential hours_walking i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

svy,subpop(female): churdle exponential hours_walking i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

***************.
*Manual
***************.

svy,subpop(male): churdle exponential hours_manual i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

svy,subpop(female): churdle exponential hours_manual i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

***************.
*Occupational
***************.
svy,subpop(male): churdle exponential hours_work i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */

svy,subpop(female): churdle exponential hours_work i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5, select(i.eqv3 i.ag16g10 i.cigsta3 i.genhelf2 i.bmivg5) ll(0.041)                            
margins eqv3,predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                               /* activity */
margins, dydx(eqv3) predict(pr(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                        /* activity */
margins eqv3,predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                 /* LV */
margins, dydx(eqv3) predict(ystar(0,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                          /* LV */
margins eqv3,predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                                /* duration */
margins, dydx(eqv3) predict(e(0.041,,)) at(ag16g10==3 cigsta3==3 bmivg5==2 genhelf2==1)                         /* duration */













































