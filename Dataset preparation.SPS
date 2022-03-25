* Encoding: UTF-8.

dataset close all.
get file = 'N:\Hurdle Model\hse08ai.sav'.
missing values all ().
exe.
select if age>=16.
exe.

*Heavy housework.

recode hwtimhm (0 thru 9=0) (else=copy) into hw10tim2.
compute mins10hwk=0.
compute mins10hwk=(hw10tim2*hvydyhm)/4.
IF (hswrkhm=2 OR hvyhwkhm=2) mins10hwk=0.
IF hw10tim2=0 mins10hwk=0.
IF any(-9,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10hwk=-9.
IF any(-8,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10hwk=-8.
IF age<=15 mins10hwk=-1.
variable label mins10hwk '(D) Average minutes doing heavy housework per week(>=10mins) - all moderate'.
EXECUTE.


* Heavy manual.

recode diytimhm (0 thru 9=0) (else=copy) into diy10tim2.
compute mins10man=0.
compute mins10man=(diy10tim2*mndayhm)/4.
IF (gardnhm=2 OR manwrkhm=2) mins10man=0.
IF diy10tim2=0 mins10man=0.
IF any(-9, gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10man=-9.
IF any(-8,gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10man=-8.
IF age<=15 mins10man=-1.
variable label mins10man '(D) Average minutes doing heavy manual per week (>=10mins) - all moderate'.
exe.

*Wlk5it: (walk for at least 5 minutes: yes; no; cant walkt at all).
*Wlk10m: (walk for at least 10 minutes: yes; no): - 1 = no walk for 5 mins or cant walk at all: 12,182 = Yes.
*daywlk: (days for a walk for at least 10 minutes): base = 12,182 (wlk10m=Yes).
*day1wlk (more than 1 walk for 10min+: 8412 = Yes).
*day1wlk (number of days having more than 1 walk for 10min+: # for 8412).

* Days (# days doing just 1 walk).

compute days = daywlk-day2wlk.
IF daywlk=-8 days=-8.
IF daywlk=-1 days=-1.
IF day1wlk=-8 days=-8.
IF day1wlk=-1 days=-1.
IF day2wlk=-8 days=-8.
IF day2wlk=-1 days=-1.
exe.

*Number of days walking for 10+ min.
*A) Zero: no walks for 5 min+; no walks for 10min+ = No.
*B) if 1 walk a day only, then days = daywlk.

Compute Walk10no=0.
IF (Wlk5it=2) OR (Wlk5it=3) Walk10no=0.
IF Wlk10M=2 Walk10no=0.
IF (Day1Wlk=2) Walk10no=DayWlk.
exe.

* Walked on only one day, but did more than one: so number of days = 2.

IF (Day1Wlk=1 and DayWlk=1) Walk10no=(Day1Wlk*2).
EXECUTE.

*Number of days did 1 plus (number of days did more than 1 times 2).

IF (Day1Wlk=1 and DayWlk>1) Walk10no=((day2wlk*2)+(days)).
EXECUTE.

IF any (-8,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk) walk10no=-8.
IF any (-9,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk) walk10no=-9.
IF walkpace=1 walk10no=0.
IF walkpace=2 walk10no=0.
IF walkpace=5 walk10no=0.
IF age<16 walk10no=-1.
variable label walk10no '(D) Number of brisk/fast walks of 10 mins+ in last 4 weeks'.
exe.

* Minutes.
Recode tottim (0 thru 9=0) (else=copy) into tottim10.
compute mins10wlk=0.
compute mins10wlk =(tottim10*walk10no)/4.
IF tottim10=0 mins10wlk =0.
IF walk10no=-8 mins10wlk =-8.
IF walk10no=-9 mins10wlk =-9.
IF walk10no=-1 mins10wlk =-1.
variable label mins10wlk '(D) Average minutes walking of 10 mins+ per week brisk or fast - moderate'.
exe.

*Occupational.
*wrkdays wrkact21 wrkact22 wrkact23 wrkact24.
*Walking;lifting;climbing at work.

*fre WkActWlk WkActLft WkActClb.

* archived variables.

compute mins10wrk08 = (hrs10wrk08 * 60).
exe.

*************.
* Sports.
*************.

compute mins10spta=0.

* Light Intensity.

IF (act11=1 OR act12=1 OR act13=1 OR act14=1 Or Act15=1 OR act16=1)  mins10spta=mins10spta+0.
IF (WhtAct05=1 AND excswt05<>1) mins10spta=mins10spta+0.
IF (WhtAct10=1 AND excswt10<>1) mins10spta=mins10spta+0.
exe.

* Moderate.

IF (WhtAct01=1 AND range(dayexc01,1,28) AND excswt01<>1 AND exctim01>=10) mins10spta=mins10spta+((dayexc01* exctim01)/4).
IF (WhtAct02=1 AND range(dayexc02,1,28) AND excswt02<>1 AND exctim02>=10) mins10spta=mins10spta+((dayexc02* exctim02)/4).
IF (WhtAct03=1 AND range(dayexc03,1,28) AND excswt03<>1 AND exctim03>=10) mins10spta=mins10spta+((dayexc03* exctim03)/4).
IF (WhtAct04=1 AND range(dayexc04,1,28) AND excswt04<>1 AND exctim04>=10) mins10spta=mins10spta+((dayexc04* exctim04)/4).
IF (WhtAct05=1 AND range(dayexc05,1,28) AND excswt05=1 AND exctim05>=10) mins10spta=mins10spta+((dayexc05* exctim05)/4).
IF (WhtAct07=1 AND range(dayexc07,1,28) AND excswt07<>1 AND exctim07>=10) mins10spta=mins10spta+((dayexc07* exctim07)/4).
IF (WhtAct08=1 AND range(dayexc08,1,28) AND excswt08<>1 AND exctim08>=10) mins10spta=mins10spta+((dayexc08* exctim08)/4).
IF (WhtAct10=1 AND range(dayexc10,1,28) AND excswt10=1 AND exctim10>=10) mins10spta=mins10spta+((dayexc10* exctim10)/4).
IF (act11=2 AND range(dayexc11,1,28) AND exctim11>=10) mins10spta=mins10spta+((dayexc11* exctim11)/4).
IF (act12=2 AND range(dayexc12,1,28) AND exctim12>=10) mins10spta=mins10spta+((dayexc12* exctim12)/4).
IF (act13=2 AND range(dayexc13,1,28) AND exctim13>=10) mins10spta=mins10spta+((dayexc13* exctim13)/4).
IF (act14=2 AND range(dayexc14,1,28) AND exctim14>=10) mins10spta=mins10spta+((dayexc14* exctim14)/4).
IF (act15=2 AND range(dayexc15,1,28) AND exctim15>=10) mins10spta=mins10spta+((dayexc15* exctim15)/4).
IF (act16=2 AND range(dayexc16,1,28) AND exctim16>=10) mins10spta=mins10spta+((dayexc16* exctim16)/4).
IF (act11=3 AND range(dayexc11,1,28) AND excswt11<>1 AND exctim11>=10) mins10spta=mins10spta+((dayexc11* exctim11)/4).
IF (act12=3 AND range(dayexc12,1,28) AND excswt12<>1 AND exctim12>=10) mins10spta=mins10spta+((dayexc12* exctim12)/4).
IF (act13=3 AND range(dayexc13,1,28) AND excswt13<>1 AND exctim13>=10) mins10spta=mins10spta+((dayexc13* exctim13)/4).
IF (act14=3 AND range(dayexc14,1,28) AND excswt14<>1 AND exctim14>=10) mins10spta=mins10spta+((dayexc14* exctim14)/4).
IF (act15=3 AND range(dayexc15,1,28) AND excswt15<>1 AND exctim15>=10) mins10spta=mins10spta+((dayexc15* exctim15)/4).
IF (act16=3 AND range(dayexc16,1,28) AND excswt16<>1 AND exctim16>=10) mins10spta=mins10spta+((dayexc16* exctim16)/4).
IF (act11=4 AND range(dayexc11,1,28) AND excswt11<>1 AND exctim11>=10) mins10spta=mins10spta+((dayexc11* exctim11)/4).
IF (act12=4 AND range(dayexc12,1,28) AND excswt12<>1 AND exctim12>=10) mins10spta=mins10spta+((dayexc12* exctim12)/4).
IF (act13=4 AND range(dayexc13,1,28) AND excswt13<>1 AND exctim13>=10) mins10spta=mins10spta+((dayexc13* exctim13)/4).
IF (act14=4 AND range(dayexc14,1,28) AND excswt14<>1 AND exctim14>=10) mins10spta=mins10spta+((dayexc14* exctim14)/4).
IF (act15=4 AND range(dayexc15,1,28) AND excswt15<>1 AND exctim15>=10) mins10spta=mins10spta+((dayexc15* exctim15)/4).
IF (act16=4 AND range(dayexc16,1,28) AND excswt16<>1 AND exctim16>=10) mins10spta=mins10spta+((dayexc16* exctim16)/4).
exe.

* Vigorous.

IF (WhtAct01=1 AND range(dayexc01,1,28) AND excswt01=1 AND exctim01>=10) mins10spta=mins10spta+(((dayexc01* exctim01)/4)*2).
IF (WhtAct02=1 AND range(dayexc02,1,28) AND excswt02=1 AND exctim02>=10) mins10spta=mins10spta+(((dayexc02* exctim02)/4)*2).
IF (WhtAct03=1 AND range(dayexc03,1,28) AND excswt03=1 AND exctim03>=10) mins10spta=mins10spta+(((dayexc03* exctim03)/4)*2).
IF (WhtAct04=1 AND range(dayexc04,1,28) AND excswt04=1 AND exctim04>=10) mins10spta=mins10spta+(((dayexc04* exctim04)/4)*2).
IF (WhtAct07=1 AND range(dayexc07,1,28) AND excswt07=1 AND exctim07>=10) mins10spta=mins10spta+(((dayexc07* exctim07)/4)*2).
IF (WhtAct08=1 AND range(dayexc08,1,28) AND excswt08=1 AND exctim08>=10) mins10spta=mins10spta+(((dayexc08* exctim08)/4)*2).
IF (WhtAct06=1 AND range(dayexc06,1,28) AND exctim06>=10) mins10spta=mins10spta+(((dayexc06* exctim06)/4)*2).
IF (WhtAct09=1 AND range(dayexc09,1,28) AND exctim09>=10) mins10spta=mins10spta+(((dayexc09* exctim09)/4)*2).
IF (act11=3 AND range(dayexc11,1,28) AND excswt11=1 AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act11=4 AND range(dayexc11,1,28) AND excswt11=1 AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act11=5 AND range(dayexc11,1,28) AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act11=6 AND range(dayexc11,1,28) AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act12=3 AND range(dayexc12,1,28) AND excswt12=1 AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act12=4 AND range(dayexc12,1,28) AND excswt12=1 AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act12=5 AND range(dayexc12,1,28) AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act12=6 AND range(dayexc12,1,28) AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act13=3 AND range(dayexc13,1,28) AND excswt13=1 AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act13=4 AND range(dayexc13,1,28) AND excswt13=1 AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act13=5 AND range(dayexc13,1,28) AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act13=6 AND range(dayexc13,1,28) AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act14=3 AND range(dayexc14,1,28) AND excswt14=1 AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act14=4 AND range(dayexc14,1,28) AND excswt14=1 AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act14=5 AND range(dayexc14,1,28) AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act14=6 AND range(dayexc14,1,28) AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act15=3 AND range(dayexc15,1,28) AND excswt15=1 AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act15=4 AND range(dayexc15,1,28) AND excswt15=1 AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act15=5 AND range(dayexc15,1,28) AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act15=6 AND range(dayexc15,1,28) AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act16=3 AND range(dayexc16,1,28) AND excswt16=1 AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
IF (act16=4 AND range(dayexc16,1,28) AND excswt16=1 AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
IF (act16=5 AND range(dayexc16,1,28) AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
IF (act16=6 AND range(dayexc16,1,28) AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
exe.

*Sports: maximum of 40 hours a week.

recode mins10spta (2400 thru hi = 2400).
exe.

* Overall summary = maximum of 60 hours a week.

*desc var mins10hwk mins10man mins10wlk mins10wrk08 mins10spta.

compute mins10tot0812 =  mins10hwk  + mins10man + mins10wlk + mins10wrk08 + mins10spta.
recode mins10tot0812 (3600 thru hi = 3600).
exe.

IF any(-9,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10tot0812=-9.
IF any(-8,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10tot0812=-8.
IF any(-9, gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10tot0812=-9.
IF any(-8,gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10tot0812=-8.
IF any (-8,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk) mins10tot0812=-8.
IF any (-9,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk) mins10tot0812=-9.
If any(-9,wrkact22,wrkact23,wrkact24,wrkdays,wkactwlk,wkactclb,wkactlft,active) mins10tot0812=-9.
If any(-8,wrkact22,wrkact23,wrkact24,wrkdays,wkactwlk,wkactclb,wkactlft,active) mins10tot0812=-8.
If any(-9,actphy) mins10tot0812=-9.
If any(-8,actphy) mins10tot0812=-8.
exe.

missing values mins10tot0812 (-9,-8,-1).
EXECUTE.

FORMAT psu (F4).
EXE.
string areastr (a4).
compute AREAstr = STRING(psu,F4).
exe.
compute AREAstr = LTRIM(areastr).
exe.
string year1 (a4).
compute year1="2008".
exe.
STRING point1 (A8).
COMPUTE point1 = CONCAT(year1,areastr).
exe.
compute year=2008.
exe.

save outfile = "N:\Temp\File2008.sav"
/keep pserial sex ag16g10 wt_int year mins10hwk mins10man mins10wlk mins10spta mins10wrk08 mins10tot0812 
eqv3 bmival cigsta3 point1 genhelf2 work active.

********.
* 2012.
*********

dataset close all.
get file = 'N:\Hurdle Model\hse2012ai.sav'.
missing values all ().
exe.
select if age>=16.
exe.

*Heavy housework.

recode hwtimhm (0 thru 9=0) (else=copy) into hw10tim2.
compute mins10hwk=0.
compute mins10hwk=(hw10tim2* hvydyhm)/4.
IF (hswrkhm=2 OR hvyhwkhm=2) mins10hwk=0.
IF hw10tim2=0 mins10hwk=0.
IF any(-9,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10hwk=-9.
IF any(-8,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10hwk=-8.
IF age<=15 mins10hwk=-1.
variable label mins10hwk '(D) Average minutes doing heavy housework per week(>=10mins) - all moderate'.
EXECUTE.

*Heavy manual.

recode diytimhm (0 thru 9=0) (else=copy) into diy10tim2.
compute mins10man=0.
compute mins10man=(diy10tim2*mndayhm)/4.
IF (gardnhm=2 OR manwrkhm=2) mins10man=0.
IF diy10tim2=0 mins10man=0.
IF any(-9, gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10man=-9.
IF any(-8,gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10man=-8.
IF age<=15 mins10man=-1.
variable label mins10man '(D) Average minutes doing heavy manual per week (>=10mins) - all moderate'.
exe.

*Wlk5it: (walk for at least 5 minutes: yes; no; cant walkt at all).
*Wlk10m: (walk for at least 10 minutes: yes; no): - 1 = no walk for 5 mins or cant walk at all: 12,182 = Yes.
*daywlk: (days for a walk for at least 10 minutes): base = 12,182 (wlk10m=Yes).
*day1wlk (more than 1 walk for 10min+: 8412 = Yes).
*day1wlk (number of days having more than 1 walk for 10min+: # for 8412).
* Days (# days doing just 1 walk).

compute days = daywlk-day2wlk.
IF daywlk=-8 days=-8.
IF daywlk=-1 days=-1.
IF day1wlk=-8 days=-8.
IF day1wlk=-1 days=-1.
IF day2wlk=-8 days=-8.
IF day2wlk=-1 days=-1.
exe.

*Number of days walking for 10+ min.
*A) Zero: no walks for 5 min+; no walks for 10min+ = No.
*B) if 1 walk a day only, then days = daywlk.

Compute Walk10no=0.
IF (Wlk5it=2) OR (Wlk5it=3) Walk10no=0.
IF Wlk10M=2 Walk10no=0.
IF (Day1Wlk=2) Walk10no=DayWlk.
exe.

* Walked on only one day, but did more than one: so number of days = 2.

IF (Day1Wlk=1 and DayWlk=1) Walk10no=(Day1Wlk*2).
EXECUTE.

*Number of days did 1 plus (number of days did more than  1 times 2).

IF (Day1Wlk=1 and DayWlk>1) Walk10no=((day2wlk*2)+(days)).
EXECUTE.

IF any (-8,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk) walk10no=-8.
IF any (-9,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk) walk10no=-9.
IF walkpace=1 walk10no=0.
IF walkpace=2 walk10no=0.
IF walkpace=5 walk10no=0.
IF age<16 walk10no=-1.
variable label walk10no '(D) Number of brisk/fast walks of 10 mins+ in last 4 weeks'.
exe.

* Minutes.

Recode tottim (0 thru 9=0) (else=copy) into tottim10.
compute mins10wlk=0.
compute mins10wlk =(tottim10*walk10no)/4.
IF tottim10=0 mins10wlk =0.
IF walk10no=-8 mins10wlk =-8.
IF walk10no=-9 mins10wlk =-9.
IF walk10no=-1 mins10wlk =-1.
variable label mins10wlk '(D) Average minutes walking of 10 mins+ per week brisk or fast - moderate'.
exe.


*Occupational.
*wrkdays wrkact21 wrkact22 wrkact23 wrkact24.
*Walking; lifting; climbing at work.

*archived variables.

compute mins10wrk08 = (hrs10wrk08 * 60).
exe.

***********.
* Sports.
***********.

compute mins10spta=0.

* Light Intensity.

IF (act11=1 OR act12=1 OR act13=1 OR act14=1 Or Act15=1 OR act16=1)  mins10spta=mins10spta+0.
IF (WhtAct05=1 AND excswt05<>1) mins10spta=mins10spta+0.
IF (WhtAct10=1 AND excswt10<>1) mins10spta=mins10spta+0.
exe.

* Moderate.

IF (WhtAct01=1 AND range(dayexc01,1,28) AND excswt01<>1 AND exctim01>=10) mins10spta=mins10spta+((dayexc01* exctim01)/4).
IF (WhtAct02=1 AND range(dayexc02,1,28) AND excswt02<>1 AND exctim02>=10) mins10spta=mins10spta+((dayexc02* exctim02)/4).
IF (WhtAct03=1 AND range(dayexc03,1,28) AND excswt03<>1 AND exctim03>=10) mins10spta=mins10spta+((dayexc03* exctim03)/4).
IF (WhtAct04=1 AND range(dayexc04,1,28) AND excswt04<>1 AND exctim04>=10) mins10spta=mins10spta+((dayexc04* exctim04)/4).
IF (WhtAct05=1 AND range(dayexc05,1,28) AND excswt05=1 AND exctim05>=10) mins10spta=mins10spta+((dayexc05* exctim05)/4).
IF (WhtAct07=1 AND range(dayexc07,1,28) AND excswt07<>1 AND exctim07>=10) mins10spta=mins10spta+((dayexc07* exctim07)/4).
IF (WhtAct08=1 AND range(dayexc08,1,28) AND excswt08<>1 AND exctim08>=10) mins10spta=mins10spta+((dayexc08* exctim08)/4).
IF (WhtAct10=1 AND range(dayexc10,1,28) AND excswt10=1 AND exctim10>=10) mins10spta=mins10spta+((dayexc10* exctim10)/4).
IF (act11=2 AND range(dayexc11,1,28) AND exctim11>=10) mins10spta=mins10spta+((dayexc11* exctim11)/4).
IF (act12=2 AND range(dayexc12,1,28) AND exctim12>=10) mins10spta=mins10spta+((dayexc12* exctim12)/4).
IF (act13=2 AND range(dayexc13,1,28) AND exctim13>=10) mins10spta=mins10spta+((dayexc13* exctim13)/4).
IF (act14=2 AND range(dayexc14,1,28) AND exctim14>=10) mins10spta=mins10spta+((dayexc14* exctim14)/4).
IF (act15=2 AND range(dayexc15,1,28) AND exctim15>=10) mins10spta=mins10spta+((dayexc15* exctim15)/4).
IF (act16=2 AND range(dayexc16,1,28) AND exctim16>=10) mins10spta=mins10spta+((dayexc16* exctim16)/4).
IF (act11=3 AND range(dayexc11,1,28) AND excswt11<>1 AND exctim11>=10) mins10spta=mins10spta+((dayexc11* exctim11)/4).
IF (act12=3 AND range(dayexc12,1,28) AND excswt12<>1 AND exctim12>=10) mins10spta=mins10spta+((dayexc12* exctim12)/4).
IF (act13=3 AND range(dayexc13,1,28) AND excswt13<>1 AND exctim13>=10) mins10spta=mins10spta+((dayexc13* exctim13)/4).
IF (act14=3 AND range(dayexc14,1,28) AND excswt14<>1 AND exctim14>=10) mins10spta=mins10spta+((dayexc14* exctim14)/4).
IF (act15=3 AND range(dayexc15,1,28) AND excswt15<>1 AND exctim15>=10) mins10spta=mins10spta+((dayexc15* exctim15)/4).
IF (act16=3 AND range(dayexc16,1,28) AND excswt16<>1 AND exctim16>=10) mins10spta=mins10spta+((dayexc16* exctim16)/4).
IF (act11=4 AND range(dayexc11,1,28) AND excswt11<>1 AND exctim11>=10) mins10spta=mins10spta+((dayexc11* exctim11)/4).
IF (act12=4 AND range(dayexc12,1,28) AND excswt12<>1 AND exctim12>=10) mins10spta=mins10spta+((dayexc12* exctim12)/4).
IF (act13=4 AND range(dayexc13,1,28) AND excswt13<>1 AND exctim13>=10) mins10spta=mins10spta+((dayexc13* exctim13)/4).
IF (act14=4 AND range(dayexc14,1,28) AND excswt14<>1 AND exctim14>=10) mins10spta=mins10spta+((dayexc14* exctim14)/4).
IF (act15=4 AND range(dayexc15,1,28) AND excswt15<>1 AND exctim15>=10) mins10spta=mins10spta+((dayexc15* exctim15)/4).
IF (act16=4 AND range(dayexc16,1,28) AND excswt16<>1 AND exctim16>=10) mins10spta=mins10spta+((dayexc16* exctim16)/4).
exe.

* Vigorous.

IF (WhtAct01=1 AND range(dayexc01,1,28) AND excswt01=1 AND exctim01>=10) mins10spta=mins10spta+(((dayexc01* exctim01)/4)*2).
IF (WhtAct02=1 AND range(dayexc02,1,28) AND excswt02=1 AND exctim02>=10) mins10spta=mins10spta+(((dayexc02* exctim02)/4)*2).
IF (WhtAct03=1 AND range(dayexc03,1,28) AND excswt03=1 AND exctim03>=10) mins10spta=mins10spta+(((dayexc03* exctim03)/4)*2).
IF (WhtAct04=1 AND range(dayexc04,1,28) AND excswt04=1 AND exctim04>=10) mins10spta=mins10spta+(((dayexc04* exctim04)/4)*2).
IF (WhtAct07=1 AND range(dayexc07,1,28) AND excswt07=1 AND exctim07>=10) mins10spta=mins10spta+(((dayexc07* exctim07)/4)*2).
IF (WhtAct08=1 AND range(dayexc08,1,28) AND excswt08=1 AND exctim08>=10) mins10spta=mins10spta+(((dayexc08* exctim08)/4)*2).
IF (WhtAct06=1 AND range(dayexc06,1,28) AND exctim06>=10) mins10spta=mins10spta+(((dayexc06* exctim06)/4)*2).
IF (WhtAct09=1 AND range(dayexc09,1,28) AND exctim09>=10) mins10spta=mins10spta+(((dayexc09* exctim09)/4)*2).
IF (act11=3 AND range(dayexc11,1,28) AND excswt11=1 AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act11=4 AND range(dayexc11,1,28) AND excswt11=1 AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act11=5 AND range(dayexc11,1,28) AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act11=6 AND range(dayexc11,1,28) AND exctim11>=10) mins10spta=mins10spta+(((dayexc11* exctim11)/4)*2).
IF (act12=3 AND range(dayexc12,1,28) AND excswt12=1 AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act12=4 AND range(dayexc12,1,28) AND excswt12=1 AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act12=5 AND range(dayexc12,1,28) AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act12=6 AND range(dayexc12,1,28) AND exctim12>=10) mins10spta=mins10spta+(((dayexc12* exctim12)/4)*2).
IF (act13=3 AND range(dayexc13,1,28) AND excswt13=1 AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act13=4 AND range(dayexc13,1,28) AND excswt13=1 AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act13=5 AND range(dayexc13,1,28) AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act13=6 AND range(dayexc13,1,28) AND exctim13>=10) mins10spta=mins10spta+(((dayexc13* exctim13)/4)*2).
IF (act14=3 AND range(dayexc14,1,28) AND excswt14=1 AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act14=4 AND range(dayexc14,1,28) AND excswt14=1 AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act14=5 AND range(dayexc14,1,28) AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act14=6 AND range(dayexc14,1,28) AND exctim14>=10) mins10spta=mins10spta+(((dayexc14* exctim14)/4)*2).
IF (act15=3 AND range(dayexc15,1,28) AND excswt15=1 AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act15=4 AND range(dayexc15,1,28) AND excswt15=1 AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act15=5 AND range(dayexc15,1,28) AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act15=6 AND range(dayexc15,1,28) AND exctim15>=10) mins10spta=mins10spta+(((dayexc15* exctim15)/4)*2).
IF (act16=3 AND range(dayexc16,1,28) AND excswt16=1 AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
IF (act16=4 AND range(dayexc16,1,28) AND excswt16=1 AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
IF (act16=5 AND range(dayexc16,1,28) AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
IF (act16=6 AND range(dayexc16,1,28) AND exctim16>=10) mins10spta=mins10spta+(((dayexc16* exctim16)/4)*2).
exe.

*Sports:maximum of 40 hours a week.

recode mins10spta (2400 thru hi = 2400).
exe.

* Overall summary = maximum of 60 hours a week.

*desc var mins10hwk  mins10man mins10wlk mins10wrk08 mins10spta.

compute mins10tot0812 =  mins10hwk  + mins10man + mins10wlk + mins10wrk08 + mins10spta.
exe.
recode mins10tot0812 (3600 thru hi = 3600).
exe.
IF any(-9,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10tot0812=-9.
IF any(-8,hswrkhm, hwrklsthm, hvyhwkhm, hvydyhm, hwtimhm) mins10tot0812=-8.
IF any(-9, gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10tot0812=-9.
IF any(-8,gardnhm, gardlistm, manwrkhm, mndayhm, diytimhm) mins10tot0812=-8.
IF any (-8,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk,tottim) mins10tot0812=-8.
IF any (-9,Wlk5it,wlk10m,daywlk,day1wlk,day2wlk, tottim) mins10tot0812=-9.
If any(-9,wrkact22,wrkact23,wrkact24,wrkdays,wkactwlk,wkactclb,wkactlft,active) mins10tot0812=-9.
If any(-8,wrkact22,wrkact23,wrkact24,wrkdays,wkactwlk,wkactclb,wkactlft,active) mins10tot0812=-8.
If any(-9,actphy) mins10tot0812=-9.
If any(-8,actphy) mins10tot0812=-8.
exe.

missing values mins10tot0812 (-9,-8,-1).
EXECUTE.

STRING point1 (A8).
COMPUTE point1 =string(psu,F7.0).
EXECUTE.

compute year=2012.
exe.

save outfile = "N:\Temp\File2012.sav"
/keep pserial sex ag16g10 wt_int year mins10hwk mins10man mins10wlk mins10spta mins10wrk08 mins10tot0812 
eqv3 bmival cigsta3 point1 genhelf2 work active.

*********.
* 2016.
**********.

dataset close all.
get file = 'N:\Hurdle Model\hse2016_eul.sav'.
rename variables (mins10wlka=mins10wlk).
exe.
select if Age16g5>=1.
exe.

STRING point1 (A8).
COMPUTE point1 =string(psu,F7.0).
EXECUTE.
compute year=2016.
exe.

save outfile = "N:\Temp\File2016.sav"
/keep seriala sex ag16g10 wt_int year mins10hwk mins10man mins10wlk mins10spta mins10wrk08 mins10tot0812 
eqv3 bmival cigsta3 point1 genhelf2 work active.

*** Put datasets together.

dataset close all.
get file = "N:\Temp\File2008.sav".
add files/file=*/file =  "N:\Temp\File2012.sav".
add files/file=*/file =  "N:\Temp\File2016.sav".
exe.

select if mins10tot0812>=0.
exe.
select if eqv3>=0.
exe.

RECODE bmival (0 thru 18.5=1)(18.5 thru 25=2)(25 thru 30=3) (30 thru hi=4) (lo thru -1=COPY) INTO bmivg5.
VARIABLE LABELS bmivg5 "(D) Valid BMI (grouped:<18.5,18.5-25,25-30,30-40 40+)".
VALUE LABELS bmivg5
1 "Under 18.5"
2 "18.5 and below 25"
3 "25 and below 30"
4 "30+".
exe.

SAVE TRANSLATE OUTFILE='N:\Analysis_Dataset.dta'
  /TYPE=STATA
  /VERSION=8
  /EDITION=SE
  /MAP
  /REPLACE.















 










