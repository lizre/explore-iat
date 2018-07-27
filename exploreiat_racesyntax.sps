* Encoding: UTF-8.
N = 5 573 014


/***************************************************************************************************************************/
/*BELOW I RECODE ALL NECESSARY VARIABLES WITH DESCRIPTIVE NAMES (WHERE NECESSARY AND NUMERIC RESPONSE (WHERE POSSIBLE)*/
/***************************************************************************************************************************/


/*IDENTIFY and remove PARTICIPANTS WITH HIGH RATES(MORE THAN 30% OR TOO MANY FAST TRIALS (MORE THAN 10%)*/

STRING  Error (A3).
IF  (pct_300<=10 & PCT_error_3467<=30)Error='No'.
IF  (pct_300>10 OR PCT_error_3467>30) Error='Yes'.
EXECUTE.

FILTER OFF.
USE ALL.
SELECT IF (Error='No').
EXECUTE.

N = 3 296 152


/*CODE EACH DAY AS DISTANCE FROM DAY 0--HERE DECEMBER 31,2006 TO LOOK AT CHANGE OVER TIME*/

COMPUTE datecode=DATEDIFF(date,DATE.DMY(31,12,2006),"days").
EXECUTE.

/*WHITTLE DATASET DOWN TO CORRECT TIME PERIOD 

FILTER OFF.
USE ALL.
SELECT IF ((year>2006)&(year<2016)).
EXECUTE.

N = 2 679 342


/***************************************************************************************************************************/
/*BELOW I RECODE ALL NECESSARY VARIABLES WITH DESCRIPTIVE NAMES (WHERE NECESSARY AND NUMERIC RESPONSE (WHERE POSSIBLE)*/
/***************************************************************************************************************************/

      RECODE sex ('m'=0) ('f'=1) (MISSING=SYSMIS) INTO sexnum.
      IF sex_5 = 1 sexnum = 0.
      IF sex_5 = 2 sexnum = 1.
      IF sex_5 = 3 sexnum = 2.
      IF sex_5 = 4 sexnum = 3. 
      IF sex_5 = 5 sexnum = 4.
      EXECUTE.

FREQUENCIES VARIABLES=sexnum
  /ORDER=ANALYSIS.

/*RECODE ALL TEXT VARIABLES INTO NUMERIC FOR DATA ANALYSIS IN REGRESSION AND FOR CREATING INTERACTIONS*/
/*Countrycit: Code as us/not us*/

RECODE countrycit ('US'=0) ('1' = 0) (MISSING=SYSMIS) (''=SYSMIS) ('.'=SYSMIS) (ELSE=1) INTO countrycitnum.
EXECUTE.

FREQUENCIES VARIABLES=countrycitnum
  /ORDER=ANALYSIS.

COMPUTE Implicit=D_biep.White_Good_all.
COMPUTE Explicit=att_7.
EXECUTE.

/*IAT SCORE AND SELF-REPORTED PREFERENCE*/

COMPUTE Implicit=D_biep.LightSkin_Good_all.
COMPUTE Explicit=att7total.
EXECUTE.

/*CODE EACH DAY AS DISTANCE FROM DAY 0--HERE DECEMBER 31,2006 TO LOOK AT CHANGE OVER TIME*/

COMPUTE datecode=DATEDIFF(date,DATE.DMY(31,12,2006),"days").
EXECUTE.

/*RECODE CONTINUOUS DEMOGRAPHICS VARIABLES TO NUMERIC*/
/*getting rid of some weird numbers in the data*/

RECODE politicalid (-999=SYSMIS) (MISSING=SYSMIS).
COMPUTE politics=politicalid.
COMPUTE religiosity=religionid.
RECODE num ('0'=1) ('1'=2) ('2'=3) ('3-5'=4) ('6+'=5) INTO numiats.
EXECUTE.

/*RECODE ALL TEXT VARIABLES INTO NUMERIC FOR DATA ANALYSIS IN REGRESSION AND FOR CREATING INTERACTIONS*/
/*Countrycit: Code as us/not us*/

RECODE countrycit ('US'=0) (MISSING=SYSMIS) (''=SYSMIS) ('.'=SYSMIS) (ELSE=1) INTO countrycitnum.
EXECUTE.

FREQUENCIES VARIABLES=countrycitnum
  /ORDER=ANALYSIS.


/*RECODE CONTINUOUS DEMOGRAPHICS VARIABLES TO NUMERIC*/
/*getting rid of some weird numbers in the data*/

RECODE politicalid (-999=SYSMIS) (MISSING=SYSMIS).
COMPUTE politics=politicalid.
COMPUTE religiosity=religionid.
RECODE num ('0'=1) ('1'=2) ('2'=3) ('3-5'=4) ('6+'=5) INTO numiats.
EXECUTE.


/*Education. Recode into continuous variable*/

RECODE edu_14 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (11=10) (12=10) (13=10) (14=9) (MISSING=SYSMIS) INTO education.
RECODE edustudent (1=1) (2=1) (3=3) (4=5) (5=5) (6=8) (7=8) (8=8) (9=8) (10=8) (11=8) (MISSING=SYSMIS) INTO education2.
RECODE edunotstudent (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (11=10) (12=10) (13=10) (14=9) (MISSING=SYSMIS) INTO education3.
EXECUTE.

/*Recode*/

COMPUTE ethnicitynum=ethnicityomb.
COMPUTE racenum=raceomb.
COMPUTE ordernum=order.
EXECUTE.


/*WHITTLE DATASET DOWN TO CORRECT TIME PERIOD AND DELETE SESSIONS THAT WERE NOT COMPLETED OR THAT ARE ENTIRELY MISSING KEY ATTITUDES VARIABLES*/

DATASET COPY Race IAT.public.2002-2015.
DATASET ACTIVATE  raceiatimpexp.
FILTER OFF.
USE ALL.
SELECT IF (~SYSMIS(Implicit))&(~SYSMIS(Explicit)).
EXECUTE.

N = 2 219 014

        RECODE edu_14 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (11=10) (12=10) (13=10) (14=9) (MISSING=SYSMIS) INTO education.
        RECODE edunotstudent (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (11=10) (12=10) (13=10) (14=9) (MISSING=SYSMIS) INTO education_temp1.
        RECODE edustudent (1=1) (2=1) (3=3) (4=5) (5=5) (6=8) (7=8) (8=8) (9=8) (10=8) (11=8) (MISSING=SYSMIS) INTO education_temp2.
        IF education_temp1 > -999 education = education_temp1.
        IF education_temp2 > -999 education = education_temp2.
        EXECUTE.
        
FILTER OFF.
USE ALL.
SELECT IF (session_status='C').
EXECUTE.

N = 1 620 789


FILTER OFF.
USE ALL.
SAMPLE  .005.
EXECUTE.

N = 7983
