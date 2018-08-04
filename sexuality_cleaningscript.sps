* Encoding: UTF-8.
/*IMPORT DATA FROM SPSS FILE FROM OSF*/

GET FILE 'Sexuality IAT.public.2004-2016.sav'.
ALTER TYPE ALL(A=AMIN).
SELECT IF (year < 2016). 
EXECUTE.
DATASET NAME OriginalData WINDOW=FRONT.

/*WHITTLE DATASET DOWN TO CORRECT TIME PERIOD AND DELETE SESSIONS THAT WERE NOT COMPLETED OR THAT ARE ENTIRELY MISSING KEY ATTITUDES VARIABLES*/

SAVE OUTFILE = "SexualityIAT_analyze.sav".
FILTER OFF.
USE ALL.
SELECT IF ((year>2006)&(~SYSMIS(D_biep.Straight_Good_all))&(~SYSMIS(att_7))).
EXECUTE.

/***************************************************************************************************************************/
/*BELOW I RECODE ALL NECESSARY VARIABLES WITH DESCRIPTIVE NAMES (WHERE NECESSARY AND NUMERIC RESPONSE (WHERE POSSIBLE)*/
/***************************************************************************************************************************/

/*IAT SCORE AND SELF-REPORTED PREFERENCE*/

COMPUTE Implicit=D_biep.Straight_Good_all.
COMPUTE Explicit=att_7.
EXECUTE.

**for the Sexuality IAT there was a change in 2011-2012 where "3" was Transgender MTF" and "4" was Transgender FTM"**

FREQUENCIES sex. 
RECODE sex ('m'=0) ('f'=1) ('3' = 2) ('4' = 3) INTO sexnum.
FREQUENCIES sexnum.
EXECUTE.

/*IDENTIFY and remove PARTICIPANTS WITH HIGH RATES(MORE THAN 30% OR TOO MANY FAST TRIALS (MORE THAN 10%)*/

STRING  Error (A3).
IF  (pct_300<=10 & PCT_error_3467<=30)Error='No'.
IF  (pct_300>10 OR PCT_error_3467>30) Error='Yes'.
EXECUTE.

FILTER OFF.
USE ALL.
SELECT IF (Error='No').
EXECUTE.

/*CODE EACH DAY AS DISTANCE FROM DAY 0--HERE DECEMBER 31,2006 TO LOOK AT CHANGE OVER TIME*/

COMPUTE datecode=DATEDIFF(date,DATE.DMY(31,12,2006),"days").
EXECUTE.

/*RECODE CONTINUOUS DEMOGRAPHICS VARIABLES TO NUMERIC*/
/*getting rid of some weird numbers in the data*/

**politicalid should not be used instead use politicalid_7 UNLESS you are only using data from after 2/6/2006 (in this case)**

RECODE politicalid (-999=SYSMIS) (MISSING=SYSMIS).
EXECUTE.

COMPUTE politics = politicalid.
EXECUTE.
COMPUTE religiosity=religionid.
RECODE num ('0'=1) ('1'=2) ('2'=3) ('3-5'=4) ('6+'=5) INTO numiats.
EXECUTE.


COMPUTE sexualityall = sexuality_3new. 
IF (sexuality_4 = 1) sexualityall = 1. 
IF (sexuality_4 = 2) sexualityall = 2.
IF (sexuality_4 = 3) sexualityall = 3.
IF (sexuality_4 = 4) sexualityall = 4.
IF (sexuality_5 = 1) sexualityall = 1.
IF (sexuality_5 = 2) sexualityall = 2.
IF (sexuality_5 = 3) sexualityall = 3.
IF (sexuality_5 = 4) sexualityall = 4.
IF (sexuality_5 = 5) sexualityall = 5.
EXECUTE. 


VARIABLE LABELS 
sexualityall "sexuality_3new, sexuality_4, and sexuality_5 combined". 

VALUE LABELS
sexualityall
1 "Heterosexual, straight"
2 "Gay or Lesbian"
3 "Bisexual"
4 "Asexual"
5 "Questioning" .

/*RECODE ALL TEXT VARIABLES INTO NUMERIC FOR DATA ANALYSIS IN REGRESSION AND FOR CREATING INTERACTIONS*/
/*Countrycit: Code as us/not us*/

RECODE countrycit_num (1=0) (MISSING=SYSMIS) (ELSE = 1) INTO countrycitnum_2.
RECODE countrycit ('US'=0) (MISSING=SYSMIS) (''=SYSMIS) ('.'=SYSMIS) (ELSE=1) INTO countrycitnum_1.

COMPUTE countrycitnum = countrycitnum_1.
IF (countrycitnum_2 = 0) countrycitnum = 0. 
IF (countrycitnum_2 = 1) countrycitnum = 1. 
EXECUTE.

/*Education. Recode into continuous variable*/

RECODE edu_14 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (11=10) (12=10) (13=10) (14=9) (MISSING=SYSMIS) INTO edu_1.
RECODE edu_14_2011 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=9) (11=10) (12=10) (13=10) (14=10) (MISSING=SYSMIS) INTO edu_2.
Compute education=mean(edu_1, edu_2).

COMPUTE ethnicitynum=ethnicityomb.
COMPUTE racenum=raceomb.
COMPUTE ordernum=order.
COMPUTE stimulinum = stimuli. 
EXECUTE.

/*CREATE AND SAVE SMALLER DATASET (THAT RUNS FASTER) WITH ONLY RELEVANT VARIABLES--YOU WILL NEED TO CHANGE DIRECTORY NAME*/

SAVE OUTFILE='sexuality_cleaned.sav'
/Keep=session_id session_status implicit explicit error datecode ethnicitynum racenum state ordernum stimulinum countrycitnum education numiats sexnum politics religiosity age year sexualityall marriagerights_3 contactmet contactfriendlyregular.

