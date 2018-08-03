* Encoding: UTF-8.
/*IMPORT DATA FROM SPSS FILE FROM OSF*/
ALTER TYPE ALL(A=AMIN).
DATASET NAME OriginalData WINDOW=FRONT.

/*WHITTLE DATASET DOWN TO CORRECT TIME PERIOD AND DELETE SESSIONS THAT WERE NOT COMPLETED OR THAT ARE ENTIRELY MISSING KEY ATTITUDES VARIABLES*/
DATASET ACTIVATE  AGEIAT_Analyze.
FILTER OFF.
USE ALL.
SELECT IF ((year>2006)&(~SYSMIS(D_biep.Young_Good_all))&(~SYSMIS(att_7))).
EXECUTE.

/***************************************************************************************************************************/
/*BELOW I RECODE ALL NECESSARY VARIABLES WITH DESCRIPTIVE NAMES (WHERE NECESSARY AND NUMERIC RESPONSE (WHERE POSSIBLE)*/
/***************************************************************************************************************************/

/*IAT SCORE AND SELF-REPORTED PREFERENCE*/

DATASET ACTIVATE AGEIAT_Analyze.
COMPUTE Explicit=att_7.
RECODE sex ('m'=0) ('f'=1) INTO sexnum.
COMPUTE Implicit=D_biep.Young_Good_all.
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

RECODE politicalid (-999=SYSMIS) (MISSING=SYSMIS).
COMPUTE politics=politicalid.
COMPUTE religiosity=religionid.
RECODE num ('0'=1) ('1'=2) ('2'=3) ('3-5'=4) ('6+'=5) INTO numiats.
EXECUTE.

/*RECODE ALL TEXT VARIABLES INTO NUMERIC FOR DATA ANALYSIS IN REGRESSION AND FOR CREATING INTERACTIONS*/
/*Countrycit: Code as us/not us*/

RECODE countrycit ('US'=0) (MISSING=SYSMIS) (''=SYSMIS) ('.'=SYSMIS) (ELSE=1) INTO countrycitnum.
RECODE sex ('m'=0) ('f'=1) (MISSING=SYSMIS) INTO sexnum.
RECODE edu_14 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (11=10) (12=10) (13=10) (14=9) (MISSING=SYSMIS) INTO education.
EXECUTE.

/*Age. Recode into groups*/
RECODE age (0 thru 20=1) (21 thru 30=2) (31 thru 40=3) (41 thru 50=4) (51 thru 60=5) (61 thru
    200=6) INTO Age_group.
EXECUTE.


COMPUTE ethnicitynum=ethnicityomb.
COMPUTE racenum=raceomb.
COMPUTE ordernum=order.
EXECUTE.

/*CREATE AND SAVE SMALLER DATASET (THAT RUNS FASTER) WITH ONLY RELEVANT VARIABLES--YOU WILL NEED TO CHANGE DIRECTORY NAME*/

SAVE OUTFILE='/Users/lizredford/Downloads/ageiat_cleaned.sav'
/Keep session_id implicit explicit error datecode ethnicitynum datecode racenum state ordernum countrycitnum edu numiats sexnum politics religiosity age choosetobe feel hopetolive othersthink education age_group year.

