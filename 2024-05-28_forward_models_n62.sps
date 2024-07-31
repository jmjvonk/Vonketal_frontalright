﻿* Encoding: UTF-8.

*Get data file.
GET DATA  /TYPE=TXT
  /FILE="C:\ucsf_docs\Jet\sbvPPA_analysis\analysis\SPSS\2024-05-06_subset_features.csv"
  /ENCODING='UTF8'
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /LEADINGSPACES IGNORE=YES
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
 PIDN AUTO
 DATE AUTO
 DX AUTO
 AGE_AT_PICNIC AUTO
 HAND AUTO
 GENDER AUTO
 GENDER_LABEL AUTO
 EDUC AUTO
 RACE AUTO
 RACE_LABEL AUTO
 MODTRAILS_TIME AUTO
 BENSON_MODREY AUTO
 BENSON_REY10M AUTO
 DIGITBW AUTO
 STROOPCOR AUTO
 VOSP_NUMBLOC AUTO
 MMSE_TOT AUTO
 CDR_BOXSCORE AUTO
 CVLT_CORR30 AUTO
 CVLT_CORR10 AUTO
 NPI_SCORE AUTO
 BNT_CORR AUTO
 ADP_TOTAL_WORDS_RATIO AUTO
 AROUSAL_NOUNS AUTO
 AROUSAL_NVAA AUTO
 AVERAGE_SYLL_PAUSE_DURATION AUTO
 AVG_T_UNIT_LEN AUTO
 AVG_VERB_PHRASE_TUNIT AUTO
 CONTENT_FUNCTION_RATIO AUTO
 CONTENT_UNITS AUTO
 CONTENT_WORDS AUTO
 IDEA_DENSITY AUTO
 INFORMATIVENESS AUTO
 NOUN_VERB_RATIO AUTO
 NOUNS_FREQ_MEAN AUTO
 NUM_PAUSES_75MS AUTO
 NUM_PAUSES_REG AUTO
 NUM_REG_PAUSES_STAND AUTO
 NUM_RESTARTS AUTO
 NVAA_ACQ_MEAN AUTO
 NVAA_AMB_MEAN AUTO
 NVAA_CONCRETE_MEAN AUTO
 NVAA_FAM_PREV_MEAN AUTO
 NVAA_FREQ_MEAN AUTO
 PAUSE_RATE AUTO
 PCDC_Z AUTO
 PCNAR_Z AUTO
 THINGS_RATIO AUTO
 TOTAL_WORDS AUTO
 VALENCE_NOUNS AUTO
 VALENCE_NVAA AUTO
  /MAP.
CACHE.
EXECUTE.

DATASET NAME new_data_CLASP WINDOW=FRONT.

* Define Variable Properties.
 VARIABLE LEVEL PIDN(NOMINAL).
 VARIABLE LEVEL DX(NOMINAL).
 VARIABLE LEVEL AGE_AT_PICNIC(SCALE).
 VARIABLE LEVEL HAND(NOMINAL).
 VARIABLE LEVEL GENDER(NOMINAL).
 VARIABLE LEVEL  GENDER_LABEL(NOMINAL).
 VARIABLE LEVEL EDUC(SCALE).
 VARIABLE LEVEL RACE(NOMINAL).
 VARIABLE LEVEL RACE_LABEL(NOMINAL).
 VARIABLE LEVEL MODTRAILS_TIME(SCALE).
 VARIABLE LEVEL  BENSON_MODREY(SCALE).
 VARIABLE LEVEL BENSON_REY10M(SCALE).
 VARIABLE LEVEL  DIGITBW(SCALE).
 VARIABLE LEVEL STROOPCOR(SCALE).
 VARIABLE LEVEL  VOSP_NUMBLOC(SCALE).
 VARIABLE LEVEL MMSE_TOT(SCALE).
 VARIABLE LEVEL CDR_BOXSCORE(SCALE).
 VARIABLE LEVEL  CVLT_CORR30(SCALE).
 VARIABLE LEVEL CVLT_CORR10(SCALE).
 VARIABLE LEVEL  NPI_SCORE(SCALE).
 VARIABLE LEVEL  BNT_CORR(SCALE).
 VARIABLE LEVEL ADP_TOTAL_WORDS_RATIO(SCALE).
 VARIABLE LEVEL AROUSAL_NOUNS(SCALE).
 VARIABLE LEVEL AROUSAL_NVAA(SCALE).
 VARIABLE LEVEL AVERAGE_SYLL_PAUSE_DURATION(SCALE).
 VARIABLE LEVEL AVG_T_UNIT_LEN(SCALE).
 VARIABLE LEVEL AVG_VERB_PHRASE_TUNIT(SCALE).
 VARIABLE LEVEL CONTENT_FUNCTION_RATIO(SCALE).
 VARIABLE LEVEL CONTENT_UNITS(SCALE).
 VARIABLE LEVEL CONTENT_WORDS(SCALE).
 VARIABLE LEVEL IDEA_DENSITY(SCALE).
 VARIABLE LEVEL INFORMATIVENESS(SCALE).
 VARIABLE LEVEL NOUN_VERB_RATIO(SCALE).
 VARIABLE LEVEL NOUNS_FREQ_MEAN(SCALE).
 VARIABLE LEVEL NUM_PAUSES_75MS(SCALE).
 VARIABLE LEVEL NUM_PAUSES_REG(SCALE).
 VARIABLE LEVEL NUM_REG_PAUSES_STAND(SCALE).
 VARIABLE LEVEL NUM_RESTARTS(SCALE).
 VARIABLE LEVEL NVAA_ACQ_MEAN(SCALE).
 VARIABLE LEVEL NVAA_AMB_MEAN(SCALE).
 VARIABLE LEVEL NVAA_CONCRETE_MEAN(SCALE).
 VARIABLE LEVEL NVAA_FAM_PREV_MEAN(SCALE).
 VARIABLE LEVEL NVAA_FREQ_MEAN(SCALE).
 VARIABLE LEVEL PAUSE_RATE(SCALE).
 VARIABLE LEVEL PCDC_Z(SCALE).
 VARIABLE LEVEL PCNAR_Z(SCALE).
 VARIABLE LEVEL THINGS_RATIO(SCALE).
 VARIABLE LEVEL TOTAL_WORDS(SCALE).
 VARIABLE LEVEL VALENCE_NOUNS(SCALE).
 VARIABLE LEVEL VALENCE_NVAA(SCALE).

*Set -999 as missing value for all numeric cols.
RECODE
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NOUNS
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION
 AVG_T_UNIT_LEN
 AVG_VERB_PHRASE_TUNIT
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 CONTENT_WORDS
 IDEA_DENSITY
 INFORMATIVENESS
 NOUN_VERB_RATIO
 NOUNS_FREQ_MEAN
 NUM_PAUSES_75MS
 NUM_PAUSES_REG
 NUM_REG_PAUSES_STAND
 NUM_RESTARTS
 NVAA_ACQ_MEAN
 NVAA_AMB_MEAN
 NVAA_CONCRETE_MEAN
 NVAA_FAM_PREV_MEAN
 NVAA_FREQ_MEAN
 PAUSE_RATE
 PCDC_Z
 PCNAR_Z
 THINGS_RATIO
 TOTAL_WORDS
 VALENCE_NOUNS
 VALENCE_NVAA
 (-999=SYSMIS).
EXECUTE.

*Create dummy variables for dx groups.
RECODE DX ('HC'=0) ('rATL'=1) ('Frontal'=2) INTO DX_3.
VARIABLE LABELS  DX_3 '0, 1, 2'.
EXECUTE.

*Remove participants without MMSE scores. 
SELECT IF NOT(MISSING(MMSE_TOT)).
*Remove participants with less than 25 total words (too impaired).
SELECT IF TOTAL_WORDS >= 25. 
*Remove HC 9119 who has major outliers across several variables.
SELECT IF PIDN ~= 9119.

*Descriptives.
CROSSTABS
  /TABLES=DX BY GENDER_LABEL
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

*Custom Tables.
CTABLES
  /VLABELS VARIABLES=AGE_AT_PICNIC EDUC MMSE_TOT TOTAL_WORDS DX DISPLAY=LABEL
  /TABLE AGE_AT_PICNIC [MEAN] + EDUC [MEAN] BY DX
  /CATEGORIES VARIABLES=DX ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.

*Check who is missing covariates.
MEANS TABLES=EDUC MMSE_TOT GENDER AGE_AT_PICNIC TOTAL_WORDS
  /CELLS=MEAN COUNT STDDEV.

FREQUENCIES VARIABLES=MMSE_TOT
  /ORDER=ANALYSIS.

*Calculate new variable - NUM_T_UNITS.
COMPUTE NUM_T_UNITS = TOTAL_WORDS / AVG_T_UNIT_LEN.
EXECUTE.

*Calculate z-score per group.
SORT CASES  BY DX_3.
SPLIT FILE LAYERED BY DX_3.

*Descriptive statistics on chosen speech measures.
DESCRIPTIVES VARIABLES=
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION
 AVG_T_UNIT_LEN
 AVG_VERB_PHRASE_TUNIT
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 CONTENT_WORDS
 INFORMATIVENESS
 NOUN_VERB_RATIO
 NUM_PAUSES_75MS
 NUM_RESTARTS
 NUM_T_UNITS
 NVAA_ACQ_MEAN
 NVAA_AMB_MEAN
 NVAA_CONCRETE_MEAN
 NVAA_FAM_PREV_MEAN
 NVAA_FREQ_MEAN
 PAUSE_RATE
 THINGS_RATIO
 VALENCE_NVAA
  /SAVE
  /STATISTICS=MEAN STDDEV MIN MAX.

SPLIT FILE OFF.

*Identify outliers per feature within each group (values >=2.5 SDs from the group's mean).
FREQUENCIES VARIABLES=
 ZADP_TOTAL_WORDS_RATIO
 ZAROUSAL_NVAA
 ZAVERAGE_SYLL_PAUSE_DURATION
 ZAVG_T_UNIT_LEN
 ZAVG_VERB_PHRASE_TUNIT
 ZCONTENT_FUNCTION_RATIO
 ZCONTENT_UNITS
 ZCONTENT_WORDS
 ZINFORMATIVENESS
 ZNOUN_VERB_RATIO
 ZNUM_PAUSES_75MS
 ZNUM_RESTARTS
 ZNUM_T_UNITS
 ZNVAA_ACQ_MEAN
 ZNVAA_AMB_MEAN
 ZNVAA_CONCRETE_MEAN
 ZNVAA_FAM_PREV_MEAN
 ZNVAA_FREQ_MEAN
 ZPAUSE_RATE
 ZTHINGS_RATIO
 ZVALENCE_NVAA
  /ORDER=ANALYSIS.

*Outliers within Dx group
*ZADP_TOTAL_WORDS_RATIO = none.
*ZAROUSAL_NVAA = none.

*ZAVERAGE_SYLL_PAUSE_DURATION: 
11773 (3.02381)
6867 (3.98101).

*ZAVG_T_UNIT_LEN: 
20458 (3.05514)
2522 (3.25159)

*ZAVG_VERB_PHRASE_TUNIT:
2522 (3.57220)

 *ZCONTENT_FUNCTION_RATIO: 
28704 (3.16940).

*ZCONTENT_UNITS = none.

*ZCONTENT_WORDS: 28704 (3.37815).

*ZINFORMATIVENESS = none.

*ZNOUN_VERB_RATIO: 
2679 (3.26955)
19694 (3.45635).

*ZNUM_PAUSES_75MS:
23393 (3.22329)
6970 (4.20606).

*ZNUM_RESTARTS
8542 (2.54605)
18304 (2.74642)
25178 (2.79966)

*ZNUM_T_UNITS: 
14062 (3.15607).

*ZNVAA_ACQ_MEAN
16825 (2.71988).

*ZNVAA_AMB_MEAN
8704 (-3.36030).

*ZNVAA_CONCRETE_MEAN
9283 (2.53444).

*ZNVAA_FAM_PREV_MEAN
8704 (-3.47430)
10177 (-2.58392)

 *ZNVAA_FREQ_MEAN
17797 (2.556671429)
17020 (2.582771999)
25977 (2.588962069)
14062 (2.627782099)
18304 (2.629392593)
30130 (2.658907468)
30180 (2.658952281)
28619 (2.661907407)
6600 (2.696270297)
18137 (2.775400000)
14731 (2.899020833).

*ZPAUSE_RATE: 
20458 (3.65594)
6970 (3.86997)
23393 (4.16144)

*ZTHINGS_RATIO: 
9333 (2.80233).

*ZVALENCE_NVAA
10177 (-3.24181)
9606 (2.96975)

*Run GLM to get residuals to check normality assumption.
GLM
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION
 AVG_T_UNIT_LEN
 AVG_VERB_PHRASE_TUNIT
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 CONTENT_WORDS
 INFORMATIVENESS
 NOUN_VERB_RATIO
 NUM_PAUSES_75MS
 NUM_RESTARTS
 NUM_T_UNITS
 NVAA_ACQ_MEAN
 NVAA_AMB_MEAN
 NVAA_CONCRETE_MEAN
 NVAA_FAM_PREV_MEAN
 NVAA_FREQ_MEAN
 PAUSE_RATE
 THINGS_RATIO
 VALENCE_NVAA
 BY DX_3 WITH AGE_AT_PICNIC GENDER EDUC MMSE_TOT
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=RESID
  /EMMEANS=TABLES(DX_3) WITH(AGE_AT_PICNIC=MEAN GENDER=MEAN EDUC=MEAN MMSE_TOT=MEAN) COMPARE 
    ADJ(LSD)
  /CRITERIA=ALPHA(.05)
  /DESIGN=AGE_AT_PICNIC GENDER EDUC MMSE_TOT DX_3.


*Check for approximate normal distributions.
GRAPH
  /HISTOGRAM(NORMAL)=RES_1.
GRAPH
  /HISTOGRAM(NORMAL)=RES_2.
GRAPH
  /HISTOGRAM(NORMAL)=RES_3.
GRAPH
  /HISTOGRAM(NORMAL)=RES_4.
GRAPH
  /HISTOGRAM(NORMAL)=RES_5.
GRAPH
  /HISTOGRAM(NORMAL)=RES_6.
GRAPH
  /HISTOGRAM(NORMAL)=RES_7.
GRAPH
  /HISTOGRAM(NORMAL)=RES_8.
GRAPH
  /HISTOGRAM(NORMAL)=RES_9.
GRAPH
  /HISTOGRAM(NORMAL)=RES_10.
GRAPH
  /HISTOGRAM(NORMAL)=RES_11.
GRAPH
  /HISTOGRAM(NORMAL)=RES_12.
GRAPH
  /HISTOGRAM(NORMAL)=RES_13.
GRAPH
  /HISTOGRAM(NORMAL)=RES_14.
GRAPH
  /HISTOGRAM(NORMAL)=RES_15.
GRAPH
  /HISTOGRAM(NORMAL)=RES_16.
GRAPH
  /HISTOGRAM(NORMAL)=RES_17.
GRAPH
  /HISTOGRAM(NORMAL)=RES_18.
GRAPH
  /HISTOGRAM(NORMAL)=RES_19.
GRAPH
  /HISTOGRAM(NORMAL)=RES_20.
GRAPH
  /HISTOGRAM(NORMAL)=RES_21.

*Check min values of features to be transformed.
DESCRIPTIVES VARIABLES=
 AVERAGE_SYLL_PAUSE_DURATION
 AVG_T_UNIT_LEN
 AVG_VERB_PHRASE_TUNIT
 NOUN_VERB_RATIO
 NUM_PAUSES_75MS
 NUM_RESTARTS
 NUM_T_UNITS
 NVAA_FAM_PREV_MEAN
 NVAA_FREQ_MEAN
 PAUSE_RATE
 /STATISTICS=MEAN STDDEV MIN MAX.

*Log transform count variables to get approx normality.
COMPUTE AVERAGE_SYLL_PAUSE_DURATION_LOG = LG10(AVERAGE_SYLL_PAUSE_DURATION).
EXECUTE.

COMPUTE AVG_T_UNIT_LEN_LOG = LG10(AVG_T_UNIT_LEN).
EXECUTE.

COMPUTE AVG_VERB_PHRASE_TUNIT_LOG = LG10(AVG_VERB_PHRASE_TUNIT).
EXECUTE.

COMPUTE NOUN_VERB_RATIO_LOG = LG10(NOUN_VERB_RATIO).
EXECUTE.

COMPUTE NUM_PAUSES_75MS_LOG = LG10(NUM_PAUSES_75MS).
EXECUTE.

COMPUTE NUM_RESTARTS_LOG = LG10(NUM_RESTARTS+1).
EXECUTE.

COMPUTE NUM_T_UNITS_LOG = LG10(NUM_T_UNITS).
EXECUTE.

COMPUTE NVAA_FAM_PREV_MEAN_LOG = LG10(NVAA_FAM_PREV_MEAN).
EXECUTE.

COMPUTE NVAA_FREQ_MEAN_LOG = LG10(NVAA_FREQ_MEAN).
EXECUTE.

COMPUTE PAUSE_RATE_LOG = LG10(PAUSE_RATE).
EXECUTE.

*New GLM to check new residuals distribution.
GLM
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 AVG_VERB_PHRASE_TUNIT_LOG
 NOUN_VERB_RATIO_LOG
 NUM_PAUSES_75MS_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 BY DX_3 WITH AGE_AT_PICNIC GENDER EDUC MMSE_TOT
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /SAVE=RESID
  /EMMEANS=TABLES(DX_3) WITH(AGE_AT_PICNIC=MEAN GENDER=MEAN EDUC=MEAN MMSE_TOT=MEAN) COMPARE 
    ADJ(LSD)
  /CRITERIA=ALPHA(.05)
  /DESIGN=AGE_AT_PICNIC GENDER EDUC MMSE_TOT DX_3.

*Check for approximate normal distributions.
GRAPH
  /HISTOGRAM(NORMAL)=RES_22.
GRAPH
  /HISTOGRAM(NORMAL)=RES_23.
GRAPH
  /HISTOGRAM(NORMAL)=RES_24.
GRAPH
  /HISTOGRAM(NORMAL)=RES_25.
GRAPH
  /HISTOGRAM(NORMAL)=RES_26.
GRAPH
  /HISTOGRAM(NORMAL)=RES_27.
GRAPH
  /HISTOGRAM(NORMAL)=RES_28.
GRAPH
  /HISTOGRAM(NORMAL)=RES_29.
GRAPH
  /HISTOGRAM(NORMAL)=RES_30.
GRAPH
  /HISTOGRAM(NORMAL)=RES_31.

*HC VS DEMENTIA ANALYSIS.
*Create two different outcome groups (HC vs DEM, Right vs Front).
RECODE DX ('HC'=0) ('rATL'=1) ('Frontal'=1) INTO DX_BI.
VARIABLE LABELS  DX_BI '0,1'.
EXECUTE.

*Check for multicollinearity.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT DX_BI
  /METHOD=ENTER
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 AVG_VERB_PHRASE_TUNIT_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 CONTENT_WORDS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_PAUSES_75MS_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_AMB_MEAN
 NVAA_CONCRETE_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA.

*Descriptives of remaining independent variables.
DESCRIPTIVES VARIABLES=
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
 /STATISTICS=MEAN STDDEV MIN MAX.

*---------------------------------------------------------------------------------------------------------.
*REGRESSION MODELS - Controls vs Dementia.
*----------------------------------------------------------------------------------------------------------.
*Box-Tidwell transformations.

COMPUTE ADP_TOTAL_WORDS_RATIO_TR = 1+abs(0)+ADP_TOTAL_WORDS_RATIO.
EXECUTE.
    
COMPUTE AVERAGE_SYLL_PAUSE_DURATION_LOG_TR = 1+abs(-.22)+AVERAGE_SYLL_PAUSE_DURATION_LOG.
EXECUTE.

COMPUTE NOUN_VERB_RATIO_LOG_TR = 1+abs(-.18)+NOUN_VERB_RATIO_LOG.
EXECUTE.
    
COMPUTE NUM_RESTARTS_LOG_TR = 1+abs(0)+NUM_RESTARTS_LOG.
EXECUTE.

COMPUTE THINGS_RATIO_TR = 1+abs(0)+THINGS_RATIO.
EXECUTE.

*Check that new var has min = 1.00.
DESCRIPTIVES VARIABLES= 
 ADP_TOTAL_WORDS_RATIO_TR
 AVERAGE_SYLL_PAUSE_DURATION_LOG_TR
 NOUN_VERB_RATIO_LOG_TR
 NUM_RESTARTS_LOG_TR
 THINGS_RATIO_TR
  /STATISTICS=MEAN STDDEV MIN MAX.

*Run Box-Tidwell Test for all features.
COMPUTE ADP_TOTAL_WORDS_RATIO_BOX = LN(ADP_TOTAL_WORDS_RATIO_TR)*ADP_TOTAL_WORDS_RATIO_TR.
EXECUTE.

COMPUTE AROUSAL_NVAA_BOX = LN(AROUSAL_NVAA)*AROUSAL_NVAA.
EXECUTE.

COMPUTE AVERAGE_SYLL_PAUSE_DURATION_LOG_BOX = LN(AVERAGE_SYLL_PAUSE_DURATION_LOG_TR)*AVERAGE_SYLL_PAUSE_DURATION_LOG_TR.
EXECUTE.

COMPUTE AVG_T_UNIT_LEN_LOG_BOX = LN(AVG_T_UNIT_LEN_LOG)*AVG_T_UNIT_LEN_LOG.
EXECUTE.
  
COMPUTE CONTENT_FUNCTION_RATIO_BOX = LN(CONTENT_FUNCTION_RATIO)*CONTENT_FUNCTION_RATIO.
EXECUTE.

COMPUTE  CONTENT_UNITS_BOX = LN(CONTENT_UNITS)*CONTENT_UNITS.
EXECUTE.

COMPUTE INFORMATIVENESS_BOX = LN(INFORMATIVENESS)*INFORMATIVENESS.
EXECUTE.

COMPUTE NOUN_VERB_RATIO_LOG_BOX = LN(NOUN_VERB_RATIO_LOG_TR)*NOUN_VERB_RATIO_LOG_TR.

COMPUTE NUM_RESTARTS_LOG_BOX = LN(NUM_RESTARTS_LOG_TR)*NUM_RESTARTS_LOG_TR.
EXECUTE.

COMPUTE NUM_T_UNITS_LOG_BOX = LN(NUM_T_UNITS_LOG)*NUM_T_UNITS_LOG.
EXECUTE.

COMPUTE NVAA_ACQ_MEAN_BOX = LN(NVAA_ACQ_MEAN)*NVAA_ACQ_MEAN.
EXECUTE.

COMPUTE NVAA_FAM_PREV_MEAN_LOG_BOX = LN(NVAA_FAM_PREV_MEAN_LOG)*NVAA_FAM_PREV_MEAN_LOG.
EXECUTE.

COMPUTE NVAA_FREQ_MEAN_BOX = LN(NVAA_FREQ_MEAN)*NVAA_FREQ_MEAN.
EXECUTE.

COMPUTE PAUSE_RATE_LOG_BOX = LN(PAUSE_RATE_LOG)*PAUSE_RATE_LOG.
EXECUTE.

COMPUTE THINGS_RATIO_BOX = LN(THINGS_RATIO_TR)*THINGS_RATIO_TR.
EXECUTE.

COMPUTE VALENCE_NVAA_BOX = LN(VALENCE_NVAA)*VALENCE_NVAA.
EXECUTE.


*Testing Box-Tidwell (controls vs dementia patients).
LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER 
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
 ADP_TOTAL_WORDS_RATIO_BOX
 AROUSAL_NVAA_BOX
 AVERAGE_SYLL_PAUSE_DURATION_LOG_BOX
 AVG_T_UNIT_LEN_LOG_BOX
 CONTENT_FUNCTION_RATIO_BOX
 CONTENT_UNITS_BOX
 INFORMATIVENESS_BOX
 NOUN_VERB_RATIO_LOG_BOX
 NUM_RESTARTS_LOG_BOX
 NUM_T_UNITS_LOG_BOX
 NVAA_ACQ_MEAN_BOX
 NVAA_FAM_PREV_MEAN_LOG_BOX
 NVAA_FREQ_MEAN_BOX
 PAUSE_RATE_LOG_BOX
 THINGS_RATIO_BOX
 VALENCE_NVAA_BOX
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

*-----------------------------------------------------------------------------------------------------------.
*SINGLE PREDICTOR MODELS.
*Regression models for controls vs dementia partipants.
*Each variable as only predictor variable.
*First one is a test of predictive power of covariates. Not including MMSE score as covariate in these models as it is a clinical measure.

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /SAVE=PRED
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

RENAME VARIABLES (PRE_1 = PRED_HCvDEM_COV).

ROC PRED_HCvDEM_COV BY DX_BI (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER ADP_TOTAL_WORDS_RATIO
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER AROUSAL_NVAA
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER AVERAGE_SYLL_PAUSE_DURATION_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER AVG_T_UNIT_LEN_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER CONTENT_FUNCTION_RATIO
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER CONTENT_UNITS
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER INFORMATIVENESS
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER NOUN_VERB_RATIO_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER NUM_RESTARTS_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER NUM_T_UNITS_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER NVAA_ACQ_MEAN
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER NVAA_FAM_PREV_MEAN_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER NVAA_FREQ_MEAN_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER PAUSE_RATE_LOG
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER THINGS_RATIO
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

*-----------------------------------------------------------------------------------------------------------.
*FORWARD MODEL (FEATURE SELECTION).

*PIN = 0.01.
LOGISTIC REGRESSION VARIABLES DX_BI
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /CRITERIA=PIN(0.01) POUT(0.01) ITERATE(20) CUT(0.5).

*PIN = 0.05.
LOGISTIC REGRESSION VARIABLES DX_BI
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /CRITERIA=PIN(0.05) POUT(0.05) ITERATE(20) CUT(0.5).

*PIN = 0.1.
LOGISTIC REGRESSION VARIABLES DX_BI
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC 
  /CRITERIA=PIN(0.1) POUT(0.1) ITERATE(20) CUT(0.5).
*-----------------------------------------------------------------------------------------------------------------------------.
*ENTER MODEL (.05).

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER
  CONTENT_UNITS
  NVAA_FAM_PREV_MEAN_LOG
  NVAA_FREQ_MEAN_LOG
  /SAVE=PRED
  /CRITERIA=PIN(0.1) POUT(0.2) ITERATE(20) CUT(0.5).

RENAME VARIABLES (PRE_1 = PRED_HCvDEM_ENTER).

ROC PRED_HCvDEM_ENTER BY DX_BI (0)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(SMALL) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER
  CONTENT_UNITS
  NVAA_FAM_PREV_MEAN_LOG
  NVAA_FREQ_MEAN_LOG
  /METHOD=ENTER AGE_AT_PICNIC 
  /PRINT=GOODFIT
  /SAVE=PRED
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

RENAME VARIABLES (PRE_1 = PRED_HCvDEM_ENTERwCOV).

ROC PRED_HCvDEM_ENTERwCOV BY DX_BI (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

*-----------------------------------------------------------------------------------------------------------------------------.
*CDR.
LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER CDR_BOXSCORE 
  /SAVE=PRED
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

RENAME VARIABLES (PRE_1 = PRED_CDR_NoCOV).

ROC PRED_CDR_NoCOV BY DX_BI (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

LOGISTIC REGRESSION VARIABLES DX_BI
  /METHOD=ENTER CDR_BOXSCORE 
  /METHOD=ENTER AGE_AT_PICNIC GENDER EDUC
  /SAVE=PRED
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

RENAME VARIABLES (PRE_1 = PRED_CDR_COV).

ROC PRED_CDR_COV BY DX_BI (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(LARGE) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

*------------------------------------------------------------------------------------------------------------------------------.
*REGRESSION MODELS - rATL vs Frontal.
*----------------------------------------------------------------------------------------------------------.
*Change outcome variable to only include rights and frontals.
USE ALL.
COMPUTE filter_$=((DX_3 = 1) OR (DX_3 = 2)).
VARIABLE LABELS filter_$ '(DX_3 = 1) OR (DX_3 = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Descriptives of remaining independent variables.
DESCRIPTIVES VARIABLES=
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
 /STATISTICS=MEAN STDDEV MIN MAX.

*------------------------------------------------------------------------------------------------------------.
*SINGLE PREDICTOR MODELS.
*Regression models for controls vs dementia partipants.
*Each variable as only predictor variable.
LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /SAVE=PRED
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

RENAME VARIABLES (PRE_1 = PRED_RvF_COV).

ROC PRED_RvF_COV BY DX_3 (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(SMALL) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER ADP_TOTAL_WORDS_RATIO
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER AROUSAL_NVAA
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER AVERAGE_SYLL_PAUSE_DURATION_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER AVG_T_UNIT_LEN_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER CONTENT_FUNCTION_RATIO
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER CONTENT_UNITS
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER INFORMATIVENESS
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER NOUN_VERB_RATIO_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER NUM_RESTARTS_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER NUM_T_UNITS_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER NVAA_ACQ_MEAN
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER NVAA_FAM_PREV_MEAN_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER NVAA_FREQ_MEAN_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER PAUSE_RATE_LOG
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER THINGS_RATIO
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /CRITERIA=PIN(0.05) POUT(0.10) ITERATE(20) CUT(0.5).

*------------------------------------------------------------------------------------------------------------.
*FORWARD MODELS (FEATURE SELECTION).

*PIN = 0.01.
LOGISTIC REGRESSION VARIABLES DX_3
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /CRITERIA=PIN(0.01) POUT(0.01) ITERATE(20) CUT(0.5).

*PIN = 0.05.
LOGISTIC REGRESSION VARIABLES DX_3
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /CRITERIA=PIN(0.05) POUT(0.05) ITERATE(20) CUT(0.5).

*PIN = 0.1.
LOGISTIC REGRESSION VARIABLES DX_3
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /CRITERIA=PIN(0.1) POUT(0.1) ITERATE(20) CUT(0.5).

*------------------------------------------------------------------------------------------------------------.
*ENTER MODELS (0.1).

*Not including informativeness.
LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER
  ADP_TOTAL_WORDS_RATIO
  AROUSAL_NVAA
  AVERAGE_SYLL_PAUSE_DURATION_LOG
  NUM_RESTARTS_LOG
  THINGS_RATIO
  /SAVE=PRED
  /CRITERIA=PIN(0.1) POUT(0.2) ITERATE(20) CUT(0.5).

RENAME VARIABLES (PRE_1 = PRED_RvF_ENTER).

ROC PRED_RvF_ENTER BY DX_3 (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(SMALL) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER
  ADP_TOTAL_WORDS_RATIO
  AROUSAL_NVAA
  AVERAGE_SYLL_PAUSE_DURATION_LOG
  NUM_RESTARTS_LOG
  THINGS_RATIO
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /PRINT=GOODFIT
  /SAVE=PRED
  /CRITERIA=PIN(0.1) POUT(0.2) ITERATE(20) CUT(0.5).

RENAME VARIABLES (PRE_1 = PRED_RvF_ENTERwCOV).

ROC PRED_RvF_ENTERwCOV BY DX_3 (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(SMALL) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

*------------------------------------------------------------------------------------------------------------.
*BNT.
LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER BNT_CORR
  /SAVE=PRED
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

RENAME VARIABLES (PRE_1 = PRED_BNT_NoCOV).

ROC PRED_BNT_NoCOV BY DX_3 (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(SMALL) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.

LOGISTIC REGRESSION VARIABLES DX_3
  /METHOD=ENTER BNT_CORR
  /METHOD=ENTER AGE_AT_PICNIC MMSE_TOT
  /SAVE=PRED
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20) CUT(.5).

RENAME VARIABLES (PRE_1 = PRED_BNT_COV).

ROC PRED_BNT_COV BY DX_3 (1)
  /PLOT=CURVE(REFERENCE)
  /PRINT=SE COORDINATES
  /CRITERIA=CUTOFF(INCLUDE) TESTPOS(SMALL) DISTRIBUTION(FREE) CI(95)
  /MISSING=EXCLUDE.


*------------------------------------------------------------------------------------------------------------------------------.
*REGRESSION MODELS - Control vs Frontal.
*----------------------------------------------------------------------------------------------------------.
*Change outcome variable to only include controls and frontals.
USE ALL.
COMPUTE filter_$=((DX_3 = 0) OR (DX_3 = 2)).
VARIABLE LABELS filter_$ '(DX_3 = 0) OR (DX_3 = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*PIN = 0.1.
LOGISTIC REGRESSION VARIABLES DX_3
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC 
  /CRITERIA=PIN(0.1) POUT(0.2) ITERATE(20) CUT(0.5).


*------------------------------------------------------------------------------------------------------------------------------.
*REGRESSION MODELS - Control vs rATL.
*----------------------------------------------------------------------------------------------------------.
*Change outcome variable to only include controls and rights.
USE ALL.
COMPUTE filter_$=((DX_3 = 0) OR (DX_3 = 1)).
VARIABLE LABELS filter_$ '(DX_3 = 0) OR (DX_3 = 1) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*PIN = 0.1.
LOGISTIC REGRESSION VARIABLES DX_3
 /METHOD=FSTEP(COND)
 ADP_TOTAL_WORDS_RATIO
 AROUSAL_NVAA
 AVERAGE_SYLL_PAUSE_DURATION_LOG
 AVG_T_UNIT_LEN_LOG
 CONTENT_FUNCTION_RATIO
 CONTENT_UNITS
 INFORMATIVENESS
 NOUN_VERB_RATIO_LOG
 NUM_RESTARTS_LOG
 NUM_T_UNITS_LOG
 NVAA_ACQ_MEAN
 NVAA_FAM_PREV_MEAN_LOG
 NVAA_FREQ_MEAN_LOG
 PAUSE_RATE_LOG
 THINGS_RATIO
 VALENCE_NVAA
  /METHOD=ENTER AGE_AT_PICNIC
  /CRITERIA=PIN(0.05) POUT(0.1) ITERATE(20) CUT(0.5).


SAVE OUTFILE='C:\ucsf_docs\Jet\sbvPPA_analysis\analysis\SPSS\2024-05-10_forward_models.sav'
  /COMPRESSED.
