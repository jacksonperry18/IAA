/***************************************************************/
*set up stuff;
libname logreg "Z:\Desktop\Fall1\Homework1_LR";

data ins_t;
	set logreg.insurance_t;
RUN;

%let binaries = DDA DIRDEP NSF SAV ATM CD IRA LOC INV ILS MM MTG CC SDB HMOWN MOVED INAREA;
%let cont = ACCTAGE DDABAL DEP DEPAMT CASHBK CHECKS NSFAMT PHONE TELLER SAVBAL ATMAMT POS POSAMT 
CDBAL IRABAL LOCBAL INVBAL ILSBAL MMBAL MMCRED MTGBAL CCBAL CCPURC INCOME LORES HMVAL AGE CRSCORE;
%let nominal = BRANCH RES;

*get the nmissing for the data;
proc means data=ins_t n nmiss;
RUN;


/*************************************************************************/
*checking chi-square test for association and odds ratios;

*unfortunately you need to replace DDA with each item in binaries/nominals and run;
proc freq data=ins_t;
    tables DDA*INS
          / chisq expected cellchi2 nocol nopercent 
            relrisk; 
run;

/**************************************************************************/
*look at linearity for continuous;

/* Logistic Regression Model */
*again we need to replace acctage with each in cont;
proc logistic data=ins_t plots(only)=(oddsratio); 
	model INS(event='1') = ACCTAGE / clodds=pl clparm=pl;
run;
quit;

/* Checking Assumptions - Box Tidwell */
data ins_t2;
	set ins_t;
	lacctage = acctage*log(acctage);
run;

proc logistic data=ins_t2 plots(only)=(oddsratio);
	model INS(event='1') = acctage lacctage / clodds=pl clparm=pl;
run;
quit;




