libname ts "Z:/Desktop/Fall 2/TS";

/*import the data*/
proc import datafile="Z:/Desktop/Fall 2/TS/train.csv" out=ts.training;
RUN;

proc import datafile="Z:/Desktop/Fall 2/TS/test.csv" out=ts.testing;
RUN;

proc import datafile="Z:/Desktop/Fall 2/TS/TS_proj_agg.csv" out=ts.full;
RUN;

/*plot the data*/
proc sgplot data=ts.training;
series x=var1 y=PM25;
run;
quit;

/* Fitting a level, slope and season component */
proc ucm data=ts.training;
level;
season length=12 type=trig;
irregular;
model PM25;
run;


 /* Make slope and season deterministic */
proc ucm data=ts.training;
level;
slope variance = 0 noest;
season length=12 type=trig variance=0 noest;
irregular;
model PM25;
run;

/* Print smoothed plots */
proc ucm data=ts.training;
level plot=smooth;
slope variance = 0 noest plot=smooth;
season length=12 type=trig variance=0 noest plot=smooth;
irregular plot=smooth;
model PM25;
run;

/* Forecast */
proc ucm data=ts.full ;
level;
slope variance=0 noest;
season length=12 type=trig variance=0 noest;
irregular P=9;
estimate plot=(acf pacf wn); 
model PM25 = CO WSF2 NO;
forecast back=6 lead=6;
run;

/*2019 Forecast */
proc ucm data=ts.full ;
level;
slope variance=0 noest;
season length=12 type=trig variance=0 noest;
irregular P=9;
estimate plot=(acf pacf wn); 
model PM25 = CO WSF2 NO;
forecast lead=6 outfor=fore;
run;


