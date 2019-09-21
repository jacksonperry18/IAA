
/*****************/
/*create the data set and create a numeric time variable */
/*****************/
data import1;
	set import1;
	t = input(var1, 10.);
RUN;

/****************/
/*run a linear regression on the raw monthly data, output residuals */
/****************/
proc reg data=import1;
	model x = t;
	output residual=r;
RUN;
QUIT;


/****************/
/*run the ADF test to assess stationarity and check for white noise */
/****************/
proc arima data=data2 plot(unpack)=all;
	identify var=r nlag=10 stationarity=(adf=2);
	estimate method=ML;
run;
quit;