libname ts "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project";

/* Read in the data sets */
/* PM 2.5 Daily Data */
data ts.pm2_5;
	infile "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project\PM_2_5_Raleigh2.csv" dlm = ',' firstobs = 2;
	input Date	: mmddyy12. Source $ Site_ID POC Mean_PM_Concentration Units $ Daily_AQI Site_Name : $16. 
				Daily_Obs_Count Pct_Complete AQS_param_code AQS_Param_Desc : $24. State_Code State : $14.	
				County_Code County $ Site_Lat Site_Long;
run;

/* NOTE Remove all commas in the site name varibles in order to correctly read in the CSVs using this code */
/* NO Daily Data */
data ts.no;
	infile "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project\NO_Raleigh.csv" dlm = ',' firstobs = 2;
	input Date	: mmddyy12. Source $ Site_ID POC Max_NO Units $ Daily_AQI Site_Name : $16. 
				Daily_Obs_Count Pct_Complete AQS_param_code AQS_Param_Desc : $24. CBSA_Code CBSA_Name $10.
				State_Code State : $14.	County_Code County $ Site_Lat Site_Long;
run;

/* CO Daily Data */
data ts.co;
	infile "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project\CO_Raleigh.csv" dlm = ',' firstobs = 2;
	input Date	: mmddyy12. Source $ Site_ID POC Max_CO Units $ Daily_AQI Site_Name : $16. 
				Daily_Obs_Count Pct_Complete AQS_param_code AQS_Param_Desc : $24. CBSA_Code CBSA_Name $10.
				State_Code State : $14.	County_Code County $ Site_Lat Site_Long;
run;

/* SO2 Daily Data */
data ts.so;
	infile "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project\SO2_Raleigh.csv" dlm = ',' firstobs = 2;
	input Date	: mmddyy12. Source $ Site_ID POC Max_SO Units $ Daily_AQI Site_Name : $16. 
				Daily_Obs_Count Pct_Complete AQS_param_code AQS_Param_Desc : $24. CBSA_Code CBSA_Name $10.
				State_Code State : $14.	County_Code County $ Site_Lat Site_Long;
run;

/* Daily Weather Data from RDU Airport */
data ts.weather;
	infile "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project\Weatherdata.csv" dlm = ',' firstobs = 2;
	input STATION : $11. NAME : $23. DATE : mmddyy12. AWND PRCP SNOW SNWD TAVG TMAX TMIN WSF2 WSF5 WT01;
run;

/* Create variables for month and year in Raleigh weather dataset */
data ts.weather;
	set ts.weather;
	month = month(date);
	year = year(date);
run;

/* Create variables for month and year in PM 2.5 Dailly concentration dataset */
data ts.monthagg;
	set ts.pm2_5;
	month = month(date);
	year = year(date);
run;

/* Create monthly aggregation table for PM 2.5 concentration dataset */
proc sql;
create table ts.monthly as
select month, year, avg(mean_pm_concentration) as monthly_mean
	from ts.monthagg
	group by year, month
;
quit;

/* Aggregate additional chemical concentration series (CO, NO, SO2) by month & year concentration */
%macro agg(table);
	data ts.agg;
		set ts.&table;
		month = month(date);
		year = year(date);
	run;

	proc sql;
	create table ts.&table._monthly as
		select month, year, avg(Max_&table) as monthly_mean
		from ts.&table
		group by year, month
	;
	quit;
	
	data ts.&table._monthly;
		set ts.&table._monthly;
		t = _n_;
	run;
%mend;
%agg(no)
%agg(co)
%agg(so)

/* Aggregate weather data series (SUM (prcp, snwd, snow) MAX (tmax, wsf2, wsf5) MEAN (awnd, tavg) MIN (tmin) */
proc sql;
	create table ts.weather_monthly as
	select sum(prcp) as total_prcp, sum(snwd) as total_snwd, sum(snow) as total_snow, sum(wt01) as sum_fog,
				 max(tmax) as max_tmax, max(wsf2) as max_wsf2, max(wsf5) as max_wsf5,
				 avg(awnd) as avg_awnd, avg(tavg) as avg_tavg, min(tmin) as min_tmin
	from ts.weather
	group by year, month
;
quit;

/* Add a 't' variable for observation number over time */
data ts.weather_monthly;
	set ts.weather_monthly;
	t = _n_;
run;

/* Plot each Xt series before modeling */
%macro plot(table);
	proc sgplot data = ts.&table._monthly;
		series x = t y = monthly_mean;
	run;
%mend;
%plot(no)
%plot(co)
%plot(so)

/* Create time dummy variables and prepare for quadratic regression model */
data ts.monthly;
	set ts.monthly;
	if month = 1 then jan = 1; else jan = 0;
	if month = 2 then feb = 1; else feb = 0;
	if month = 3 then mar = 1; else mar = 0;
	if month = 4 then apr = 1; else apr = 0;
	if month = 5 then may = 1; else may = 0;
	if month = 6 then jun = 1; else jun = 0;
	if month = 7 then jul = 1; else jul = 0;
	if month = 8 then aug = 1; else aug = 0;
	if month = 9 then sep = 1; else sep = 0;
	if month = 10 then oct = 1; else oct = 0;
	if month = 11 then nov = 1; else nov = 0;
	t = _n_;
	t_sq = t * t;
	actual = monthly_mean;
run;

/* Create monthly_train dataset with missing values for the last six months to build the model */
/* This will be used to calculate MAPE & MAE on forecasts for ARIMA since BACK = does not hold out */
data ts.monthly_train;
	set ts.monthly;
	if _n_ ge 55 then monthly_mean = .;
run;

/* Perform ESM on PM 2.5 data set and output forecasts to WORK.TEST */
/* Linear ESM */
proc esm data = ts.monthly print = all plot = all seasonality = 12 lead = 6 back = 6 outfor = test;
	forecast monthly_mean / model = linear;
run;
/* Additive Seasonal ESM */
proc esm data = ts.monthly print = all plot = all seasonality = 12 lead = 6 back = 6 outfor = test;
	forecast monthly_mean / model = addseasonal;
run;
/* Multiplicative Seasonal ESM */
proc esm data = ts.monthly print = all plot = all seasonality = 12 lead = 6 back = 6 outfor = test;
	forecast monthly_mean / model = multseasonal;
run;
/* Additive Holt Winters ESM */
proc esm data = ts.monthly print = all plot = all seasonality = 12 lead = 6 back = 6 outfor = test;
	forecast monthly_mean / model = addwinters;
run;
/* Multiplicative Holt Winters ESM */
proc esm data = ts.monthly print = all plot = all seasonality = 12 lead = 6 back = 6 outfor = test;
	forecast monthly_mean / model = multwinters;
run;

	/* MAPE calculation for ESM */
data test2;
	set test;
	if _n_ > 54;
	abs_error = abs(error);
	abs_err_obs = abs_error / abs(actual);
run;
proc sql;
	select mean(abs_error) as MAE, mean(abs_err_obs) as MAPE
	from test2
;
quit;

/***** BEST ESM -- ADDITIVE SEASONAL *****/

/* ARIMA Modeling */
/* ARIMA(0,0,0) */
proc arima data = ts.monthly_train;
	identify var = monthly_mean stationarity = (adf = 2);
	estimate method = ML;
run;
quit;
/* Fit a quadratic regression line to remove trend */
proc reg data = ts.monthly_train outest = estimates;
	model monthly_mean = t t_sq;
	output out = ts.resid residual = residuals;
run;
quit;

	/* Test stationarity of the series with trend removed */
proc arima data = ts.resid plot(unpack) = all;
	identify var = residuals stationarity = (adf = 2);
	estimate method = ML;
run;
quit; /* quadratic residuals are stationary */

	/* ARIMA model with trend removed */
	/* Automatic selection of ARMA terms */
proc arima data = ts.monthly_train plot(unpack) = all;
	identify var = monthly_mean esacf p = (0 : 12) q = (0: 12) crosscorr = (t t_sq) nlag = 12;
	estimate input = (t t_sq) method = ML;
run;
quit;
		/* MINIC - ARMA(6,2) */
		/* SCAN - ARMA(0,1) */
		/* ESACF - (2,0), (0,1), (1,1) */
	/* Trying the ARMA(1,0) */
proc arima data = ts.monthly_train plot(unpack) = all;
	identify var = monthly_mean crosscorr = (t t_sq) nlag = 12;
	estimate p = (1) q = (0) input = (t t_sq) method = ML;
run;
quit;
	/* Trying the ARMA(1,1) */
proc arima data = ts.monthly_train plot(unpack) = all;
	identify var = monthly_mean crosscorr = (t t_sq) nlag = 12;
	estimate p = (1) q = (1) input = (t t_sq) method = ML;
run;
quit;
	/* Trying the ARMA(0,1) */
proc arima data = ts.monthly_train plot(unpack) = all;
	identify var = monthly_mean crosscorr = (t t_sq) nlag = 12;
	estimate p = (0) q = (1) input = (t t_sq) method = ML;
	forecast lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;
quit;
		/* Overall best model: Quadratic trend ARIMA(0,1) */

/* SEASONAL ARIMA */
proc arima data = ts.monthly_train plot(unpack) = all;
	identify var = monthly_mean crosscorr = (t t_sq jan feb mar apr may jun jul aug sep oct nov) nlag = 12;
	estimate p = (1) q = (0, 12, 13) input = (t t_sq jan feb mar apr may jun jul aug sep oct nov) method = ML;
run;
quit;

proc arima data = ts.monthly_train plot(unpack) = all;
	identify var = monthly_mean crosscorr = (t t_sq jan feb mar apr may jun jul aug sep oct nov) nlag = 12;
	estimate p = (1) q = (0)(12, 13) input = (t t_sq jan feb mar apr may jun jul aug sep oct nov) method = ML;
	forecast lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;
quit;
%mape(ts, forecasts, monthly_test)

/* Create validation set to be used when calculating MAPE */
data ts.monthly_test;
	set ts.monthly_train;
	where monthly_mean = .;
run;

/* Define macro 'mape' to output MAPE and MAE calculations for forecasts of last six months in data */
%macro mape(libname, forecasts, test);
	data &&libname..&forecasts;
		merge &&libname..&forecasts &&libname..&test;
	run;

	data &libname..stats;
		set &&libname..&forecasts (keep = actual forecast);
		pe = abs(actual - forecast) / abs(actual) * 100;
		e = abs(actual - forecast);
	run;

	proc sql;
		select mean(pe) as MAPE, mean(e) as MAE
		from &libname..stats
	;
	quit;
%mend;

/***** BEST ARIMA 
/* ARIMAX */
/* Merge X series into monthly_train data set for ARIMA modeling */
proc sql;
create table ts.monthly_train_x as
	select m.*, s.monthly_mean as SO_mean, n.monthly_mean as NO_mean, c.monthly_mean as CO_mean, w.*
	from ts.monthly_train as m, ts.so_monthly as s, ts.no_monthly as n, ts.co_monthly as c, ts.weather_monthly as w
	where m.t = s.t and s.t = n.t and n.t = c.t and c.t = w.t
	order by t
;
quit;
	/* Test correlations between all varaibles in the new time series and monthly_mean*/
proc corr data = ts.monthly_train_x plots = matrix;
	var total_snow monthly_mean;
run;

	/* Check stationarity of the Xt series (CO, NO, SO) */
%macro stationarity(var);
	title "Test of Stationarity for &var series";
	proc arima data = ts.monthly_train_x;
		identify var = &var stationarity = (adf = 2);
	run;
	quit;
	title;
%mend;

%stationarity(co) /* Stationary */
%stationarity(no)	/* Stationary */
%stationarity(so)	/* Stationary */
%stationarity(total_prcp) /* Stationary */
%stationarity(avg_awnd) /* Stationary */
%stationarity(avg_tavg) /* Not stationary - difference */
%stationarity(total_snwd) /* Stationarity */

/* Starting with ARIMAX(0,0,0) and all x variables in the model. Then removed insignificant regressors
NO_mean and TOTAL_snwd. Added 1 AR term due to PACF and IACF plots. Achieved white noise, proceeded to
test model on validation set - MAPE 13.81556 MAE 1.366893  */
/* Best White Noise!! */
/* Allowed for trend and season to be modeled by Xt series */
proc arima data = ts.monthly_train_x plot(unpack) = all;
	identify var = monthly_mean crosscorr = (co_mean so_mean total_prcp avg_awnd avg_tavg(1)) nlag = 24;
	estimate p = 1 input = (co_mean so_mean total_prcp avg_awnd avg_tavg) method = ML;
	forecast lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;
quit;

%mape(ts, forecasts, monthly_test)

/* Slightly better MAPE, but this ARIMA may be over fit and does not really make sense */
proc arima data = ts.monthly_train_x plot(unpack) = all;
	identify var = monthly_mean crosscorr = (t t_sq co_mean no_mean) nlag = 24;
	estimate p = (2) q = (1) input = (t t_sq /(1) co_mean no_mean) method = ML;
	forecast lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;
quit;
%mape(ts, forecasts, monthly_test) /* MAPE 12.86969 MAE 1.272522 */

/* UCM */
	/* First iteration */
proc ucm data = ts.monthly;
	level;
	season length = 12 type = trig;
	irregular;
	model monthly_mean;
	estimate plot = (acf pacf wn);
	forecast back = 6 lead = 6;
run;

proc ucm data = ts.monthly;
	level;
	season length = 12 type = dummy;
	irregular q = 1;
	model monthly_mean;
	estimate plot = (acf pacf wn);
	forecast back = 6 lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;

/* Slope & Cycle terms found to be not significant in the first iteration. Hence dropped */
/* MAPE - 14.05 */
proc sql;
create table ts.monthly_x as
	select m.*, s.monthly_mean as SO_mean, n.monthly_mean as NO_mean, c.monthly_mean as CO_mean, w.*
	from ts.monthly as m, ts.so_monthly as s, ts.no_monthly as n, ts.co_monthly as c, ts.weather_monthly as w
	where m.t = s.t and s.t = n.t and n.t = c.t and c.t = w.t
	order by t
;
quit;

proc ucm data = ts.monthly_x;
	season length = 12 type = trig;
	irregular p = 2;
	estimate plot=(acf pacf wn); 
	model monthly_mean = co_mean;
	forecast back = 6 lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;

proc ucm data = ts.monthly_x;
	season length = 12 type = trig;
	irregular p = 2 sq = 1;
	estimate plot=(acf pacf wn); 
	model monthly_mean = co_mean so_mean total_prcp avg_awnd avg_tavg;
	forecast back = 6 lead = 6;
	ods output ForecastsOnlyPlot = ts.forecasts;
run;

data ts.stats;
	set ts.forecasts (keep = actual predict);
	pe = abs(actual - predict) / abs(actual) * 100;
	e = abs(actual - predict);
run;

proc sql;
	select mean(pe) as MAPE, mean(e) as MAE
	from ts.stats
;
quit;
