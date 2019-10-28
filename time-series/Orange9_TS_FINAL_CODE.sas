/* Set library ts to file directory for this project */
libname ts "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project";

/* Read in the data sets */
/* PM 2.5 Daily Data */
data ts.pm2_5;
	infile "C:\Users\zachm\Documents\Fall\Module 2\Time Series II\Final Project\PM_2_5_Raleigh2.csv" dlm = ',' firstobs = 2;
	input Date	: mmddyy12. Source $ Site_ID POC Mean_PM_Concentration Units $ Daily_AQI Site_Name : $16. 
				Daily_Obs_Count Pct_Complete AQS_param_code AQS_Param_Desc : $24. State_Code State : $14.	
				County_Code County $ Site_Lat Site_Long;
run;

/* NOTE Removed all commas in the site name varibles in order to correctly read in the CSVs using this code */
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

/* Create variables for month and year in PM 2.5 Daily concentration dataset */
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
/* Each series exhibits a seasonal pattern */

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
/* ADDED VARIABLE wildfire_smoke - pulse for observation 35 when wildfire smoke blew over Wake Co. */
/* wildfire_smoke will be modeled in seasonal ARIMA and ARIMAX modeling if found to enhance the model */
data ts.monthly_train;
	set ts.monthly;
	if _n_ ge 55 then monthly_mean = .;
	if t = 35 then wildfire_smoke = 1;
	else wildfire_smoke = 0;
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
	estimate p = (1) q = (0, 12) input = (t t_sq jan feb mar apr may jun jul aug sep oct nov) method = ML;
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

/***** BEST ARIMA: ARIMA(1,0,0)(0,0,1)12 with dummy variables *****/



/* Intervention Analysis: Why is Nov 2016 spiking? */
/* Identified the week of Nov. 18 as an especially high PM 2.5 concentration week */
proc sql;
	select date, Mean_PM_Concentration, day(date)
	from ts.pm2_5
	where month(date) eq 11 and year(date) eq 2016
;
quit;


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

/* Ensemble Modeling */
/* We want to use the Additive Seasonal Model and ARIMAX Model to create predictions */
/* The forecasts have been output to WORK.ESM and WORK.ARIMAX */
/* Need to take the average of these and calculate the MAPE and MAE */
/* Subset ESM predictions to last six months */
data esm;
	set esm (keep = _timeid_ predict);
	where _timeid_ > 54;
run;
/* Get average of ESM and ARIMAX predictions for ensemble_pred */
proc sql;
	create table ensemble as
	select a.time, a.forecast, e.predict, ((a.forecast + e.predict)/2) as ensemble_pred
	from esm as e, arimax as a
	where a.time eq e._timeid_
;
quit;

data ensemble2;
	merge ensemble ts.monthly_test;
run;

data stats;
	set ensemble2 (keep = actual ensemble_pred);
	pe = abs(actual - ensemble_pred) / abs(actual) * 100;
	e = abs(actual - ensemble_pred);
run;

proc sql;
	select mean(pe) as MAPE, mean(e) as MAE
	from stats
;
quit;

/* Ensemble model is best */

/* GETTING FINAL PREDICTIONS FOR JAN 2019 - JUN 2019 USING ALL DATA */

/* Need to obtain predictions for first six months of 2019 */
/* Additive Seasonal ESM */
proc esm data = ts.monthly print = all plot = all seasonality = 12 lead = 6 outfor = esm;
	forecast monthly_mean / model = addseasonal;
run;

/* Prepare data & get predictions for Xt series for ARIMAX model so that they can be used for PM predictions */
data monthly_x;
	set ts.monthly_x (keep = t monthly_mean co_mean so_mean total_prcp avg_awnd avg_tavg);
run;

proc esm data = monthly_x print = all plot = all seasonality = 12 lead = 6 outfor = co;
	forecast co_mean / model = addwinters;
run;
proc esm data = monthly_x print = all plot = all seasonality = 12 lead = 6 outfor = so;
	forecast so_mean / model = multwinters;
run;
proc esm data = monthly_x print = all plot = all seasonality = 12 lead = 6 outfor = total_prcp;
	forecast total_prcp / model = addseasonal;
run;
proc esm data = monthly_x print = all plot = all seasonality = 12 lead = 6 outfor = avg_awnd;
	forecast avg_awnd / model = addwinters;
run;
proc esm data = monthly_x print = all plot = all seasonality = 12 lead = 6 outfor = avg_tavg;
	forecast avg_tavg / model = multwinters;
run;

proc sql;
	create table x_preds as 
	select c._timeid_, c.predict as co_mean, s.predict as so_mean, t.predict as total_prcp, w.predict as avg_awnd, a.predict as avg_tavg
	from co as c, so as s, total_prcp as t, avg_awnd as w, avg_tavg as a
	where c._timeid_ = s._timeid_ and s._timeid_ = t._timeid_ and t._timeid_ = w._timeid_ and w._timeid_ = a._timeid_
;
quit;

data x_preds;
	set x_preds;
	where _timeid_ > 60;
	t = _timeid_;
	drop _timeid_;
run;

data monthly_x_new;
	set monthly_x x_preds;
run;

proc arima data = monthly_x_new plot(unpack) = all;
	identify var = monthly_mean crosscorr = (co_mean so_mean total_prcp avg_awnd avg_tavg(1)) nlag = 24;
	estimate p = 1 input = (co_mean so_mean total_prcp avg_awnd avg_tavg) method = ML;
	forecast lead = 6;
	ods output ForecastsOnlyPlot = arimaX;
run;
quit;

/* Get average of model predictions for ENSEMBLE */
/* Subset ESM predictions to last six months */
data esm;
	set esm (keep = _timeid_ predict);
	where _timeid_ > 60;
run;
/* Get average of ESM and ARIMAX predictions for ensemble_pred */
proc sql;
	create table ensemble as
	select a.time, a.forecast, e.predict, ((a.forecast + e.predict)/2) as ensemble_pred
	from esm as e, arimax as a
	where a.time eq e._timeid_
;
quit;
