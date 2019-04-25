## Chapter 1
# The packages astsa and xts are preloaded in your R environment.
# View a detailed description of AirPassengers
help(AirPassengers)

# Plot AirPassengers
plot(AirPassengers)

# Plot the DJIA daily closings
plot(djia$Close)

# Plot the Southern Oscillation Index
plot(soi)

# Plot detrended y (trend stationary)
plot(diff(y))

# Plot detrended x (random walk)
plot(diff(x))

# Plot globtemp and detrended globtemp
par(mfrow = c(2,1))
plot(globtemp) 
plot(diff(globtemp))

# Plot cmort and detrended cmort
par(mfrow = c(2,1))
plot(cmort)
plot(diff(cmort))

# astsa and xts are preloaded 

# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))

# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)
plot(diff(log(djia$Close)))

# Generate and plot white noise
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)

# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = 0.9), n = 200)
plot(MA)

# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -0.75)), n = 200)
plot(AR)

## Chapter 2
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
acf2(x)

# Fit an AR(1) to the data and examine the t-table
sarima(x, p = 1, d = 0, q = 0)

# using x <- arima.sim(model = list(order = c(2, 0, 0), ar = c(1.5, -0.75)), n = 200)
# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an AR(2) to the data and examine the t-table
sarima(x, p = 2, d = 0, q = 0)

# using x <- arima.sim(model = list(order = c(0, 0, 1), ma = -0.8), n = 100)

# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an MA(1) to the data and examine the t-table
sarima(x, p = 0, d = 0, q = 1)

# x <- arima.sim(model = list(order = c(2, 0, 1), ar = c(1, -.9), ma = .8), n = 250)
# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an ARMA(2,1) to the data and examine the t-table
sarima(x, p = 2, d = 0, q = 1)

# Fit an MA(1) to dl_varve.   
sarima(dl_varve, p = 0, d = 0, q = 1)

# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve, p = 0, d = 0, q = 2)

# Fit an ARMA(1,1) to dl_varve. Improvement?
sarima(dl_varve, p = 1, d = 0, q = 1)

# Fit an MA(1) to dl_varve. Examine the residuals  
sarima(dl_varve, p = 0, d = 0, q = 1)

# Fit an ARMA(1,1) to dl_varve. Examine the residuals
sarima(dl_varve, p = 1, d = 0, q = 1)

# Calculate approximate oil returns
oil_returns <- diff(log(oil))

# Plot oil_returns. Notice the outliers.
plot(oil_returns)

# Plot the P/ACF pair for oil_returns
acf2(oil_returns)

# Assuming both P/ACF are tailing, fit a model to oil_returns
sarima(oil_returns, p = 1, d = 0, q = 1)

## Chapter 3
# Plot x
plot(x)

# Plot the P/ACF pair of x
acf2(x)

# Plot the differenced data
plot(diff(x))

# Plot the P/ACF pair of the differenced data
acf2(diff(x))

# Plot sample P/ACF of differenced data and determine model
acf2(y)

# Estimate parameters and examine output
sarima(x, p = 2, d = 1, q = 0)

# Plot the sample P/ACF pair of the differenced data 
acf2(diff(globtemp))

# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, p = 1, d = 1, q = 1)

# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(globtemp, p = 0, d = 1, q = 2)

# Plot sample P/ACF pair of the differenced data
acf2(diff(x))

# Fit the first model, compare parameters, check diagnostics
sarima(x, p = 0, d = 1, q = 1)

# Fit the second model and compare fit
sarima(x, p = 0, d = 1, q = 2)

# Fit ARIMA(0,1,2) to globtemp and check diagnostics  
sarima(globtemp, p = 0, d = 1, q = 2)

# Fit ARIMA(1,1,1) to globtemp and check diagnostics
sarima(globtemp, p = 1, d = 1, q = 1)

# Which is the better model?
"ARIMA(0, 1, 2)"

# Plot P/ACF pair of differenced data 
acf2(diff(x))

# Fit model - check t-table and diagnostics
sarima(x, p = 1, d = 1, q = 0)

# Forecast the data 20 time periods ahead
sarima.for(x, n.ahead = 20, p = 1, d = 1, q = 0) 
lines(y) 

# Fit an ARIMA(0,1,2) to globtemp and check the fit
sarima(globtemp, p = 0, d = 1, q = 2)

# Forecast data 35 years into the future
sarima.for(globtemp, n.ahead = 35, p = 0, d = 1, q = 2)

## Chapter 4
# Seasonal ARIMA
# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)

# Fit the seasonal model to x
sarima(x, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12)

# Plot sample P/ACF pair to lag 60 and compare to actual
acf2(x, max.lag = 60)

# Fit the seasonal model to x
sarima(x, p = 0, d = 0, q = 1, P = 0, D = 0, Q = 1, S = 12)

# Plot unemp 
plot(unemp)

# Difference your data and plot it
d_unemp <- diff(unemp)
plot(d_unemp)

# Seasonally difference d_unemp and plot it
dd_unemp <- diff(d_unemp, lag = 12)  
plot(dd_unemp)

# Plot P/ACF pair of fully differenced data to lag 60
dd_unemp <- diff(diff(unemp), lag = 12)
acf2(dd_unemp, max.lag = 60)

# Fit an appropriate model
sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)

# Plot differenced chicken
plot(diff(chicken))

# Plot P/ACF pair of differenced data to lag 60
acf2(diff(chicken), max.lag = 60)

# Fit ARIMA(2,1,0) to chicken - not so good
sarima(chicken, p = 2, d = 1, q = 0)

# Fit SARIMA(2,1,0,1,0,0,12) to chicken - that works
sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)

# Birth Data
# Plot P/ACF to lag 60 of differenced data
d_birth <- diff(birth)
acf2(d_birth, max.lag = 60)

# Plot P/ACF to lag 60 of seasonal differenced data
dd_birth <- diff(d_birth, lag = 12)
acf2(dd_birth, max.lag = 60)

# Fit SARIMA(0,1,1)x(0,1,1)_12. What happens?
sarima(birth, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# Add AR term and conclude
sarima(birth, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

# Fit your previous model to unemp and check the diagnostics
sarima(unemp, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)

# Forecast the data 3 years into the future
sarima.for(unemp, n.ahead = 36, p = 2, d = 1, q = 0, P = 0, D = 1, Q = 1, S = 12)

# Fit the chicken model again and check diagnostics
sarima(chicken, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)

# Forecast the chicken data 5 years into the future
sarima.for(chicken, n.ahead = 60, p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12)
