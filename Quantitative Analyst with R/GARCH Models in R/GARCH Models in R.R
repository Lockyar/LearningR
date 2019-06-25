## Chapter 1

library(xts)
library(PerformanceAnalytics)

# Plot daily S&P 500 prices
plot(sp500prices)

# Compute daily returns
sp500ret <- CalculateReturns(sp500prices)

# Check the class of sp500ret
class(sp500ret)

# Plot daily returns
plot(sp500ret)

# Compute the daily standard deviation for the complete sample   
sd(sp500ret)

# Compute the annualized volatility for the complete sample
sqrt(252) * sd(sp500ret)

# Compute the annualized standard deviation for the year 2009 
sqrt(252) * sd(sp500ret["2009"])

# Compute the annualized standard deviation for the year 2017 
sqrt(252) * sd(sp500ret["2017"])

# Roll, roll, roll
# Load the package PerformanceAnalytics
library(PerformanceAnalytics)

# Showing two plots on the same figure
par(mfrow=c(2,1)) 

# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000::2017"], width = 22,
     FUN = "sd.annualized", scale = 252, main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000::2017"], width = 66,
     FUN = "sd.annualized", scale = 252, main = "Three months rolling volatility")

# Prediction errors
# Compute the mean daily return
m <- mean(sp500ret)

# Define the series of prediction errors
e <- sp500ret - m

# Plot the absolute value of the prediction errors
par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
plot(abs(e))

# Plot the acf of the absolute prediction errors
acf(abs(e))

# Compute the predicted variances
predvar[1] <- var(sp500ret) 
# nobs is number of observations in sp500ret
for(t in 2:nobs){
   # omega, alpha, beta are predetermined while predvar is a vector of that only contains a first number, the rest are NA's of the number of observations in nobs.
   # e2 is the e^2 of the last exercise.
   predvar[t] <- omega + alpha * e2[t-1] + beta * predvar[t-1]
}

# Create annualized predicted volatility
ann_predvol <- xts(sqrt(252) * sqrt(predvar), order.by = time(sp500ret))

# Plot the annual predicted volatility in 2008 and 2009
plot(ann_predvol["2008::2009"], main = "Ann. S&P 500 vol in 2008-2009")

# Specify a standard GARCH model with constant mean
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                 variance.model = list(model = "sGARCH"), 
                 distribution.model = "norm")

# Estimate the model
garchfit <- ugarchfit(data = sp500ret, spec = garchspec)

# Use the method sigma to retrieve the estimated volatilities 
garchvol <- sigma(garchfit)

# Plot the volatility for 2017
plot(garchvol["2017"])

# Compute unconditional volatility
sqrt(uncvariance(garchfit))

# Print last 10 ones in garchvol
tail(garchvol, n = 10)

# Forecast volatility 5 days ahead and add 
garchforecast <- ugarchforecast(fitORspec = garchfit, 
                     n.ahead = 5)

# Extract the predicted volatilities and print them
print(sigma(garchforecast))

# Compute the annualized volatility
annualvol <- sqrt(252) * sigma(garchfit)

# Compute the 5% vol target weights  
vt_weights <- 0.05 / annualvol

# Compare the annualized volatility to the portfolio weights in a plot
plot(merge(annualvol, vt_weights), multi.panel = TRUE)

## Chapter 2
# Plot the return series
 plot(ret)

# Specify the garch model to be used
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH"),
                        distribution.model = "sstd")

# Estimate the model
garchfit <- ugarchfit(data = ret, spec = garchspec)

# Inspect the coefficients
coef(garchfit)

# Standardized returns
# Compute the standardized returns
stdret <- residuals(garchfit, standardize = TRUE)

# Compute the standardized returns using fitted() and sigma()
stdret <- (ret - fitted(garchfit)) / sigma(garchfit)

# Load the package PerformanceAnalytics and make the histogram
library(PerformanceAnalytics)
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"))

# Estimation of GJR garch model
# Specify the GJR GARCH model
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                 variance.model = list(model = "gjrGARCH"),
                 distribution.model = "sstd")

# Estimate the model and compute volatility
gjrgarchfit <- ugarchfit(data = msftret, spec = garchspec)
gjrgarchvol <- sigma(gjrgarchfit)

# Compare volatility
plotvol <- plot(abs(msftret), col = "grey")
plotvol <- addSeries(gjrgarchvol, col = "red", on=1)
plotvol <- addSeries(sgarchvol, col = "blue", on=1)
plotvol

# The AR(1)-GJR GARCH dynamics of MSFT returns
# Specify AR(1)-GJR GARCH model
garchspec <- ugarchspec(mean.model = list(armaOrder = c(1, 0) ),
                        variance.model = list(model = "gjrGARCH"),
                        distribution.model = "sstd")

# Estimate the model
garchfit <- ugarchfit(data = msftret, spec = garchspec)

# Print the first two coefficients
coef(garchfit)[c(1:2)]

# Effect of mean model on volatility predictions
# GARCH-in-Mean specification and estimation
gim_garchspec <- ugarchspec( 
  mean.model = list(armaOrder = c(0,0), archm = TRUE, archpow = 2),
  variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")
gim_garchfit <- ugarchfit(data = msftret , spec = gim_garchspec)

# Predicted mean returns and volatility of GARCH-in-mean
gim_mean <- fitted(gim_garchfit)
gim_vol <- sigma(gim_garchfit)

# Correlation between predicted return using AR(1) and GARCH-in-mean models
cor(ar1_mean, gim_mean)

# Correlation between predicted volatilities across mean.models
cor(merge(constmean_vol, ar1_vol, gim_vol))

# Fixing GARCH parameters
# Print the flexible GARCH parameters
coef(flexgarchfit)

# Restrict the flexible GARCH model by impose a fixed ar1 and skew parameter
rflexgarchspec <- flexgarchspec
setfixed(rflexgarchspec) <- list(ar1 = 0, skew = 1)

# Estimate the restricted GARCH model
rflexgarchfit <- ugarchfit(data = EURUSDret,  spec = rflexgarchspec)

# Compare the volatility of the unrestricted and restriced GARCH models
plotvol <- plot(abs(EURUSDret), col = "grey")
plotvol <- addSeries(sigma(flexgarchfit), col = "black", lwd = 4, on=1 )
plotvol <- addSeries(sigma(rflexgarchfit), col = "red", on=1)
plotvol

# Parameter bounds and impact on forecasts
# Define bflexgarchspec as the bound constrained version
bflexgarchspec <- flexgarchspec
setbounds(bflexgarchspec) <- list(alpha1 = c(0.05, 0.2), beta1 = c(0.8, 0.95))

# Estimate the bound constrained model
bflexgarchfit <- ugarchfit(data = EURUSDret, spec = bflexgarchspec)

# Inspect coefficients
coef(bflexgarchfit)

# Compare forecasts for the next ten days
cbind(sigma(ugarchforecast(flexgarchfit, n.ahead = 10)),
      sigma(ugarchforecast(bflexgarchfit, n.ahead = 10)))

# Variance targeting
# Complete the specification to do variance targeting
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH",
                                              variance.targeting = TRUE),
                        distribution.model = "std")

# Estimate the model
garchfit <- ugarchfit(data = EURUSDret, spec = garchspec)

# Print the GARCH model implied long run volatility
sqrt(uncvariance(garchfit))

# Verify that it equals the standard deviation (after rounding)
all.equal(sqrt(uncvariance(garchfit)), sd(EURUSDret), tol = 1e-4)

## Chapter 3
# Analyzing estimation output
# Specify model with AR(1) dynamics, GJR GARCH and skewed student t
flexgarchspec <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                            variance.model = list(model = "gjrGARCH"),
                            distribution.model = "sstd")

# Estimate the model
flexgarchfit <- ugarchfit(data = EURUSDret, spec = flexgarchspec)

# Complete and study the statistical significance of the estimated parameters  
round(flexgarchfit@fit$matcoef,6)

# A better model for EUR/USD returns
# Specify model with constant mean, standard GARCH and student t
tgarchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                         variance.model = list(model = "sGARCH", variance.targeting = TRUE),
                         distribution.model = "std")

# Fix the mu parameter at zero
 setfixed(tgarchspec) <- list("mu" = 0)

# Estimate the model
tgarchfit <- ugarchfit(data = EURUSDret, spec = tgarchspec)

# Verify that the differences in volatility are small
plot(sigma(tgarchfit) - sigma(flexgarchfit))

# Mean squared prediction errors
# Compute prediction errors
garcherrors <- residuals(garchfit)
gjrerrors  <- residuals(gjrfit)

# Compute MSE for variance prediction of garchfit model
mean((sigma(garchfit)^2 - garcherrors^2)^2)

# Compute MSE for variance prediction of gjrfit model
mean((sigma(gjrfit)^2 - gjrerrors^2)^2)

# Comparing likelihood and information criteria
# Print the number of estimated parameters
length(coef(garchfit))
length(coef(gjrfit))

# Print likelihood of the two models
likelihood(garchfit)
likelihood(gjrfit)

# Print the information criteria of the two models
infocriteria(garchfit)
infocriteria(gjrfit)

# Correlogram and Ljung-Box test
# Compute the standardized returns
stdEURUSDret <- residuals(tgarchfit, standardize = TRUE)

# Compute their sample mean and standard deviation
mean(stdEURUSDret)
sd(stdEURUSDret)

# Correlogram of the absolute (standardized) returns
par(mfrow = c(1, 2))
acf(abs(EURUSDret), 22)
acf(abs(stdEURUSDret), 22)

# Ljung-Box test
Box.test(abs(stdEURUSDret), 22, type = "Ljung-Box")

# Change estimation sample
# Estimate the model on the last 2500 observations
tgarchspec <- ugarchspec( mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = "std")
tgarchfit <- ugarchfit(data = tail(EURUSDret, n = 2500) , spec = tgarchspec)

# Compute standardized returns
stdEURUSDret <- residuals(tgarchfit, standardize = TRUE)

# Do the Ljung-Box test on the absolute standardized returns
Box.test(abs(stdEURUSDret), 22, type = "Ljung-Box")

# In-sample versus rolling sample vol
# Estimate the GARCH model using all the returns and compute the in-sample estimates of volatility
garchinsample <- ugarchfit(data = sp500ret, spec = garchspec)
garchvolinsample <- sigma(garchinsample)

# Use ugarchroll for rolling estimation of the GARCH model 
garchroll <- ugarchroll(garchspec, data = sp500ret , 
        n.start = 2000, refit.window = "moving",  refit.every = 2500)

# Set preds to the data frame with rolling predictions
preds <- as.data.frame(garchroll)

# Compare in-sample and rolling sample volatility in one plot
garchvolroll <- xts(preds$Sigma, order.by = as.Date(rownames(preds)))
volplot <- plot(garchvolinsample, col = "darkgrey", lwd = 1.5, main = "In-sample versus rolling vol forecasts")
volplot <- addSeries(garchvolroll, col = "blue", on = 1)
plot(volplot)

# Horse race
# Inspect the first three rows of the dataframe with out of sample predictions
garchpreds <- as.data.frame(garchroll)
head(garchpreds, 3)

# Compute prediction errors
e  <- garchpreds$Realized - garchpreds$Mu  
d  <- e^2 - garchpreds$Sigma^2 

# Compute MSE for the garchroll variance prediction
garchMSE <- mean(d^2)

# Compute MSE for gjrgarchroll
gjrgarchpreds <- as.data.frame(gjrgarchroll)
e  <- gjrgarchpreds$Realized - gjrgarchpreds$Mu
d  <- e^2 - gjrgarchpreds$Sigma^2
gjrgarchMSE <- mean(d^2)

## Chapter 4
# Comovement between predicted vol and VaR
# Extract the dataframe with predictions from the rolling GARCH estimation
garchpreds <- as.data.frame(garchroll)

# Extract the 5% VaR 
garchVaR <- quantile(garchroll, probs = 0.05)

# Extract the volatility from garchpreds
garchvol <- xts(garchpreds$Sigma, order.by = time(garchVaR))

# Analyze the comovement in a time series plot
garchplot <- plot(garchvol, ylim = c(-0.1, 0.1))
garchplot <- addSeries(garchVaR, on = 1, col = "blue")
plot(garchplot, main = "Daily vol and 5% VaR")

# Sensitivity of coverage to distribution model
# Take a default specification a with a normal and skewed student t distribution
normgarchspec <- ugarchspec(distribution.model = "norm")
sstdgarchspec <- ugarchspec(distribution.model = "sstd")

# Do rolling estimation
normgarchroll <- ugarchroll(normgarchspec, data = msftret, 
                            n.start = 2500, refit.window = "moving", refit.every = 2000)
sstdgarchroll <- ugarchroll(sstdgarchspec, data = msftret, 
                            n.start = 2500, refit.window = "moving", refit.every = 2000)

# Compute the 5% value at risk
normgarchVaR <- quantile(normgarchroll, probs = 0.05)
sstdgarchVaR <- quantile(sstdgarchroll, probs = 0.05)

# Compute the coverage
actual <- xts(as.data.frame(normgarchroll)$Realized,time(normgarchVaR))
mean(actual < normgarchVaR)
mean(actual < sstdgarchVaR)

# Use in production
# Estimate the model
garchfit <- ugarchfit(data = sp500ret["/2006-12"], spec = garchspec)

# Fix the parameters
progarchspec <- garchspec
setfixed(progarchspec) <- as.list(coef(garchfit))

# Use ugarchfilter to obtain the estimated volatility for the complete period
garchfilter <- ugarchfilter(data = sp500ret, spec = progarchspec)
plot(sigma(garchfilter))

# Compare the 252 days ahead forecasts made at the end of September 2008 and September 2017
garchforecast2008 <- ugarchforecast(data = sp500ret["/2008-09"], fitORspec = progarchspec, n.ahead = 252)
garchforecast2017 <- ugarchforecast(data = sp500ret["/2017-09"], fitORspec = progarchspec, n.ahead = 252)
par(mfrow = c(2,1), mar = c(3,2,3,2))
plot(sigma(garchforecast2008), main = "/2008-09", type = "l")
plot(sigma(garchforecast2017), main = "/2017-09", type = "l")

# Use in simulation
# Complete the code to simulate 4 time series of 10 years of daily returns
simgarch <- ugarchpath(spec=simgarchspec, m.sim = 4, n.sim = 10*252,  rseed = 210) 

# Plot the simulated returns of the four series
simret <- fitted(simgarch)
plot.zoo(simret)
plot.zoo(sigma(simgarch))

# Compute the corresponding simulated prices and plot them
simprices <- exp(apply(simret, 2, "cumsum"))
matplot(simprices, type = "l", lwd = 3)

# Starting values
# Estimate using default starting values
garchfit <- ugarchfit(data = EURUSDret, spec = garchspec)

# Print the estimated parameters and the likelihood
coef(garchfit)
likelihood(garchfit)

# Set other starting values and re-estimate
setstart(garchspec) <- list(alpha1 = 0.05, beta1 = 0.9, shape = 6) 
garchfit <- ugarchfit(data = EURUSDret, spec = garchspec)

# Print the estimated parameters and the likelihood
coef(garchfit)
likelihood(garchfit)

# Minimum variance portfolio weights
# Compute the standardized US and EU returns, together with their correlation 
stdusret <- residuals(usgarchfit, standardize = TRUE)
stdeuret <- residuals(eugarchfit, standardize = TRUE)

useucor <- as.numeric(cor(stdusret, stdeuret))
print(useucor)

  # Compute the covariance and variance of the US and EU returns 
useucov <- useucor * sigma(usgarchfit) * sigma(eugarchfit)
usvar <- sigma(usgarchfit)^2
euvar <- sigma(eugarchfit)^2

# Compute the minimum variance weight of the US ETF in the US-EU ETF portfolio 
usweight <- (euvar - useucov) / (usvar + euvar - 2 * useucov)
plot(usweight)

# GARCH & Co
# Compute standardized returns
stdmsftret <- residuals(msftgarchfit, standardize = TRUE)
stdwmtret <- residuals(wmtgarchfit, standardize = TRUE)

# Print the correlation
cor(stdmsftret, stdwmtret)

# Load the package PerformanceAnalytics
library(PerformanceAnalytics)

# Plot the 3-month rolling correlation
chart.RollingCorrelation(stdmsftret, stdwmtret, width = 66, main = "3-month rolling correlation between MSFT and WMT daily returns")
